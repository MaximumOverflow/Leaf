using Type = Leaf.Compilation.Types.Type;
using Leaf.Compilation.Reflection.Static;
using DotNetCoreUtilities.Extensions;
using System.Text.RegularExpressions;
using System.Collections.Concurrent;
using Leaf.Compilation.Exceptions;
using Leaf.Compilation.Statements;
using System.Collections.Generic;
using Leaf.Compilation.Functions;
using Leaf.Compilation.Grammar;
using Leaf.Compilation.Types;
using LLVMSharp.Interop;
using System.Linq;
using System;

namespace Leaf.Compilation.Values
{
	[Flags]
	public enum ValueRetrievalFlags : byte
	{
		None				= 0b00000000,
		GetByRef			= 0b00000001,
		GetTypeOnly			= 0b00000010,
		AllowRefAliasing	= 0b00000100,
	}
	
	public struct ValueRetrievalOptions
	{
		public Value? Parent;
		public Type? ExpectedType;
		public Value[]? Parameters;
		public ValueRetrievalFlags Flags;
	}
	
	public readonly partial struct Value
	{
		public static Value Get(LeafParser.ValueContext v, in LocalCompilationContext ctx, in ValueRetrievalOptions options = default)
			=> Get(v, in ctx, options, out _);
		
		public static Value Get(LeafParser.ValueContext v, in LocalCompilationContext ctx, in ValueRetrievalOptions options, out Value? parent)
		{
			parent = default;
			if (v.StaticAccessor() != null) 
				return TypeInfo.QueryValue(ctx.CurrentFragment.GetType(v.type(), ctx), v.Id().GetText(), v.value(), in ctx);

			var id = v.Id()?.GetText();
			if (id != null) return GetById(id, in ctx, out parent, in options);

			var i = v.integer();
			if (i != null) return GetInteger(i, in ctx, in options);

			var r = v.Ref();
			if (r != null)
			{
				return Get(v.value(0), in ctx, new ValueRetrievalOptions
				{
					Parent = options.Parent,
					Parameters = options.Parameters,
					ExpectedType = options.ExpectedType,
					Flags = options.Flags | ValueRetrievalFlags.GetByRef,
				}, out parent);
			}

			if (v.SizeOf() != null)
			{
				return (options.Flags & ValueRetrievalFlags.GetTypeOnly) != 0 
					? new Value {Type = ctx.GlobalContext.GlobalNamespace.Types["i64"]} 
					: Type.SizeOf(ctx.CurrentFragment.GetType(v.type()), ctx.GlobalContext);
			}

			var cstr = v.CString();
			if (cstr != null)
			{
				return (options.Flags & ValueRetrievalFlags.GetTypeOnly) != 0 
					? new Value {Type = PointerType.Create(ctx.GlobalContext.GlobalNamespace.Types["i8"])} 
					: GetCString(Regex.Unescape(cstr.GetText()[2..^1]), in ctx, in options);
			}

			var initList = v.initialization_list();
			if (initList != null)
			{
				return (options.Flags & ValueRetrievalFlags.GetTypeOnly) != 0 
					? new Value{Type = ctx.CurrentFragment.GetType(initList.type())}
					: GetInitList(initList, in ctx, in options);
			}

			if (v.nested != null && !options.Parent.HasValue)
			{
				var opt = options;
				opt.Parent = Get(v.value(0), in ctx, in options, out parent);
				return Get(v.value(1), in ctx, in opt, out parent);
			}

			if (v.par != null)
				return Get(v.value(0), in ctx, in options, out parent);

			if (v.call != null)
			{
				var c = ctx;
				var opt = options;
				var vals = v.value().Skip(1).Select(p => Get(p, c)).ToArray();
				opt.Parameters = vals;
				var fn = Get(v.call, in ctx, in opt, out parent);
				return (options.Flags & ValueRetrievalFlags.GetTypeOnly) != 0 
					? (fn.Flags & ValueFlags.Callable) != 0
						? new Value{Type = fn.ReturnType}
						: throw new CompilationException("Value is not callable.", c.CurrentFragment, v.Start.Line)
					: Statement.CompileCall(fn, vals, in c);
			}
			
			if (v.True() != null) return new Value
			{
				Flags = ValueFlags.Constant,
				Type = ctx.GlobalContext.GlobalNamespace.Types["bool"],
				LlvmValue = LLVMValueRef.CreateConstInt(LLVMTypeRef.Int1, 1),
			};
			
			if (v.False() != null) return new Value
			{
				Flags = ValueFlags.Constant,
				Type = ctx.GlobalContext.GlobalNamespace.Types["bool"],
				LlvmValue = LLVMValueRef.CreateConstInt(LLVMTypeRef.Int1, 0),
			};

			if (v.Eq() != null) return GetOp(v.value(0), v.value(1), Operator.Eq, in ctx, in options);
			if (v.Neq() != null) return GetOp(v.value(0), v.value(1), Operator.Neq, in ctx, in options);
			if (v.Add() != null) return GetOp(v.value(0), v.value(1), Operator.Add, in ctx, in options);
			if (v.Sub() != null) return GetOp(v.value(0), v.value(1), Operator.Sub, in ctx, in options);
			if (v.Mul() != null) return GetOp(v.value(0), v.value(1), Operator.Mul, in ctx, in options);
			if (v.Div() != null) return GetOp(v.value(0), v.value(1), Operator.Div, in ctx, in options);
			if (v.Mod() != null) return GetOp(v.value(0), v.value(1), Operator.Mod, in ctx, in options);
			if (v.As() != null) return Get(v.value(0), in ctx, in options).CastTo(ctx.CurrentFragment.GetType(v.type(), ctx), in ctx, true, v);

			throw new NotImplementedException();
		}

		private static Value GetById(string id, in LocalCompilationContext ctx, out Value? parentVal, in ValueRetrievalOptions options = default)
		{
			var builder = ctx.Builder;
			if (options.Parent.HasValue)
			{
				parentVal = options.Parent.Value.AsPlainLValue(in ctx);
				var parent = parentVal.Value!;
				switch (parent.Type)
				{
					case StructType structT:
					{
						if (!structT.Members.TryGetValue(id, out var member))
						{
							if ((options.Flags & ValueRetrievalFlags.GetByRef) != 0)
								throw new CompilationException("Methods cannot be referenced.", ctx.CurrentFragment);
							
							var param = options.Parameters;
							if(param == null) throw new MemberNotFoundException(id, structT, ctx.CurrentScope);

							if (!structT.Methods.TryGetValue(id, out var overloads))
								throw new MemberNotFoundException(id, structT, ctx.CurrentScope);

							var args = new Type[param.Length + 1];
							args[0] = (parent.Flags & ValueFlags.Mutable) != 0
								? ReferenceType.Create(parent.Type)
								: LightReferenceType.Create(parent.Type);
							
							for (var i = 1; i < args.Length; i++) args[i] = param[i - 1].Type;
							return overloads.GetImplementation(args);
						}

						if ((options.Flags & ValueRetrievalFlags.GetTypeOnly) != 0)
							return new Value {Type = member.Type};
						
						var variable = new Value
						{
							Type = member.Type,
							Flags = ValueFlags.LValue | (parent.Flags & ValueFlags.Mutable),
							LlvmValue = builder.BuildStructGEP(parent.LlvmValue, member.Index),
						};

						if ((options.Flags & ValueRetrievalFlags.GetByRef) != 0)
							return variable;

						return variable.AsRValue(in ctx);
					}
					
					default: throw new NotImplementedException($"Type {parent.Type.GetType()} is not supported");
				}
			}

			parentVal = default;
			var scope = ctx.CurrentScope;
			while (scope != null)
			{
				if (scope.Variables.TryGetValue(id, out var variable))
					return variable;
					
				if ((ctx.CurrentFunction.Flags & FunctionFlags.MemberFunc) != 0)
				{
					try
					{
						var opt = options;
						opt.Parent = scope.Variables["this"];
						var val = GetById(id, in ctx, out var parent, in opt);
						parentVal = parent;

						if ((options.Flags & ValueRetrievalFlags.GetByRef) != 0)
							return val;

						return val.AsRValue(in ctx);
					}
					catch (MemberNotFoundException) {}
				}

				if (options.Parameters != null && ctx.CurrentFragment.TryGetOverloadGroup(id, out var overloads))
				{
					if ((options.Flags & ValueRetrievalFlags.GetByRef) != 0)
						throw new CompilationException("Functions cannot be referenced.", ctx.CurrentFragment);
					
					var fn = overloads!.GetImplementation(options.Parameters);
					ctx.CurrentFragment.Module.EnsureLinkage(ref fn);

					return fn;
				}

				scope = scope.Parent;
			}

			throw new SymbolNotFoundException(id, ctx.CurrentScope);
		}

		private static Value GetInteger(LeafParser.IntegerContext i, in LocalCompilationContext ctx, in ValueRetrievalOptions options)
		{
			if (i.Integer() != null)
			{
				var v = i.Integer().GetText();
				var value = long.Parse(v);
				var type = value switch
				{
					> int.MaxValue => ctx.GlobalContext.GlobalNamespace.Types["i64"],
					> short.MaxValue => ctx.GlobalContext.GlobalNamespace.Types["i32"],
					> byte.MaxValue => ctx.GlobalContext.GlobalNamespace.Types["i16"],
					_ => ctx.GlobalContext.GlobalNamespace.Types["i8"],
				};

				return new Value
				{
					Type = type,
					Flags = ValueFlags.Constant,
					LlvmValue = (options.Flags & ValueRetrievalFlags.GetTypeOnly) == 0 
						? LLVMValueRef.CreateConstInt(type, unchecked((ulong) value), true)
						: default
				};
			}

			throw new NotImplementedException();
		}
		
		private static Value GetCString(string text, in LocalCompilationContext ctx, in ValueRetrievalOptions options)
		{
			var globalContext = ctx.GlobalContext;
			var module = ctx.CurrentFragment.Module;
			
			if ((options.Flags & ValueRetrievalFlags.GetTypeOnly) != 0) return new Value
				{ Type = PointerType.Create(globalContext.GlobalNamespace.Types["i8"]) };
			
			return module.GetConstCString(text, in ctx);
		}

		private static readonly ConcurrentBag<Dictionary<string, Value>> NameValueDictPool = new();
		private static Value GetInitList(LeafParser.Initialization_listContext initList, in LocalCompilationContext ctx, in ValueRetrievalOptions options)
		{
			var voidT = ctx.GlobalContext.GlobalNamespace.Types["void"];
			var type = ctx.CurrentFragment.GetType(initList.type());

			if (type == voidT)
				type = options.ExpectedType;

			if (type == null)
				throw new CompilerBugException();

			if (type is not StructType structT)
				throw new NotImplementedException("Only structs are currently supported.");

			var ids = initList.Id();
			var vals = initList.value();
			var members = NameValueDictPool.TakeOrCreate();
			members.EnsureCapacity(ids.Length);

			for (var i = 0; i < ids.Length; i++)
			{
				var name = ids[i].GetText();
				if (!structT.Members.TryGetValue(name, out var member))
					throw new MemberNotFoundException(name, structT, ctx.CurrentScope, ids[i].Symbol.Line);

				var opt = options; 
				opt.ExpectedType = member.Type;
				var val = Get(vals[i], in ctx, in opt);

				if (val.Type != member.Type)
					val = val.CastTo(member.Type, in ctx, false, vals[i]);

				if (!members.TryAdd(name, val))
					throw new CompilationException($"Duplicate assigment of member '{name}'.", ctx.CurrentFragment, ids[i].Symbol.Line);
			}

			if (members.Count != structT.Members.Count)
				throw new CompilationException("All members must be initialized.", ctx.CurrentFragment, initList.Start.Line);

			var @const = true;
			var builder = ctx.Builder;
			Span<LLVMValueRef> values = stackalloc LLVMValueRef[structT.Members.Count];

			foreach (var (name, member) in structT.Members)
			{
				var mem = members[name];
				if ((mem.Flags & ValueFlags.Constant) == 0)
					@const = false;
				
				values[(int) member.Index] = (mem.Flags & ValueFlags.LValue) != 0
					? builder.BuildLoad(mem.LlvmValue)
					: mem.LlvmValue;
			}

			Value value;
			if (@const) return ctx.CurrentFragment.Module.GetConstStruct(structT, values, in ctx);

			else //TODO You might want to implement it differently
			{
				var llvmValue = builder.BuildAlloca(type);
				
				for (var i = 0; i < values.Length; i++)
					builder.BuildStore(values[i], builder.BuildStructGEP(llvmValue, (uint) i));
				
				value = new Value
				{
					Type = type,
					LlvmValue = builder.BuildLoad(llvmValue)
				};
			}
			
			if (options.ExpectedType != null && type != options.ExpectedType && type != voidT)
				throw new NotImplementedException();

			members.Clear();
			NameValueDictPool.Add(members);
			return value;
		}

		public static Value CreateReference(Value value, in LocalCompilationContext ctx, bool light)
		{
			var builder = ctx.Builder;

			if (light) return value;
			
			if (value.Type is ReferenceType)
				return value;
				
			var type = ReferenceType.Create(value.Type);
			var llvmValue = builder.BuildAlloca(type);

			builder.BuildStore(value.LlvmValue, builder.BuildStructGEP(llvmValue, 0));
			
			builder.BuildStore(value.Allocator ?? LLVMValueRef.CreateConstPointerNull(
					LLVMTypeRef.CreatePointer(ctx.GlobalContext.AllocatorVTableType.LlvmType, 0)),
				builder.BuildStructGEP(llvmValue, 1));
				
			var reference = new Value
			{
				Type = type,
				Flags = value.Flags,
				Allocator = value.Allocator,
				LlvmValue = builder.BuildLoad(llvmValue)
			};
				
			return reference;
		}
	}
}