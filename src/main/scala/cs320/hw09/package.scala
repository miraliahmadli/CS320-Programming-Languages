package cs320

package object hw09 extends Homework09 {
import scala.annotation.meta.field

  def mustSame(left: Type, right: Type, tyEnv: TyEnv): Type =
      if (same(left, right, tyEnv)) left
      else notype(s"$left is not equal to $right")
      
  def same(left: Type, right: Type, tyEnv: TyEnv): Boolean =
    (left, right) match {
        case (NumT, NumT) => true
        case (BoolT, BoolT) => true
        case (IdT(x), IdT(y)) =>  tyEnv.varMap.get(x) match{//(x==y)
            case None => sameTbind(tyEnv.tbinds.getOrElse(x, notype(s"${x} is not here")),tyEnv.tbinds.getOrElse(y, notype(s"${y} is not here")), tyEnv)
            case Some(value) => same(value, tyEnv.varMap.getOrElse(y, notype(s"${y} is not here")), tyEnv)
        }
        case (ArrowT(p1, r1), ArrowT(p2, r2)) => sameArrow(p1, p2, tyEnv) && same(r1, r2, tyEnv)
        case _ => false
      }

  def sameTbind(tbind1: Map[String, List[Type]], tbind2: Map[String, List[Type]], tyEnv: TyEnv): Boolean =  
      tbind1.foldLeft(true){
        case (acc, (name, types1)) => 
            val types2 = tbind2.getOrElse(name, notype(s"${name} is not here"))
            types1.equals(types2)
      }

  def validArray(pr: List[Type], tyEnv: TyEnv): Type = pr match {
    case Nil => NumT
    case head :: tl => 
      validType(head, tyEnv)
      validArray(tl, tyEnv) 
  }

  def validType(ty: Type, tyEnv: TyEnv): Type = ty match {
    case NumT => ty
    case BoolT => ty
    case ArrowT(p, r) =>
        validArray(p, tyEnv)
        validType(r, tyEnv)
        ArrowT(p, r)
    case IdT(x) =>if (tyEnv.tbinds.contains(x)) ty else notype(s"$x is a free type")
    case _ => notype("this type does not exist")
  }

  def sameArrow(p1: List[Type], p2: List[Type], tyEnv: TyEnv): Boolean=
      (p1, p2) match {
        case (Nil, Nil) => true
        case (Nil, _) => error("wrong arity")
        case (_, Nil) => error("wrong arity")
        case (l :: ltail, r :: rtail) => same(l,r, tyEnv) && sameArrow(ltail, rtail, tyEnv)
      }

  def notype(msg: Any): Nothing = error(s"no type: $msg")

  def addMap(params: List[(String, Type)], tyEnv: TyEnv, types: List[Type]): (TyEnv, List[Type]) = 
      params match{
        case Nil => (tyEnv, types)
        case (head, htype) :: tl => addMap(tl, tyEnv.addVar(head, htype, false), htype :: types)
      }

  def checkApp(types: List[Type], args: List[Expr], tyEnv: TyEnv): Boolean =
      (types, args) match {
        case (Nil, Nil) => true
        case (Nil, _) => error("wrong arity")
        case (_, Nil) => error("wrong arity")
        case (ty :: tl, head :: tail) => 
            val typ = typeCheck(head, tyEnv)
            same(ty, typ, tyEnv) && checkApp(tl, tail, tyEnv)
      }

  def blockStmts(stmts: List[Stmt], tyEnv: TyEnv): TyEnv =
      stmts match {
        case Nil => tyEnv
        case head :: tl => blockStmts(tl, typeCheck(tyEnv, head))
      }

  def matchStringsToTypes(fields: List[String], types: List[Type], tyEnv: TyEnv): TyEnv =
      (fields, types) match {
        case (Nil, Nil) => tyEnv
        case (Nil, _) => error("wrong arity")
        case (_, Nil) => error("wrong arity")
        case (field :: tl, ty :: tail) => matchStringsToTypes(tl, tail, tyEnv.addVar(field, ty, false))
      }

  def typeCheck(e: Expr, tyEnv: TyEnv): Type = 
    e match{
      case Id(name) => 
          tyEnv.varMap.getOrElse(name,notype(s"$name is a free identifier"))
      case Num(num) => NumT
      case Bool(bool) => BoolT

      case Add(left, right) => 
          mustSame(typeCheck(left, tyEnv), NumT, tyEnv)
          mustSame(typeCheck(right, tyEnv), NumT, tyEnv)
          NumT
      case Sub(left, right) => 
          mustSame(typeCheck(left, tyEnv), NumT, tyEnv)
          mustSame(typeCheck(right, tyEnv), NumT, tyEnv)
          NumT

      case Eq(left, right) => 
          mustSame(typeCheck(left, tyEnv), NumT, tyEnv)
          mustSame(typeCheck(right, tyEnv), NumT, tyEnv)
          BoolT
      case Lt(left, right) => 
          mustSame(typeCheck(left, tyEnv), NumT, tyEnv)
          mustSame(typeCheck(right, tyEnv), NumT, tyEnv)
          BoolT

      case Fun(params, body) => 
          validArray(getTypeList(params, List()), tyEnv)
          val (env, types) = addMap(params, tyEnv, List())
          ArrowT(types.reverse, typeCheck(body, env))

      case App(func, args) => 
          val types = typeCheck(func, tyEnv) 
          types match {
            case ArrowT(types, result) => 
              val check = checkApp(types, args, tyEnv)
              if(check){result}
              else{notype(s"apply args to types")}
            case _ => notype(s"apply args to types")
          }
      case Assign(name, expr) => 
          val type1 = tyEnv.varMap.getOrElse(name, notype("name does not exist"))
          if(tyEnv.mutables.contains(name)) mustSame(typeCheck(Id(name), tyEnv), typeCheck(expr, tyEnv), tyEnv)
          else notype("name does not exist")

      case Block(stmts, expr) => typeCheck(expr, blockStmts(stmts, tyEnv))

      case IfThenElse(cond, thenE, elseE) => 
          mustSame(typeCheck(cond, tyEnv), BoolT, tyEnv)
          mustSame(typeCheck(thenE, tyEnv), typeCheck(elseE, tyEnv), tyEnv)
          
      case Match(expr, cases) => typeCheck(expr, tyEnv) match {
        case IdT(t) => 
            val tbind = tyEnv.tbinds.getOrElse(t, notype("it does not exist in type Env")) 
            val lst : List[Type] = List()
            (cases.foldLeft(lst){
              case (acc, (name, (fields, e))) => 
                  val expType = typeCheck(e, matchStringsToTypes(fields, tbind.getOrElse(name, notype("such name does not exist")), tyEnv))
                  lst match {
                    case Nil =>  expType :: List()
                    case type1 :: tl => 
                        mustSame(type1, expType, tyEnv) :: List()
                  }
            }).head
        case _ => notype("it should be tbind")
      }
    }
  
  def getTypeList(params: List[(String, Type)], ans:  List[Type]) : List[Type]=
      params match{
        case Nil => ans
        case ((_, ty) :: tl) => getTypeList(tl, ty :: ans)
      }
  
  def getStrList(params: List[(String, Type)], ans:  List[String]) : List[String]=
      params match{
        case Nil => ans
        case ((str, _) :: tl) => getStrList(tl, str :: ans)
      }

  def addToNewEnv(params: List[(String, Type)], newEnv: TyEnv): TyEnv =
  params match{
    case Nil => newEnv
    case (x, ty) :: tl => addToNewEnv(tl, newEnv.addVar(x, ty, false))
  }

  def typeCheck(tyEnv: TyEnv, stmt: Stmt): TyEnv = 
      stmt match {
        case Val(isLazy, name, ty, expr) => 
            validType(ty, tyEnv)
            if(isLazy){
              val check = mustSame(ty, typeCheck(expr, tyEnv), tyEnv)
              val env = tyEnv.addVar(name, ty, false)
              env
            }
            else {
              val check = mustSame(ty, typeCheck(expr, tyEnv), tyEnv)
              tyEnv.addVar(name, ty, false)
            }
        case Var(name, ty, expr) => 
            validType(ty, tyEnv)
            var check = mustSame(ty, typeCheck(expr, tyEnv), tyEnv)
            tyEnv.addVar(name, ty, true)
        case Def(name, params, retTy, body) => 
            validType(retTy, tyEnv)
            val ans = getTypeList(params, List()).reverse
            validArray(ans, tyEnv)
            val newEnv = tyEnv.addVar(name, ArrowT(ans, retTy), false)
            val check = mustSame(retTy, typeCheck(body, addToNewEnv(params, newEnv)), newEnv)
            newEnv
        case Trait(name, cases) => 
            val newTyEnv = tyEnv.addTBind(name, cases)
            val newTyEnv2 = cases.foldLeft(newTyEnv){
                case (acc, (field, types)) => 
                    acc.addVar(field, ArrowT(types, IdT(name)), false)
            }
            cases.foldLeft(newTyEnv2){
              case (acc, (field, types)) => 
                  validArray(types, acc)
                  acc
            }
            newTyEnv2
      }
  
  def numOpV(op: (Int, Int) => Int): (Value, Value) => Value = (_, _) match {
    case (NumV(x), NumV(y)) => NumV(op(x, y))
    case (x, y) => error(s"not both numbers: $x, $y")
  }
  val numAddV = numOpV(_ + _)
  val numSubV = numOpV(_ - _)

  def numEqV(fst: Value, snd: Value) : Value =(fst, snd) match {
    case (NumV(x), NumV(y)) => BoolV(x==y)
    case (x, y) => error(s"not both numbers: $x, $y")
  }
  def numLtV(fst: Value, snd: Value) : Value =(fst, snd) match {
    case (NumV(x), NumV(y)) => BoolV(x<y)
    case (x, y) => error(s"not both numbers: $x, $y")
  }

  def storeLookup(addr: Addr, sto: Sto): Value  = sto.getOrElse(addr, error("no such field"))
  def lookup(name: String, env: Env): Value = env.getOrElse(name, error(s"free identifier: $name"))

  def malloc(sto: Sto): Addr =  
      sto.foldLeft(0) {
        case (max, (addr, _)) => math.max(max, addr)
      } + 1

  def interpAppCloV(args: List[Expr], params: List[String], env: Env, fenv: Env, sto: Sto): (Env, Sto) =
      (args, params) match {
        case (Nil, Nil) => (fenv, sto)
        case (Nil, _) => error("wrong arity")
        case (_, Nil) => error("wrong arity")
        case (expr :: tl, field :: tail) => 
            val (value, st) = interp(expr, env, sto)
            interpAppCloV(tl, tail, env, fenv + (field -> value), st)
      }
  
  def interpAppConstV(args: List[Expr], env: Env, sto: Sto, vals: List[Value]): (List[Value], Sto) =
      args match {
        case Nil => (vals, sto)
        case expr :: tl => 
            val (value, st) = interp(expr, env, sto)
            interpAppConstV(tl, env, st, value :: vals)
      }

  def interpStmt(stmts: List[Stmt], env: Env, sto: Sto): (Env, Sto) =
      stmts match {
        case Nil => (env, sto)
        case stmt :: tl => 
            val (newEnv, newSto) = interp((env, sto), stmt)
            interpStmt(tl, newEnv, newSto)
      }

  def matchFields(fields: List[String], vals: List[Value], env: Env) : Env =
      (fields, vals) match {
        case (Nil, Nil) => env
        case (Nil, _) => error("wrong arity")
        case (_, Nil) => error("wrong arity")
        case (field :: tl, value :: tail) => 
            matchFields(tl, tail, env + (field -> value))
      }

  def interp(e: Expr, env: Env, sto: Sto): (Value, Sto) = 
      e match {
        // case expr => println(expr); (NumV(1),sto)
        case Id(name) => lookup(name, env) match {
          case AddrV(addr) => storeLookup(addr, sto) match {
            case ExprV(expr, nenv) => 
                val (value, st) = interp(expr, nenv, sto)
                (value, st + (addr -> value))
            case value => (value, sto) 
          }
          case value => (value, sto)
        } 
        case Num(num) => (NumV(num), sto)
        case Add(left, right) => 
            val (lv, ls) = interp(left, env, sto)
            val (rv, rs) = interp(right, env, ls)
            (numAddV(lv, rv), rs)
        case Sub(left, right) => 
            val (lv, ls) = interp(left, env, sto)
            val (rv, rs) = interp(right, env, ls)
            (numSubV(lv, rv), rs)

        case Bool(bool) => (BoolV(bool), sto) 
        case Eq(left, right) => 
            val (lv, ls) = interp(left, env, sto)
            val (rv, rs) = interp(right, env, ls)
            (numEqV(lv, rv), rs) 
        case Lt(left, right) => 
            val (lv, ls) = interp(left, env, sto)
            val (rv, rs) = interp(right, env, ls)
            (numLtV(lv, rv), rs)  

        case Fun(params, body) => (CloV(getStrList(params, List()).reverse, body, env), sto) 
        case App(func, args) => 
            interp(func, env, sto) match{
                case (CloV(params, body, fenv), st) =>
                    val (newFenv, newSto) = interpAppCloV(args, params, env, fenv, st)
                    interp(body, newFenv, newSto)
                case (ConstructorV(field), st) =>  
                    val (vals, newSto) = interpAppConstV(args, env, st, List())
                    (VariantV(field, vals.reverse), newSto)
                case _ => error("wrong")
            }       

        case Block(stmts, expr) => 
            val (newEnv, newSto) = interpStmt(stmts, env, sto)
            interp(expr, newEnv, newSto)

        case Assign(name, expr) => env.get(name) match {
          case Some(AddrV(addr)) => 
              val (value, st) = interp(expr, env, sto)
              (value, st + (addr -> value))
          case _ => error("not an address")
        } 

        case IfThenElse(cond, thenE, elseE) => interp(cond, env, sto) match{
          case (BoolV(bool), st) =>
              if(bool) interp(thenE, env, st)
              else interp(elseE, env, st)
          case _ => notype("should be bool")
        } 

        case Match(expr, cases) => interp(expr, env, sto) match {
          case (VariantV(field, vals), st) => cases.get(field) match {
            case None => error("field is not in pattern matching")
            case Some((fields, expr)) => interp(expr, matchFields(fields, vals, env), st)
          }
          case _ => error("not pattern matching")
        } 
      }
  
  def interp(pair: (Env, Sto), stmt: Stmt): (Env, Sto) = {
      val (env, sto) = pair
      stmt match {
        case Val(isLazy, name, ty, expr) => 
            if(isLazy){
              val addr = malloc(sto)
              (env + (name -> AddrV(addr)) , sto + (addr ->ExprV(expr, env)))
            }
            else {
              val (v, st) = interp(expr, env, sto)
              (env + (name -> v) ,st)
            }
        case Var(name, ty, expr) => 
              val (value, st) = interp(expr, env, sto)
              val addr = malloc(st)
              (env + (name -> AddrV(addr)), st + (addr -> value))
        case Def(name, params, retTy, body) =>  
            val cloV = CloV(getStrList(params, List()).reverse, body, env)
            cloV.env = cloV.env + (name -> cloV)
            (cloV.env, sto)
        case Trait(name, cases) => 
            ((cases.foldLeft(env){
              case (acc, (field, _)) => acc + (field -> ConstructorV(field))
            }), sto)
      }
    }

  def tests: Unit = {
    test(run("""
      var x: Int = 1
      val y: Int = (x = 3)
      x + y
    """), "6")

    test(run("""
      var x: Int = 1
      lazy val y: Int = (x = 3)
      x + y + x
    """), "7")
    test(run("""
    var x: Int = 1
    lazy val y: Int = (x = 3)
    x + y
    """), "4")
    test(run("""
      var x: Int = 0
      lazy val y: Int = (x = x + 1)
      val z: Int = y + y + y + y
      z"""), "4")
    testExc(run("""val x: Int = 42; x = 24"""), "")
    test(run("""
      trait AE
      case class Num(Int)
      case class Add(AE, AE)
      case class Sub(AE, AE)

      def interp(e: AE): Int = e match {
        case Num(n) => n
        case Add(l, r) => interp(l) + interp(r)
        case Sub(l, r) => interp(l) - interp(r)
      }

      interp(Add(Num(2), Sub(Num(3), Num(1))))
    """), "4")
    test(run("""
      trait Tree
      case class Leaf(Int)
      case class Node(Tree, Tree)

      def max(l: Int, r: Int): Int =
        if (l < r) r else l

      def depth(e: Tree): Int = e match {
        case Leaf(n) => 1
        case Node(l, r) => max(depth(l), depth(r)) + 1
      }

      depth(Node(Node(Leaf(1), Node(Leaf(2), Leaf(3))), Leaf(4)))
    """), "4")

    // My test cases
    test(run("""
      var x: Int = 0
      lazy val y: Int = (x = x + 1)
      val z: Int = y + y + y + y
      z + x + y"""), "6")
    test(run("""
      trait AE
      case class Num(Int)
      case class Add(AE, AE)
      case class Sub(AE, AE)

      def interp(e: AE): Int = e match {
        case Num(n) => n
        case Add(l, r) => interp(l) + interp(r)
        case Sub(l, r) => interp(l) - interp(r)
      }

      interp(Sub(Num(10), Add(Num(3), Num(2))))
      """), "5")
    test(run("""
      val x: Int = 1
      val y: Int = 5
      x + y
      """), "6")

    test(run("""
      var x: Int = 0
      lazy val y: Int = (x = x + 1)
      lazy val d: Int = (x = x + 100)
      val z: Int = d + y + d + y
      z + y + x + d"""), "704")
      
    test(run("""
      trait Tree
      case class Leaf(Int)
      case class Node(Tree, Tree)

      def sumOfLeaves(e: Tree): Int = e match {
        case Leaf(n) => n
        case Node(l, r) =>  sumOfLeaves(l) + sumOfLeaves(r)
      }

      sumOfLeaves(Node(Node(Leaf(1), Node(Leaf(2), Leaf(3))), Leaf(4)))
    """), "10")

    test(run("""
      trait AE
      case class Num(Int)
      case class Add(AE, AE)
      case class Sub(AE, AE)
      case class Thr(AE)

      def interp(e: AE): Int = e match {
        case Num(n) => n
        case Add(l, r) => interp(l) + interp(r)
        case Sub(l, r) => interp(l) - interp(r)
      }

      interp(Add(Num(2), Sub(Num(3), Num(1))))
    """), "4")

    testExc(run("""
    trait AE
    case class Num(Int)
    case class Add(AE, AE)
    case class Sub(AE, AE)
    case class Thr(AE)

    def interp(e: AE): Bool = e match {
      case Num(n) => n
      case Add(l, r) => interp(l) + interp(r)
      case Sub(l, r) => interp(l) - interp(r)
    }

    interp(Add(Num(2), Sub(Num(3), Num(1))))
  """), "")
  }
}
