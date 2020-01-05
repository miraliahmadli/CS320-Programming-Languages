// case Match(expr, cases) =>typeCheck(expr, tyEnv) match {
//     case IdT(t) =>
//         if (tyEnv.tbinds.contains(t)) {
//             val emptyList : List[Type] = List()
//             val dummyList = DummyT :: emptyList
//             val finalType =
//             cases.foldLeft(dummyList)((acc, cur) => {
//                 acc.head match {
//                 case DummyT =>
//                     val name = cur._1
//                     val fields = cur._2._1
//                     val expression = cur._2._2
//                     val stringToTypes = tyEnv.tbinds(t)
//                     if (stringToTypes.contains(name)) {
//                         val types = stringToTypes(name)
//                         val tuples = fields zip types
//                         val newEnv = addMultipleParam(tuples, tyEnv)
//                         val exprType = typeCheck(expression, newEnv)
//                         exprType :: acc
//                     }
//                     else notype("No such case")
//                 case v =>
//                     val name = cur._1
//                     val fields = cur._2._1
//                     val expression = cur._2._2
//                     val stringToTypes = tyEnv.tbinds(t)
//                     if (stringToTypes.contains(name)) {
//                         val types = stringToTypes(name)
//                         val tuples = fields zip types
//                         val newEnv = addMultipleParam(tuples, tyEnv)
//                         val exprType = typeCheck(expression, newEnv)
//                         if (same(exprType, v)) exprType :: acc
//                         else notype("not same")
//                     }
//                     else notype("No such case")
//                 }
//             })
//             finalType.head
//         }
//         else notype("no such binding")
//     case _ => notype("false binding")        
// }