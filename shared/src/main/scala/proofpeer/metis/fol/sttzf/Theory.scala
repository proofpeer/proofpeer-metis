// package proofpeer.metis.fol.sttzf

// import org.ensime.sexp.{ SexpCompactPrinter => SexpPrinter, SexpSymbol, SexpParser }

// import scalaz._
// import Scalaz._

// import proofpeer.metis.fol.SExpr._
// import proofpeer.metis.SExpr._
// import ZFProver._
// import Interactive._

// object Theory {
//   lazy val Success(Some(empty)) =
//     interactive(
//       "(ex Empty (! x (not (vin x Empty))))",
//       List(("x","(not (= x x))")),
//       List()).map { _.thm >>= { define(_,SexpSymbol("Empty")) }}
//   lazy val Success(Some(upair_def)) =
//     interactive(
//       "(! a (! b (ex UPair (! x (<-> (vin x UPair) (or (= x a) (= x b)))))))",
//       List(),
//       List(ZFProver.upair)).map { _.thm >>= { define(_,SexpSymbol("UPair")) }}
//   lazy val Success(Some(eq_upair)) =
//     interactive(
//       "(<-> (= P ((UPair) a b)) (! x (<-> (vin x P) (or (= x a) (= x b)))))",
//       List(),
//       List(extensionality,upair_def)).map { _.thm }
//   lazy val Success(Some(vpair_def)) = interactive(
//     """(! a (! b (ex VPair (= VPair ((UPair) ((UPair) a a) ((UPair) a b))))))""",
//     List(),
//     List()).map { _.thm >>= { define(_,SexpSymbol("VPair")) }}
//   lazy val Success(Some(vpair_thm)) = interactive(
//     "(<-> (= ((VPair) a b) ((VPair) c d)) (and (= a c) (= b d)))",
//     List(),
//     List(vpair_def, extensionality,upair_def)).map { _.thm }
//   lazy val Success(Some(powerset_def)) = interactive(
//     "(! A (ex P (! X (<-> (vin X P) (! x (-> (vin x X) (vin x A)))))))",
//     List(),
//     List(ZFProver.powerset)).map { _.thm >>= { define(_,SexpSymbol("Power")) }}
//   lazy val Success(Some(union_def)) = interactive(
//     "(! A (! B (ex U (! x (<-> (vin x U) (or (vin x A) (vin x B)))))))",
//     List(),
//     List(fUnion)).map { _.thm >>= { define(_,SexpSymbol("Union")) }}
//   val Success(fn_body) =
//     folOfString("""
// (ex x (and (and (-> (vin ((f) x) B) (= P ((VPair) x ((f) x))))
//                 (-> (not (vin ((f) x) B)) (= P ((VPair) x ((Member) B)))))
//            (vin x A)))
// """)

//   val fn = {
//     val power =
//       termOfSExpr(SexpParser.parse("((Power) ((Power) ((Union) A B)))"))
//     val Some(fnInst) =
//       separationInst(
//         SexpSymbol("P"),
//         fn_body)
//     val Some(defn) =
//       define(
//         inst(fnInst, IMap.singleton(SexpSymbol("Super"), power)),
//         SexpSymbol("Fn"))
//     defn
//   }

//   lazy val Success(one) =
//     interactive("""
// (-> (! P
//        (<-> (vin P ((Fn)))
//             (and (vin P ((Power) ((Power) ((Union) A B))))
//                  (ex x (and (and (-> (vin ((f) x) B) (= P ((VPair) x ((f) x))))
//                                  (-> (not (vin ((f) x) B))
//                                      (= P ((VPair) x ((Member) B)))))
//                             (vin x A))))))
//     (-> (ex b (vin b B)) (! p (-> (vin p ((Fn)))
//                                   (ex x (ex y (and (= p ((VPair) x y))
//                                                    (and (vin x A) (vin y B)))))))))
// """, List(), List(fn, choice))

//   lazy val Success(Some(two)) =
//     interactive("""
// (! x (! y (-> (and (vin x A) (vin y B))
//               (vin ((VPair) x y) ((Power) ((Power) ((Union) A B)))))))
// """, List(), List(powerset_def,vpair_def,upair_def,eq_upair,union_def)).map(_.thm)

//   lazy val Success(three) =
//     interactive("""
// (-> (! P
//        (<-> (vin P ((Fn)))
//             (and (vin P ((Power) ((Power) ((Union) A B))))
//                  (ex x (and (and (-> (vin ((f) x) B) (= P ((VPair) x ((f) x))))
//                                  (-> (not (vin ((f) x) B))
//                                      (= P ((VPair) x ((Member) B)))))
//                             (vin x A))))))
//     (-> (ex b (vin b B))
//         (! x (-> (and (vin x A) (not (vin ((f) x) B)))
//                  (vin ((VPair) x ((Member) B)) ((Fn)))))))
// """, List(), List(two,choice))

//   lazy val Success(four_l) =
//     interactive("""
// (-> (! P
//        (<-> (vin P ((Fn)))
//             (and (vin P ((Power) ((Power) ((Union) A B))))
//                  (ex x (and (and (-> (vin ((f) x) B)
//                                      (= P ((VPair) x ((f) x))))
//                                  (-> (not (vin ((f) x) B))
//                                      (= P ((VPair) x ((Member) B)))))
//                             (vin x A))))))
//     (-> (and (vin x A) (vin ((f) x) B))
//         (and (! y (-> (and (vin y B) (vin ((VPair) x y) ((Fn))))
//                       (= ((f) x) y)))
//              (and (vin ((f) x) B) (vin ((VPair) x ((f) x)) ((Fn)))))))
// """, List(), List(two,vpair_thm,choice))


//   lazy val Success(four) =
//     interactive("""
// (-> (! P
//        (<-> (vin P ((Fn)))
//             (and (vin P ((Power) ((Power) ((Union) A B))))
//                  (ex x (and (and (-> (vin ((f) x) B) (= P ((VPair) x ((f) x))))
//                                  (-> (not (vin ((f) x) B))
//                                      (= P ((VPair) x ((Member) B)))))
//                             (vin x A))))))
//     (-> (ex b (vin b B))
//         (! x (-> (vin x A)
//                  (ex y (and (! z (-> (and (vin z B) (vin ((VPair) x z) ((Fn))))
//                                      (= y z)))
//                             (and (vin y B) (vin ((VPair) x y) ((Fn))))))))))
// """, List(), List(two,choice,vpair_thm))



//   lazy val Success(Some(fnspace_def)) = interactive("""
// (! A
//   (! B
//     (ex fspace
//       (! f (<->
//              (vin f fspace)
//              (and
//                (vin f Super)
//                (and
//                  (! x (-> (vin x A)
//                             (ex y (and
//                                     (and
//                                       (vin y B)
//                                       (vin ((VPair) x y) f))
//                                     (! z (-> (and (vin z B)
//                                                (vin ((VPair) x z) f))
//                                              (= y z)))))))
//                  (! p (-> (vin p f)
//                           (ex x (ex y (and
//                                         (and
//                                           (= p ((VPair) x y))
//                                           (vin x A))
//                                       (vin y B)))))))))))))
//     """,
//     List(("f", """
// (and
//   (! x (-> (vin x A)
//            (ex y (and
//                   (and
//                    (vin y B)
//                    (vin ((VPair) x y) f))
//                  (! z (-> (and (vin z B)
//                                (vin ((VPair) x z) f))
//                           (= y z)))))))
//   (! p (-> (vin p f) (ex x (ex y (and
//                                   (and
//                                    (= p ((VPair) x y))
//                                    (vin x A))
//                                   (vin y B))))))))
// """)),
//     List()).map { _.thm >>= { define(_,) }}
//   lazy val Success(Some(foo)) = interactive("""
// (! A (! B (ex fspace (! f (<->
//                            (vin f fspace)
//                            (and
//                             (vin f ((Power) ((Power) ((Power) ((Union) A B)))))
//                             (! y (vin y f))))))))
//     """,
//     List(("f","(! y (vin y f))")),
//     List()).map { _.thm >>= { define(_) }}

//   lazy val Success(fnspace_def2) = interactive("""
// (! A
//   (! B
//     (ex fspace
//       (! f (<->
//              (vin f fspace)
//              (and
//                (vin f ((Power) ((Power) ((Power) ((Union) A B)))))
//                (! x (-> (vin x A)
//                         (ex y (and (vin y B) (vin ((VPair) x y) f)))))))))))
//     """,
// """
//     (ex Sub
//       (! f (<->
//              (vin f Sub)
//              (and
//                (vin f Super)
//                (! x (-> (vin x A)
//                         (ex y
//                           (and
//                             (vin y B)
//                             (vin (VPair x y) f)))))))))
// """
//     List(("f", """
// (! x (-> (vin x A)
//          (ex y (and (vin y B) (vin ((VPair) x y) f)))))
// """)),
//     List()) //.map { _.thm >>= { define(_) }}
//}
