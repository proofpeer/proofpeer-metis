package proofpeer.metis.testing.tptp.Axioms

import proofpeer.metis.SimpleParse
import proofpeer.metis.testing.tptp._

object SET003_MINUS0 {
  val mainClauses =
    List(
      // Name: a2
      // Role: axiom
      List("~Member(x0, x1)","Little_set(x0)"),
      // Name: extensionality1
      // Role: axiom
      List("(x0 = x1)","Little_set(F1(x0, x1))"),
      // Name: extensionality2
      // Role: axiom
      List("(x0 = x1)","Member(F1(x0, x1), x0)","Member(F1(x0, x1), x1)"),
      // Name: extensionality3
      // Role: axiom
      List("~Member(F1(x0, x1), x0)","~Member(F1(x0, x1), x1)","(x0 = x1)"),
      // Name: non_ordered_pair1
      // Role: axiom
      List("~Member(x0, Non_ordered_pair(x1, x2))","(x0 = x1)","(x0 = x2)"),
      // Name: non_ordered_pair2
      // Role: axiom
      List("~(x0 = x1)","~Little_set(x0)","Member(x0, Non_ordered_pair(x1, x2))"),
      // Name: non_ordered_pair3
      // Role: axiom
      List("~(x0 = x2)","~Little_set(x0)","Member(x0, Non_ordered_pair(x1, x2))"),
      // Name: non_ordered_pair4
      // Role: axiom
      List("Little_set(Non_ordered_pair(x0, x1))"),
      // Name: singleton_set
      // Role: axiom
      List("(Singleton_set(x0) = Non_ordered_pair(x0, x0))"),
      // Name: ordered_pair
      // Role: axiom
      List("(Ordered_pair(x0, x1) = Non_ordered_pair(Singleton_set(x0), Non_ordered_pair(x0, x1)))"),
      // Name: ordered_pair_predicate1
      // Role: axiom
      List("~Ordered_pair_predicate(x0)","Little_set(F2(x0))"),
      // Name: ordered_pair_predicate2
      // Role: axiom
      List("~Ordered_pair_predicate(x0)","Little_set(F3(x0))"),
      // Name: ordered_pair_predicate3
      // Role: axiom
      List("~Ordered_pair_predicate(x0)","(x0 = Ordered_pair(F2(x0), F3(x0)))"),
      // Name: ordered_pair_predicate4
      // Role: axiom
      List("~(x0 = Ordered_pair(x1, x2))","~Little_set(x1)","~Little_set(x2)","Ordered_pair_predicate(x0)"),
      // Name: first1
      // Role: axiom
      List("~Member(x1, First(x0))","Little_set(F4(x1, x0))"),
      // Name: first2
      // Role: axiom
      List("~Member(x1, First(x0))","Little_set(F5(x1, x0))"),
      // Name: first3
      // Role: axiom
      List("~Member(x1, First(x0))","(x0 = Ordered_pair(F4(x1, x0), F5(x1, x0)))"),
      // Name: first4
      // Role: axiom
      List("~Member(x1, First(x0))","Member(x1, F4(x1, x0))"),
      // Name: first5
      // Role: axiom
      List("~(x2 = Ordered_pair(x0, x1))","~Little_set(x0)","~Little_set(x1)","~Member(x3, x0)","Member(x3, First(x2))"),
      // Name: second1
      // Role: axiom
      List("~Member(x1, Second(x0))","Little_set(F6(x1, x0))"),
      // Name: second2
      // Role: axiom
      List("~Member(x1, Second(x0))","Little_set(F7(x1, x0))"),
      // Name: second3
      // Role: axiom
      List("~Member(x1, Second(x0))","(x0 = Ordered_pair(F6(x1, x0), F7(x1, x0)))"),
      // Name: second4
      // Role: axiom
      List("~Member(x1, Second(x0))","Member(x1, F7(x1, x0))"),
      // Name: second5
      // Role: axiom
      List("~(x2 = Ordered_pair(x0, x1))","~Little_set(x0)","~Little_set(x1)","~Member(x3, x1)","Member(x3, Second(x2))"),
      // Name: element_relation1
      // Role: axiom
      List("~Member(x0, Estin)","Ordered_pair_predicate(x0)"),
      // Name: element_relation2
      // Role: axiom
      List("~Member(x0, Estin)","Member(First(x0), Second(x0))"),
      // Name: element_relation3
      // Role: axiom
      List("~Little_set(x0)","~Member(First(x0), Second(x0))","~Ordered_pair_predicate(x0)","Member(x0, Estin)"),
      // Name: intersection1
      // Role: axiom
      List("~Member(x2, Intersection(x0, x1))","Member(x2, x0)"),
      // Name: intersection2
      // Role: axiom
      List("~Member(x2, Intersection(x0, x1))","Member(x2, x1)"),
      // Name: intersection3
      // Role: axiom
      List("~Member(x2, x0)","~Member(x2, x1)","Member(x2, Intersection(x0, x1))"),
      // Name: complement1
      // Role: axiom
      List("~Member(x1, x0)","~Member(x1, Complement(x0))"),
      // Name: complement2
      // Role: axiom
      List("~Little_set(x1)","Member(x1, x0)","Member(x1, Complement(x0))"),
      // Name: union
      // Role: axiom
      List("(Union(x0, x1) = Complement(Intersection(Complement(x0), Complement(x1))))"),
      // Name: domain1
      // Role: axiom
      List("~Member(x1, Domain_of(x0))","Ordered_pair_predicate(F8(x1, x0))"),
      // Name: domain2
      // Role: axiom
      List("~Member(x1, Domain_of(x0))","Member(F8(x1, x0), x0)"),
      // Name: domain3
      // Role: axiom
      List("~Member(x1, Domain_of(x0))","(x1 = First(F8(x1, x0)))"),
      // Name: domain4
      // Role: axiom
      List("~(x2 = First(x1))","~Little_set(x2)","~Member(x1, x0)","~Ordered_pair_predicate(x1)","Member(x2, Domain_of(x0))"),
      // Name: cross_product1
      // Role: axiom
      List("~Member(x2, Cross_product(x0, x1))","Ordered_pair_predicate(x2)"),
      // Name: cross_product2
      // Role: axiom
      List("~Member(x2, Cross_product(x0, x1))","Member(First(x2), x0)"),
      // Name: cross_product3
      // Role: axiom
      List("~Member(x2, Cross_product(x0, x1))","Member(Second(x2), x1)"),
      // Name: cross_product4
      // Role: axiom
      List("~Little_set(x2)","~Member(First(x2), x0)","~Member(Second(x2), x1)","~Ordered_pair_predicate(x2)","Member(x2, Cross_product(x0, x1))"),
      // Name: converse1
      // Role: axiom
      List("~Member(x1, Converse(x0))","Ordered_pair_predicate(x1)"),
      // Name: converse2
      // Role: axiom
      List("~Member(x1, Converse(x0))","Member(Ordered_pair(Second(x1), First(x1)), x0)"),
      // Name: converse3
      // Role: axiom
      List("~Little_set(x1)","~Member(Ordered_pair(Second(x1), First(x1)), x0)","~Ordered_pair_predicate(x1)","Member(x1, Converse(x0))"),
      // Name: rotate_right1
      // Role: axiom
      List("~Member(x1, Rotate_right(x0))","Little_set(F9(x1, x0))"),
      // Name: rotate_right2
      // Role: axiom
      List("~Member(x1, Rotate_right(x0))","Little_set(F10(x1, x0))"),
      // Name: rotate_right3
      // Role: axiom
      List("~Member(x1, Rotate_right(x0))","Little_set(F11(x1, x0))"),
      // Name: rotate_right4
      // Role: axiom
      List("~Member(x1, Rotate_right(x0))","(x1 = Ordered_pair(F9(x1, x0), Ordered_pair(F10(x1, x0), F11(x1, x0))))"),
      // Name: rotate_right5
      // Role: axiom
      List("~Member(x1, Rotate_right(x0))","Member(Ordered_pair(F10(x1, x0), Ordered_pair(F11(x1, x0), F9(x1, x0))), x0)"),
      // Name: rotate_right6
      // Role: axiom
      List("~(x4 = Ordered_pair(x0, Ordered_pair(x1, x2)))","~Little_set(x0)","~Little_set(x1)","~Little_set(x2)","~Little_set(x4)","~Member(Ordered_pair(x1, Ordered_pair(x2, x0)), x3)","Member(x4, Rotate_right(x3))"),
      // Name: flip_range1
      // Role: axiom
      List("~Member(x1, Flip_range_of(x0))","Little_set(F12(x1, x0))"),
      // Name: flip_range2
      // Role: axiom
      List("~Member(x1, Flip_range_of(x0))","Little_set(F13(x1, x0))"),
      // Name: flip_range3
      // Role: axiom
      List("~Member(x1, Flip_range_of(x0))","Little_set(F14(x1, x0))"),
      // Name: flip_range4
      // Role: axiom
      List("~Member(x1, Flip_range_of(x0))","(x1 = Ordered_pair(F12(x1, x0), Ordered_pair(F13(x1, x0), F14(x1, x0))))"),
      // Name: flip_range5
      // Role: axiom
      List("~Member(x1, Flip_range_of(x0))","Member(Ordered_pair(F12(x1, x0), Ordered_pair(F14(x1, x0), F13(x1, x0))), x0)"),
      // Name: flip_range6
      // Role: axiom
      List("~(x4 = Ordered_pair(x0, Ordered_pair(x1, x2)))","~Little_set(x0)","~Little_set(x1)","~Little_set(x2)","~Little_set(x4)","~Member(Ordered_pair(x0, Ordered_pair(x2, x1)), x3)","Member(x4, Flip_range_of(x3))"),
      // Name: successor
      // Role: axiom
      List("(Successor(x0) = Union(x0, Singleton_set(x0)))"),
      // Name: empty_set
      // Role: axiom
      List("~Member(x0, Empty_set)"),
      // Name: universal_set
      // Role: axiom
      List("~Little_set(x0)","Member(x0, Universal_set)"),
      // Name: infinity1
      // Role: axiom
      List("Little_set(Infinity)"),
      // Name: infinity2
      // Role: axiom
      List("Member(Empty_set, Infinity)"),
      // Name: infinity3
      // Role: axiom
      List("~Member(x0, Infinity)","Member(Successor(x0), Infinity)"),
      // Name: sigma1
      // Role: axiom
      List("~Member(x1, Sigma(x0))","Member(F16(x1, x0), x0)"),
      // Name: sigma2
      // Role: axiom
      List("~Member(x1, Sigma(x0))","Member(x1, F16(x1, x0))"),
      // Name: sigma3
      // Role: axiom
      List("~Member(x1, x0)","~Member(x2, x1)","Member(x2, Sigma(x0))"),
      // Name: sigma4
      // Role: axiom
      List("~Little_set(x0)","Little_set(Sigma(x0))"),
      // Name: subset1
      // Role: axiom
      List("~Member(x0, x1)","~Subset(x1, x2)","Member(x0, x2)"),
      // Name: subset2
      // Role: axiom
      List("Member(F17(x0, x1), x0)","Subset(x0, x1)"),
      // Name: subset3
      // Role: axiom
      List("~Member(F17(x0, x1), x1)","Subset(x0, x1)"),
      // Name: proper_subset1
      // Role: axiom
      List("~Proper_subset(x0, x1)","Subset(x0, x1)"),
      // Name: proper_subset2
      // Role: axiom
      List("~(x0 = x1)","~Proper_subset(x0, x1)"),
      // Name: proper_subset3
      // Role: axiom
      List("~Subset(x0, x1)","(x0 = x1)","Proper_subset(x0, x1)"),
      // Name: powerset1
      // Role: axiom
      List("~Member(x1, Powerset(x0))","Subset(x1, x0)"),
      // Name: powerset2
      // Role: axiom
      List("~Little_set(x1)","~Subset(x1, x0)","Member(x1, Powerset(x0))"),
      // Name: powerset3
      // Role: axiom
      List("~Little_set(x0)","Little_set(Powerset(x0))"),
      // Name: relation1
      // Role: axiom
      List("~Member(x0, x1)","~Relation(x1)","Ordered_pair_predicate(x0)"),
      // Name: relation2
      // Role: axiom
      List("Member(F18(x0), x0)","Relation(x0)"),
      // Name: relation3
      // Role: axiom
      List("~Ordered_pair_predicate(F18(x0))","Relation(x0)"),
      // Name: single_valued_set1
      // Role: axiom
      List("~Little_set(x0)","~Little_set(x1)","~Little_set(x2)","~Member(Ordered_pair(x0, x1), x3)","~Member(Ordered_pair(x0, x2), x3)","~Single_valued_set(x3)","(x1 = x2)"),
      // Name: single_valued_set2
      // Role: axiom
      List("Little_set(F19(x0))","Single_valued_set(x0)"),
      // Name: single_valued_set3
      // Role: axiom
      List("Little_set(F20(x0))","Single_valued_set(x0)"),
      // Name: single_valued_set4
      // Role: axiom
      List("Little_set(F21(x0))","Single_valued_set(x0)"),
      // Name: single_valued_set5
      // Role: axiom
      List("Member(Ordered_pair(F19(x0), F20(x0)), x0)","Single_valued_set(x0)"),
      // Name: single_valued_set6
      // Role: axiom
      List("Member(Ordered_pair(F19(x0), F21(x0)), x0)","Single_valued_set(x0)"),
      // Name: single_valued_set7
      // Role: axiom
      List("~(F20(x0) = F21(x0))","Single_valued_set(x0)"),
      // Name: function1
      // Role: axiom
      List("~Function(x0)","Relation(x0)"),
      // Name: function2
      // Role: axiom
      List("~Function(x0)","Single_valued_set(x0)"),
      // Name: function3
      // Role: axiom
      List("~Relation(x0)","~Single_valued_set(x0)","Function(x0)"),
      // Name: image_and_substitution1
      // Role: axiom
      List("~Member(x2, Image(x0, x1))","Ordered_pair_predicate(F22(x2, x0, x1))"),
      // Name: image_and_substitution2
      // Role: axiom
      List("~Member(x2, Image(x0, x1))","Member(F22(x2, x0, x1), x1)"),
      // Name: image_and_substitution3
      // Role: axiom
      List("~Member(x2, Image(x0, x1))","Member(First(F22(x2, x0, x1)), x0)"),
      // Name: image_and_substitution4
      // Role: axiom
      List("~Member(x2, Image(x0, x1))","(Second(F22(x2, x0, x1)) = x2)"),
      // Name: image_and_substitution5
      // Role: axiom
      List("~(Second(x2) = x3)","~Little_set(x3)","~Member(x2, x1)","~Member(First(x2), x0)","~Ordered_pair_predicate(x2)","Member(x3, Image(x0, x1))"),
      // Name: image_and_substitution6
      // Role: axiom
      List("~Function(x1)","~Little_set(x0)","Little_set(Image(x0, x1))"),
      // Name: disjoint1
      // Role: axiom
      List("~Disjoint(x1, x2)","~Member(x0, x1)","~Member(x0, x2)"),
      // Name: disjoint2
      // Role: axiom
      List("Disjoint(x0, x1)","Member(F23(x0, x1), x0)"),
      // Name: disjoint3
      // Role: axiom
      List("Disjoint(x0, x1)","Member(F23(x0, x1), x1)"),
      // Name: regularity1
      // Role: axiom
      List("(x0 = Empty_set)","Member(F24(x0), x0)"),
      // Name: regularity2
      // Role: axiom
      List("(x0 = Empty_set)","Disjoint(F24(x0), x0)"),
      // Name: choice1
      // Role: axiom
      List("Function(F25)"),
      // Name: choice2
      // Role: axiom
      List("~Little_set(x0)","(x0 = Empty_set)","Member(F26(x0), x0)"),
      // Name: choice3
      // Role: axiom
      List("~Little_set(x0)","(x0 = Empty_set)","Member(Ordered_pair(x0, F26(x0)), F25)"),
      // Name: range_of1
      // Role: axiom
      List("~Member(x1, Range_of(x0))","Ordered_pair_predicate(F27(x1, x0))"),
      // Name: range_of2
      // Role: axiom
      List("~Member(x1, Range_of(x0))","Member(F27(x1, x0), x0)"),
      // Name: range_of3
      // Role: axiom
      List("~Member(x1, Range_of(x0))","(x1 = Second(F27(x1, x0)))"),
      // Name: range_of4
      // Role: axiom
      List("~(x2 = Second(x1))","~Little_set(x2)","~Member(x1, x0)","~Ordered_pair_predicate(x1)","Member(x2, Range_of(x0))"),
      // Name: identity_relation1
      // Role: axiom
      List("~Member(x0, Identity_relation)","Ordered_pair_predicate(x0)"),
      // Name: identity_relation2
      // Role: axiom
      List("~Member(x0, Identity_relation)","(First(x0) = Second(x0))"),
      // Name: identity_relation3
      // Role: axiom
      List("~(First(x0) = Second(x0))","~Little_set(x0)","~Ordered_pair_predicate(x0)","Member(x0, Identity_relation)"),
      // Name: restrict
      // Role: axiom
      List("(Restrict(x0, x1) = Intersection(x0, Cross_product(x1, Universal_set)))"),
      // Name: one_to_one_function1
      // Role: axiom
      List("~One_to_one_function(x0)","Function(x0)"),
      // Name: one_to_one_function2
      // Role: axiom
      List("~One_to_one_function(x0)","Function(Converse(x0))"),
      // Name: one_to_one_function3
      // Role: axiom
      List("~Function(x0)","~Function(Converse(x0))","One_to_one_function(x0)"),
      // Name: apply1
      // Role: axiom
      List("~Member(x2, Apply(x0, x1))","Ordered_pair_predicate(F28(x2, x0, x1))"),
      // Name: apply2
      // Role: axiom
      List("~Member(x2, Apply(x0, x1))","Member(F28(x2, x0, x1), x0)"),
      // Name: apply3
      // Role: axiom
      List("~Member(x2, Apply(x0, x1))","(First(F28(x2, x0, x1)) = x1)"),
      // Name: apply4
      // Role: axiom
      List("~Member(x2, Apply(x0, x1))","Member(x2, Second(F28(x2, x0, x1)))"),
      // Name: apply5
      // Role: axiom
      List("~(First(x0) = x2)","~Member(x0, x1)","~Member(x3, Second(x0))","~Ordered_pair_predicate(x0)","Member(x3, Apply(x1, x2))"),
      // Name: apply_to_two_arguments
      // Role: axiom
      List("(Apply_to_two_arguments(x1, x0, x2) = Apply(x1, Ordered_pair(x0, x2)))"),
      // Name: maps1
      // Role: axiom
      List("~Maps(x1, x0, x2)","Function(x1)"),
      // Name: maps2
      // Role: axiom
      List("~Maps(x1, x0, x2)","(Domain_of(x1) = x0)"),
      // Name: maps3
      // Role: axiom
      List("~Maps(x1, x0, x2)","Subset(Range_of(x1), x2)"),
      // Name: maps4
      // Role: axiom
      List("~(Domain_of(x1) = x0)","~Function(x1)","~Subset(Range_of(x1), x2)","Maps(x1, x0, x2)"),
      // Name: closed1
      // Role: axiom
      List("~Closed(x1, x0)","Little_set(x1)"),
      // Name: closed2
      // Role: axiom
      List("~Closed(x1, x0)","Little_set(x0)"),
      // Name: closed3
      // Role: axiom
      List("~Closed(x1, x0)","Maps(x0, Cross_product(x1, x1), x1)"),
      // Name: closed4
      // Role: axiom
      List("~Little_set(x0)","~Little_set(x1)","~Maps(x0, Cross_product(x1, x1), x1)","Closed(x1, x0)"),
      // Name: compose1
      // Role: axiom
      List("~Member(x2, Compose(x0, x1))","Little_set(F29(x2, x0, x1))"),
      // Name: compose2
      // Role: axiom
      List("~Member(x2, Compose(x0, x1))","Little_set(F30(x2, x0, x1))"),
      // Name: compose3
      // Role: axiom
      List("~Member(x2, Compose(x0, x1))","Little_set(F31(x2, x0, x1))"),
      // Name: compose4
      // Role: axiom
      List("~Member(x2, Compose(x0, x1))","(x2 = Ordered_pair(F29(x2, x0, x1), F30(x2, x0, x1)))"),
      // Name: compose5
      // Role: axiom
      List("~Member(x2, Compose(x0, x1))","Member(Ordered_pair(F29(x2, x0, x1), F31(x2, x0, x1)), x0)"),
      // Name: compose6
      // Role: axiom
      List("~Member(x2, Compose(x0, x1))","Member(Ordered_pair(F31(x2, x0, x1), F30(x2, x0, x1)), x1)"),
      // Name: compose7
      // Role: axiom
      List("~(x5 = Ordered_pair(x1, x4))","~Little_set(x0)","~Little_set(x1)","~Little_set(x4)","~Little_set(x5)","~Member(Ordered_pair(x0, x4), x3)","~Member(Ordered_pair(x1, x0), x2)","Member(x5, Compose(x2, x3))"),
      // Name: homomorphism1
      // Role: axiom
      List("~Homomorphism(x2, x3, x0, x4, x1)","Closed(x3, x0)"),
      // Name: homomorphism2
      // Role: axiom
      List("~Homomorphism(x2, x3, x0, x4, x1)","Closed(x4, x1)"),
      // Name: homomorphism3
      // Role: axiom
      List("~Homomorphism(x2, x3, x0, x4, x1)","Maps(x2, x3, x4)"),
      // Name: homomorphism4
      // Role: axiom
      List("~Homomorphism(x3, x4, x1, x5, x2)","~Member(x0, x4)","~Member(x6, x4)","(Apply(x3, Apply_to_two_arguments(x1, x0, x6)) = Apply_to_two_arguments(x2, Apply(x3, x0), Apply(x3, x6)))"),
      // Name: homomorphism5
      // Role: axiom
      List("~Closed(x3, x0)","~Closed(x4, x1)","~Maps(x2, x3, x4)","Homomorphism(x2, x3, x0, x4, x1)","Member(F32(x2, x3, x0, x4, x1), x3)"),
      // Name: homomorphism6
      // Role: axiom
      List("~Closed(x3, x0)","~Closed(x4, x1)","~Maps(x2, x3, x4)","Homomorphism(x2, x3, x0, x4, x1)","Member(F33(x2, x3, x0, x4, x1), x3)"),
      // Name: homomorphism7
      // Role: axiom
      List("~(Apply(x2, Apply_to_two_arguments(x0, F32(x2, x3, x0, x4, x1), F33(x2, x3, x0, x4, x1))) = Apply_to_two_arguments(x1, Apply(x2, F32(x2, x3, x0, x4, x1)), Apply(x2, F33(x2, x3, x0, x4, x1))))","~Closed(x3, x0)","~Closed(x4, x1)","~Maps(x2, x3, x4)","Homomorphism(x2, x3, x0, x4, x1)")).
    map(_.map(SimpleParse.parse(_).get))

  def clauses =
    this.mainClauses
}
