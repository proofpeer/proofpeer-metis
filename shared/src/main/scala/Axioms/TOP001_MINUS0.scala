package proofpeer.metis.testing.tptp.Axioms

import proofpeer.metis.SimpleParse
import proofpeer.metis.testing.tptp._

object TOP001_MINUS0 {
  val mainClauses =
    List(
      // Name: union_of_members_1
      // Role: axiom
      List("~Element_of_set(x0, Union_of_members(x1))","Element_of_set(x0, F1(x1, x0))"),
      // Name: union_of_members_2
      // Role: axiom
      List("~Element_of_set(x0, Union_of_members(x1))","Element_of_collection(F1(x1, x0), x1)"),
      // Name: union_of_members_3
      // Role: axiom
      List("~Element_of_collection(x1, x2)","~Element_of_set(x0, x1)","Element_of_set(x0, Union_of_members(x2))"),
      // Name: intersection_of_members_4
      // Role: axiom
      List("~Element_of_collection(x1, x2)","~Element_of_set(x0, Intersection_of_members(x2))","Element_of_set(x0, x1)"),
      // Name: intersection_of_members_5
      // Role: axiom
      List("Element_of_collection(F2(x1, x0), x1)","Element_of_set(x0, Intersection_of_members(x1))"),
      // Name: intersection_of_members_6
      // Role: axiom
      List("~Element_of_set(x0, F2(x1, x0))","Element_of_set(x0, Intersection_of_members(x1))"),
      // Name: topological_space_7
      // Role: axiom
      List("~Topological_space(x1, x0)","Equal_sets(Union_of_members(x0), x1)"),
      // Name: topological_space_8
      // Role: axiom
      List("~Topological_space(x1, x0)","Element_of_collection(Empty_set, x0)"),
      // Name: topological_space_9
      // Role: axiom
      List("~Topological_space(x1, x0)","Element_of_collection(x1, x0)"),
      // Name: topological_space_10
      // Role: axiom
      List("~Element_of_collection(x2, x0)","~Element_of_collection(x3, x0)","~Topological_space(x1, x0)","Element_of_collection(Intersection_of_sets(x2, x3), x0)"),
      // Name: topological_space_11
      // Role: axiom
      List("~Subset_collections(x0, x1)","~Topological_space(x2, x1)","Element_of_collection(Union_of_members(x0), x1)"),
      // Name: topological_space_12
      // Role: axiom
      List("~Element_of_collection(x1, x0)","~Element_of_collection(Empty_set, x0)","~Equal_sets(Union_of_members(x0), x1)","Element_of_collection(F3(x1, x0), x0)","Subset_collections(F5(x1, x0), x0)","Topological_space(x1, x0)"),
      // Name: topological_space_13
      // Role: axiom
      List("~Element_of_collection(x1, x0)","~Element_of_collection(Empty_set, x0)","~Element_of_collection(Union_of_members(F5(x1, x0)), x0)","~Equal_sets(Union_of_members(x0), x1)","Element_of_collection(F3(x1, x0), x0)","Topological_space(x1, x0)"),
      // Name: topological_space_14
      // Role: axiom
      List("~Element_of_collection(x1, x0)","~Element_of_collection(Empty_set, x0)","~Equal_sets(Union_of_members(x0), x1)","Element_of_collection(F4(x1, x0), x0)","Subset_collections(F5(x1, x0), x0)","Topological_space(x1, x0)"),
      // Name: topological_space_15
      // Role: axiom
      List("~Element_of_collection(x1, x0)","~Element_of_collection(Empty_set, x0)","~Element_of_collection(Union_of_members(F5(x1, x0)), x0)","~Equal_sets(Union_of_members(x0), x1)","Element_of_collection(F4(x1, x0), x0)","Topological_space(x1, x0)"),
      // Name: topological_space_16
      // Role: axiom
      List("~Element_of_collection(x1, x0)","~Element_of_collection(Empty_set, x0)","~Element_of_collection(Intersection_of_sets(F3(x1, x0), F4(x1, x0)), x0)","~Equal_sets(Union_of_members(x0), x1)","Subset_collections(F5(x1, x0), x0)","Topological_space(x1, x0)"),
      // Name: topological_space_17
      // Role: axiom
      List("~Element_of_collection(x1, x0)","~Element_of_collection(Empty_set, x0)","~Element_of_collection(Intersection_of_sets(F3(x1, x0), F4(x1, x0)), x0)","~Element_of_collection(Union_of_members(F5(x1, x0)), x0)","~Equal_sets(Union_of_members(x0), x1)","Topological_space(x1, x0)"),
      // Name: open_set_18
      // Role: axiom
      List("~Open(x0, x2, x1)","Topological_space(x2, x1)"),
      // Name: open_set_19
      // Role: axiom
      List("~Open(x0, x2, x1)","Element_of_collection(x0, x1)"),
      // Name: open_set_20
      // Role: axiom
      List("~Element_of_collection(x0, x1)","~Topological_space(x2, x1)","Open(x0, x2, x1)"),
      // Name: closed_set_21
      // Role: axiom
      List("~Closed(x0, x2, x1)","Topological_space(x2, x1)"),
      // Name: closed_set_22
      // Role: axiom
      List("~Closed(x0, x2, x1)","Open(Relative_complement_sets(x0, x2), x2, x1)"),
      // Name: closed_set_23
      // Role: axiom
      List("~Open(Relative_complement_sets(x0, x2), x2, x1)","~Topological_space(x2, x1)","Closed(x0, x2, x1)"),
      // Name: finer_topology_24
      // Role: axiom
      List("~Finer(x1, x0, x2)","Topological_space(x2, x1)"),
      // Name: finer_topology_25
      // Role: axiom
      List("~Finer(x1, x0, x2)","Topological_space(x2, x0)"),
      // Name: finer_topology_26
      // Role: axiom
      List("~Finer(x1, x0, x2)","Subset_collections(x0, x1)"),
      // Name: finer_topology_27
      // Role: axiom
      List("~Subset_collections(x0, x1)","~Topological_space(x2, x0)","~Topological_space(x2, x1)","Finer(x1, x0, x2)"),
      // Name: basis_for_topology_28
      // Role: axiom
      List("~Basis(x1, x0)","Equal_sets(Union_of_members(x0), x1)"),
      // Name: basis_for_topology_29
      // Role: axiom
      List("~Basis(x3, x2)","~Element_of_collection(x0, x2)","~Element_of_collection(x1, x2)","~Element_of_set(x4, x3)","~Element_of_set(x4, Intersection_of_sets(x0, x1))","Element_of_set(x4, F6(x3, x2, x4, x0, x1))"),
      // Name: basis_for_topology_30
      // Role: axiom
      List("~Basis(x3, x2)","~Element_of_collection(x0, x2)","~Element_of_collection(x1, x2)","~Element_of_set(x4, x3)","~Element_of_set(x4, Intersection_of_sets(x0, x1))","Element_of_collection(F6(x3, x2, x4, x0, x1), x2)"),
      // Name: basis_for_topology_31
      // Role: axiom
      List("~Basis(x3, x2)","~Element_of_collection(x0, x2)","~Element_of_collection(x1, x2)","~Element_of_set(x4, x3)","~Element_of_set(x4, Intersection_of_sets(x0, x1))","Subset_sets(F6(x3, x2, x4, x0, x1), Intersection_of_sets(x0, x1))"),
      // Name: basis_for_topology_32
      // Role: axiom
      List("~Equal_sets(Union_of_members(x0), x1)","Basis(x1, x0)","Element_of_set(F7(x1, x0), x1)"),
      // Name: basis_for_topology_33
      // Role: axiom
      List("~Equal_sets(Union_of_members(x0), x1)","Basis(x1, x0)","Element_of_collection(F8(x1, x0), x0)"),
      // Name: basis_for_topology_34
      // Role: axiom
      List("~Equal_sets(Union_of_members(x0), x1)","Basis(x1, x0)","Element_of_collection(F9(x1, x0), x0)"),
      // Name: basis_for_topology_35
      // Role: axiom
      List("~Equal_sets(Union_of_members(x0), x1)","Basis(x1, x0)","Element_of_set(F7(x1, x0), Intersection_of_sets(F8(x1, x0), F9(x1, x0)))"),
      // Name: basis_for_topology_36
      // Role: axiom
      List("~Element_of_collection(x0, x1)","~Element_of_set(F7(x2, x1), x0)","~Equal_sets(Union_of_members(x1), x2)","~Subset_sets(x0, Intersection_of_sets(F8(x2, x1), F9(x2, x1)))","Basis(x2, x1)"),
      // Name: topology_generated_37
      // Role: axiom
      List("~Element_of_collection(x0, Top_of_basis(x1))","~Element_of_set(x2, x0)","Element_of_set(x2, F10(x1, x0, x2))"),
      // Name: topology_generated_38
      // Role: axiom
      List("~Element_of_collection(x0, Top_of_basis(x1))","~Element_of_set(x2, x0)","Element_of_collection(F10(x1, x0, x2), x1)"),
      // Name: topology_generated_39
      // Role: axiom
      List("~Element_of_collection(x0, Top_of_basis(x1))","~Element_of_set(x2, x0)","Subset_sets(F10(x1, x0, x2), x0)"),
      // Name: topology_generated_40
      // Role: axiom
      List("Element_of_collection(x0, Top_of_basis(x1))","Element_of_set(F11(x1, x0), x0)"),
      // Name: topology_generated_41
      // Role: axiom
      List("~Element_of_collection(x1, x2)","~Element_of_set(F11(x2, x0), x1)","~Subset_sets(x1, x0)","Element_of_collection(x0, Top_of_basis(x2))"),
      // Name: subspace_topology_42
      // Role: axiom
      List("~Element_of_collection(x0, Subspace_topology(x2, x1, x3))","Topological_space(x2, x1)"),
      // Name: subspace_topology_43
      // Role: axiom
      List("~Element_of_collection(x0, Subspace_topology(x2, x1, x3))","Subset_sets(x3, x2)"),
      // Name: subspace_topology_44
      // Role: axiom
      List("~Element_of_collection(x0, Subspace_topology(x2, x1, x3))","Element_of_collection(F12(x2, x1, x3, x0), x1)"),
      // Name: subspace_topology_45
      // Role: axiom
      List("~Element_of_collection(x0, Subspace_topology(x2, x1, x3))","Equal_sets(x0, Intersection_of_sets(x3, F12(x2, x1, x3, x0)))"),
      // Name: subspace_topology_46
      // Role: axiom
      List("~Element_of_collection(x1, x2)","~Equal_sets(x0, Intersection_of_sets(x4, x1))","~Subset_sets(x4, x3)","~Topological_space(x3, x2)","Element_of_collection(x0, Subspace_topology(x3, x2, x4))"),
      // Name: interior_47
      // Role: axiom
      List("~Element_of_set(x0, Interior(x3, x2, x1))","Topological_space(x2, x1)"),
      // Name: interior_48
      // Role: axiom
      List("~Element_of_set(x0, Interior(x3, x2, x1))","Subset_sets(x3, x2)"),
      // Name: interior_49
      // Role: axiom
      List("~Element_of_set(x0, Interior(x3, x2, x1))","Element_of_set(x0, F13(x3, x2, x1, x0))"),
      // Name: interior_50
      // Role: axiom
      List("~Element_of_set(x0, Interior(x3, x2, x1))","Subset_sets(F13(x3, x2, x1, x0), x3)"),
      // Name: interior_51
      // Role: axiom
      List("~Element_of_set(x0, Interior(x3, x2, x1))","Open(F13(x3, x2, x1, x0), x2, x1)"),
      // Name: interior_52
      // Role: axiom
      List("~Element_of_set(x0, x1)","~Open(x1, x3, x2)","~Subset_sets(x1, x4)","~Subset_sets(x4, x3)","~Topological_space(x3, x2)","Element_of_set(x0, Interior(x4, x3, x2))"),
      // Name: closure_53
      // Role: axiom
      List("~Element_of_set(x0, Closure(x3, x2, x1))","Topological_space(x2, x1)"),
      // Name: closure_54
      // Role: axiom
      List("~Element_of_set(x0, Closure(x3, x2, x1))","Subset_sets(x3, x2)"),
      // Name: closure_55
      // Role: axiom
      List("~Closed(x1, x3, x2)","~Element_of_set(x0, Closure(x4, x3, x2))","~Subset_sets(x4, x1)","Element_of_set(x0, x1)"),
      // Name: closure_56
      // Role: axiom
      List("~Subset_sets(x3, x2)","~Topological_space(x2, x1)","Element_of_set(x0, Closure(x3, x2, x1))","Subset_sets(x3, F14(x3, x2, x1, x0))"),
      // Name: closure_57
      // Role: axiom
      List("~Subset_sets(x3, x2)","~Topological_space(x2, x1)","Closed(F14(x3, x2, x1, x0), x2, x1)","Element_of_set(x0, Closure(x3, x2, x1))"),
      // Name: closure_58
      // Role: axiom
      List("~Element_of_set(x0, F14(x3, x2, x1, x0))","~Subset_sets(x3, x2)","~Topological_space(x2, x1)","Element_of_set(x0, Closure(x3, x2, x1))"),
      // Name: neighborhood_59
      // Role: axiom
      List("~Neighborhood(x0, x3, x2, x1)","Topological_space(x2, x1)"),
      // Name: neighborhood_60
      // Role: axiom
      List("~Neighborhood(x0, x3, x2, x1)","Open(x0, x2, x1)"),
      // Name: neighborhood_61
      // Role: axiom
      List("~Neighborhood(x0, x3, x2, x1)","Element_of_set(x3, x0)"),
      // Name: neighborhood_62
      // Role: axiom
      List("~Element_of_set(x3, x0)","~Open(x0, x2, x1)","~Topological_space(x2, x1)","Neighborhood(x0, x3, x2, x1)"),
      // Name: limit_point_63
      // Role: axiom
      List("~Limit_point(x3, x2, x1, x0)","Topological_space(x1, x0)"),
      // Name: limit_point_64
      // Role: axiom
      List("~Limit_point(x3, x2, x1, x0)","Subset_sets(x2, x1)"),
      // Name: limit_point_65
      // Role: axiom
      List("~Limit_point(x4, x3, x2, x1)","~Neighborhood(x0, x4, x2, x1)","Element_of_set(F15(x4, x3, x2, x1, x0), Intersection_of_sets(x0, x3))"),
      // Name: limit_point_66
      // Role: axiom
      List("~Eq_p(F15(x4, x3, x2, x1, x0), x4)","~Limit_point(x4, x3, x2, x1)","~Neighborhood(x0, x4, x2, x1)"),
      // Name: limit_point_67
      // Role: axiom
      List("~Subset_sets(x2, x1)","~Topological_space(x1, x0)","Limit_point(x3, x2, x1, x0)","Neighborhood(F16(x3, x2, x1, x0), x3, x1, x0)"),
      // Name: limit_point_68
      // Role: axiom
      List("~Element_of_set(x0, Intersection_of_sets(F16(x4, x3, x2, x1), x3))","~Subset_sets(x3, x2)","~Topological_space(x2, x1)","Eq_p(x0, x4)","Limit_point(x4, x3, x2, x1)"),
      // Name: boundary_69
      // Role: axiom
      List("~Element_of_set(x0, Boundary(x3, x2, x1))","Topological_space(x2, x1)"),
      // Name: boundary_70
      // Role: axiom
      List("~Element_of_set(x0, Boundary(x3, x2, x1))","Element_of_set(x0, Closure(x3, x2, x1))"),
      // Name: boundary_71
      // Role: axiom
      List("~Element_of_set(x0, Boundary(x3, x2, x1))","Element_of_set(x0, Closure(Relative_complement_sets(x3, x2), x2, x1))"),
      // Name: boundary_72
      // Role: axiom
      List("~Element_of_set(x0, Closure(x3, x2, x1))","~Element_of_set(x0, Closure(Relative_complement_sets(x3, x2), x2, x1))","~Topological_space(x2, x1)","Element_of_set(x0, Boundary(x3, x2, x1))"),
      // Name: hausdorff_73
      // Role: axiom
      List("~Hausdorff(x1, x0)","Topological_space(x1, x0)"),
      // Name: hausdorff_74
      // Role: axiom
      List("~Element_of_set(x2, x1)","~Element_of_set(x3, x1)","~Hausdorff(x1, x0)","Eq_p(x2, x3)","Neighborhood(F17(x1, x0, x2, x3), x2, x1, x0)"),
      // Name: hausdorff_75
      // Role: axiom
      List("~Element_of_set(x2, x1)","~Element_of_set(x3, x1)","~Hausdorff(x1, x0)","Eq_p(x2, x3)","Neighborhood(F18(x1, x0, x2, x3), x3, x1, x0)"),
      // Name: hausdorff_76
      // Role: axiom
      List("~Element_of_set(x2, x1)","~Element_of_set(x3, x1)","~Hausdorff(x1, x0)","Disjoint_s(F17(x1, x0, x2, x3), F18(x1, x0, x2, x3))","Eq_p(x2, x3)"),
      // Name: hausdorff_77
      // Role: axiom
      List("~Topological_space(x1, x0)","Element_of_set(F19(x1, x0), x1)","Hausdorff(x1, x0)"),
      // Name: hausdorff_78
      // Role: axiom
      List("~Topological_space(x1, x0)","Element_of_set(F20(x1, x0), x1)","Hausdorff(x1, x0)"),
      // Name: hausdorff_79
      // Role: axiom
      List("~Eq_p(F19(x1, x0), F20(x1, x0))","~Topological_space(x1, x0)","Hausdorff(x1, x0)"),
      // Name: hausdorff_80
      // Role: axiom
      List("~Disjoint_s(x0, x1)","~Neighborhood(x0, F19(x3, x2), x3, x2)","~Neighborhood(x1, F20(x3, x2), x3, x2)","~Topological_space(x3, x2)","Hausdorff(x3, x2)"),
      // Name: separation_81
      // Role: axiom
      List("~Separation(x0, x1, x3, x2)","Topological_space(x3, x2)"),
      // Name: separation_82
      // Role: axiom
      List("~Equal_sets(x0, Empty_set)","~Separation(x0, x1, x3, x2)"),
      // Name: separation_83
      // Role: axiom
      List("~Equal_sets(x1, Empty_set)","~Separation(x0, x1, x3, x2)"),
      // Name: separation_84
      // Role: axiom
      List("~Separation(x0, x1, x3, x2)","Element_of_collection(x0, x2)"),
      // Name: separation_85
      // Role: axiom
      List("~Separation(x0, x1, x3, x2)","Element_of_collection(x1, x2)"),
      // Name: separation_86
      // Role: axiom
      List("~Separation(x0, x1, x3, x2)","Equal_sets(Union_of_sets(x0, x1), x3)"),
      // Name: separation_87
      // Role: axiom
      List("~Separation(x0, x1, x3, x2)","Disjoint_s(x0, x1)"),
      // Name: separation_88
      // Role: axiom
      List("~Disjoint_s(x0, x1)","~Element_of_collection(x0, x2)","~Element_of_collection(x1, x2)","~Equal_sets(Union_of_sets(x0, x1), x3)","~Topological_space(x3, x2)","Equal_sets(x0, Empty_set)","Equal_sets(x1, Empty_set)","Separation(x0, x1, x3, x2)"),
      // Name: connected_space_89
      // Role: axiom
      List("~Connected_space(x1, x0)","Topological_space(x1, x0)"),
      // Name: connected_space_90
      // Role: axiom
      List("~Connected_space(x3, x2)","~Separation(x0, x1, x3, x2)"),
      // Name: connected_space_91
      // Role: axiom
      List("~Topological_space(x1, x0)","Connected_space(x1, x0)","Separation(F21(x1, x0), F22(x1, x0), x1, x0)"),
      // Name: connected_set_92
      // Role: axiom
      List("~Connected_set(x0, x2, x1)","Topological_space(x2, x1)"),
      // Name: connected_set_93
      // Role: axiom
      List("~Connected_set(x0, x2, x1)","Subset_sets(x0, x2)"),
      // Name: connected_set_94
      // Role: axiom
      List("~Connected_set(x0, x2, x1)","Connected_space(x0, Subspace_topology(x2, x1, x0))"),
      // Name: connected_set_95
      // Role: axiom
      List("~Connected_space(x0, Subspace_topology(x2, x1, x0))","~Subset_sets(x0, x2)","~Topological_space(x2, x1)","Connected_set(x0, x2, x1)"),
      // Name: open_covering_96
      // Role: axiom
      List("~Open_covering(x0, x2, x1)","Topological_space(x2, x1)"),
      // Name: open_covering_97
      // Role: axiom
      List("~Open_covering(x0, x2, x1)","Subset_collections(x0, x1)"),
      // Name: open_covering_98
      // Role: axiom
      List("~Open_covering(x0, x2, x1)","Equal_sets(Union_of_members(x0), x2)"),
      // Name: open_covering_99
      // Role: axiom
      List("~Equal_sets(Union_of_members(x0), x2)","~Subset_collections(x0, x1)","~Topological_space(x2, x1)","Open_covering(x0, x2, x1)"),
      // Name: compact_space_100
      // Role: axiom
      List("~Compact_space(x1, x0)","Topological_space(x1, x0)"),
      // Name: compact_space_101
      // Role: axiom
      List("~Compact_space(x2, x1)","~Open_covering(x0, x2, x1)","Finite(F23(x2, x1, x0))"),
      // Name: compact_space_102
      // Role: axiom
      List("~Compact_space(x2, x1)","~Open_covering(x0, x2, x1)","Subset_collections(F23(x2, x1, x0), x0)"),
      // Name: compact_space_103
      // Role: axiom
      List("~Compact_space(x2, x1)","~Open_covering(x0, x2, x1)","Open_covering(F23(x2, x1, x0), x2, x1)"),
      // Name: compact_space_104
      // Role: axiom
      List("~Topological_space(x1, x0)","Compact_space(x1, x0)","Open_covering(F24(x1, x0), x1, x0)"),
      // Name: compact_space_105
      // Role: axiom
      List("~Finite(x0)","~Open_covering(x0, x2, x1)","~Subset_collections(x0, F24(x2, x1))","~Topological_space(x2, x1)","Compact_space(x2, x1)"),
      // Name: compact_set_106
      // Role: axiom
      List("~Compact_set(x0, x2, x1)","Topological_space(x2, x1)"),
      // Name: compact_set_107
      // Role: axiom
      List("~Compact_set(x0, x2, x1)","Subset_sets(x0, x2)"),
      // Name: compact_set_108
      // Role: axiom
      List("~Compact_set(x0, x2, x1)","Compact_space(x0, Subspace_topology(x2, x1, x0))"),
      // Name: compact_set_109
      // Role: axiom
      List("~Compact_space(x0, Subspace_topology(x2, x1, x0))","~Subset_sets(x0, x2)","~Topological_space(x2, x1)","Compact_set(x0, x2, x1)")).
    map(_.map(SimpleParse.parse(_).get))

  def clauses =
    this.mainClauses
}
