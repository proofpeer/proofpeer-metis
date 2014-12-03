package proofpeer.metis.testing.tptp.Axioms

import proofpeer.metis.testing.tptp._

object TOP001_MINUS0{
  val mainClauses =
    List(
      // Name: union_of_members_1
      // Role: axiom
      List("~element_of_set(x0, union_of_members(x1))","element_of_set(x0, f1(x1, x0))"),
      // Name: union_of_members_2
      // Role: axiom
      List("~element_of_set(x0, union_of_members(x1))","element_of_collection(f1(x1, x0), x1)"),
      // Name: union_of_members_3
      // Role: axiom
      List("~element_of_collection(x1, x2)","~element_of_set(x0, x1)","element_of_set(x0, union_of_members(x2))"),
      // Name: intersection_of_members_4
      // Role: axiom
      List("~element_of_collection(x1, x2)","~element_of_set(x0, intersection_of_members(x2))","element_of_set(x0, x1)"),
      // Name: intersection_of_members_5
      // Role: axiom
      List("element_of_collection(f2(x1, x0), x1)","element_of_set(x0, intersection_of_members(x1))"),
      // Name: intersection_of_members_6
      // Role: axiom
      List("~element_of_set(x0, f2(x1, x0))","element_of_set(x0, intersection_of_members(x1))"),
      // Name: topological_space_7
      // Role: axiom
      List("~topological_space(x1, x0)","equal_sets(union_of_members(x0), x1)"),
      // Name: topological_space_8
      // Role: axiom
      List("~topological_space(x1, x0)","element_of_collection(empty_set, x0)"),
      // Name: topological_space_9
      // Role: axiom
      List("~topological_space(x1, x0)","element_of_collection(x1, x0)"),
      // Name: topological_space_10
      // Role: axiom
      List("~element_of_collection(x2, x0)","~element_of_collection(x3, x0)","~topological_space(x1, x0)","element_of_collection(intersection_of_sets(x2, x3), x0)"),
      // Name: topological_space_11
      // Role: axiom
      List("~subset_collections(x0, x1)","~topological_space(x2, x1)","element_of_collection(union_of_members(x0), x1)"),
      // Name: topological_space_12
      // Role: axiom
      List("~element_of_collection(x1, x0)","~element_of_collection(empty_set, x0)","~equal_sets(union_of_members(x0), x1)","element_of_collection(f3(x1, x0), x0)","subset_collections(f5(x1, x0), x0)","topological_space(x1, x0)"),
      // Name: topological_space_13
      // Role: axiom
      List("~element_of_collection(x1, x0)","~element_of_collection(empty_set, x0)","~element_of_collection(union_of_members(f5(x1, x0)), x0)","~equal_sets(union_of_members(x0), x1)","element_of_collection(f3(x1, x0), x0)","topological_space(x1, x0)"),
      // Name: topological_space_14
      // Role: axiom
      List("~element_of_collection(x1, x0)","~element_of_collection(empty_set, x0)","~equal_sets(union_of_members(x0), x1)","element_of_collection(f4(x1, x0), x0)","subset_collections(f5(x1, x0), x0)","topological_space(x1, x0)"),
      // Name: topological_space_15
      // Role: axiom
      List("~element_of_collection(x1, x0)","~element_of_collection(empty_set, x0)","~element_of_collection(union_of_members(f5(x1, x0)), x0)","~equal_sets(union_of_members(x0), x1)","element_of_collection(f4(x1, x0), x0)","topological_space(x1, x0)"),
      // Name: topological_space_16
      // Role: axiom
      List("~element_of_collection(x1, x0)","~element_of_collection(empty_set, x0)","~element_of_collection(intersection_of_sets(f3(x1, x0), f4(x1, x0)), x0)","~equal_sets(union_of_members(x0), x1)","subset_collections(f5(x1, x0), x0)","topological_space(x1, x0)"),
      // Name: topological_space_17
      // Role: axiom
      List("~element_of_collection(x1, x0)","~element_of_collection(empty_set, x0)","~element_of_collection(intersection_of_sets(f3(x1, x0), f4(x1, x0)), x0)","~element_of_collection(union_of_members(f5(x1, x0)), x0)","~equal_sets(union_of_members(x0), x1)","topological_space(x1, x0)"),
      // Name: open_set_18
      // Role: axiom
      List("~open(x0, x2, x1)","topological_space(x2, x1)"),
      // Name: open_set_19
      // Role: axiom
      List("~open(x0, x2, x1)","element_of_collection(x0, x1)"),
      // Name: open_set_20
      // Role: axiom
      List("~element_of_collection(x0, x1)","~topological_space(x2, x1)","open(x0, x2, x1)"),
      // Name: closed_set_21
      // Role: axiom
      List("~closed(x0, x2, x1)","topological_space(x2, x1)"),
      // Name: closed_set_22
      // Role: axiom
      List("~closed(x0, x2, x1)","open(relative_complement_sets(x0, x2), x2, x1)"),
      // Name: closed_set_23
      // Role: axiom
      List("~open(relative_complement_sets(x0, x2), x2, x1)","~topological_space(x2, x1)","closed(x0, x2, x1)"),
      // Name: finer_topology_24
      // Role: axiom
      List("~finer(x1, x0, x2)","topological_space(x2, x1)"),
      // Name: finer_topology_25
      // Role: axiom
      List("~finer(x1, x0, x2)","topological_space(x2, x0)"),
      // Name: finer_topology_26
      // Role: axiom
      List("~finer(x1, x0, x2)","subset_collections(x0, x1)"),
      // Name: finer_topology_27
      // Role: axiom
      List("~subset_collections(x0, x1)","~topological_space(x2, x0)","~topological_space(x2, x1)","finer(x1, x0, x2)"),
      // Name: basis_for_topology_28
      // Role: axiom
      List("~basis(x1, x0)","equal_sets(union_of_members(x0), x1)"),
      // Name: basis_for_topology_29
      // Role: axiom
      List("~basis(x3, x2)","~element_of_collection(x0, x2)","~element_of_collection(x1, x2)","~element_of_set(x4, x3)","~element_of_set(x4, intersection_of_sets(x0, x1))","element_of_set(x4, f6(x3, x2, x4, x0, x1))"),
      // Name: basis_for_topology_30
      // Role: axiom
      List("~basis(x3, x2)","~element_of_collection(x0, x2)","~element_of_collection(x1, x2)","~element_of_set(x4, x3)","~element_of_set(x4, intersection_of_sets(x0, x1))","element_of_collection(f6(x3, x2, x4, x0, x1), x2)"),
      // Name: basis_for_topology_31
      // Role: axiom
      List("~basis(x3, x2)","~element_of_collection(x0, x2)","~element_of_collection(x1, x2)","~element_of_set(x4, x3)","~element_of_set(x4, intersection_of_sets(x0, x1))","subset_sets(f6(x3, x2, x4, x0, x1), intersection_of_sets(x0, x1))"),
      // Name: basis_for_topology_32
      // Role: axiom
      List("~equal_sets(union_of_members(x0), x1)","basis(x1, x0)","element_of_set(f7(x1, x0), x1)"),
      // Name: basis_for_topology_33
      // Role: axiom
      List("~equal_sets(union_of_members(x0), x1)","basis(x1, x0)","element_of_collection(f8(x1, x0), x0)"),
      // Name: basis_for_topology_34
      // Role: axiom
      List("~equal_sets(union_of_members(x0), x1)","basis(x1, x0)","element_of_collection(f9(x1, x0), x0)"),
      // Name: basis_for_topology_35
      // Role: axiom
      List("~equal_sets(union_of_members(x0), x1)","basis(x1, x0)","element_of_set(f7(x1, x0), intersection_of_sets(f8(x1, x0), f9(x1, x0)))"),
      // Name: basis_for_topology_36
      // Role: axiom
      List("~element_of_collection(x0, x1)","~element_of_set(f7(x2, x1), x0)","~equal_sets(union_of_members(x1), x2)","~subset_sets(x0, intersection_of_sets(f8(x2, x1), f9(x2, x1)))","basis(x2, x1)"),
      // Name: topology_generated_37
      // Role: axiom
      List("~element_of_collection(x0, top_of_basis(x1))","~element_of_set(x2, x0)","element_of_set(x2, f10(x1, x0, x2))"),
      // Name: topology_generated_38
      // Role: axiom
      List("~element_of_collection(x0, top_of_basis(x1))","~element_of_set(x2, x0)","element_of_collection(f10(x1, x0, x2), x1)"),
      // Name: topology_generated_39
      // Role: axiom
      List("~element_of_collection(x0, top_of_basis(x1))","~element_of_set(x2, x0)","subset_sets(f10(x1, x0, x2), x0)"),
      // Name: topology_generated_40
      // Role: axiom
      List("element_of_collection(x0, top_of_basis(x1))","element_of_set(f11(x1, x0), x0)"),
      // Name: topology_generated_41
      // Role: axiom
      List("~element_of_collection(x1, x2)","~element_of_set(f11(x2, x0), x1)","~subset_sets(x1, x0)","element_of_collection(x0, top_of_basis(x2))"),
      // Name: subspace_topology_42
      // Role: axiom
      List("~element_of_collection(x0, subspace_topology(x2, x1, x3))","topological_space(x2, x1)"),
      // Name: subspace_topology_43
      // Role: axiom
      List("~element_of_collection(x0, subspace_topology(x2, x1, x3))","subset_sets(x3, x2)"),
      // Name: subspace_topology_44
      // Role: axiom
      List("~element_of_collection(x0, subspace_topology(x2, x1, x3))","element_of_collection(f12(x2, x1, x3, x0), x1)"),
      // Name: subspace_topology_45
      // Role: axiom
      List("~element_of_collection(x0, subspace_topology(x2, x1, x3))","equal_sets(x0, intersection_of_sets(x3, f12(x2, x1, x3, x0)))"),
      // Name: subspace_topology_46
      // Role: axiom
      List("~element_of_collection(x1, x2)","~equal_sets(x0, intersection_of_sets(x4, x1))","~subset_sets(x4, x3)","~topological_space(x3, x2)","element_of_collection(x0, subspace_topology(x3, x2, x4))"),
      // Name: interior_47
      // Role: axiom
      List("~element_of_set(x0, interior(x3, x2, x1))","topological_space(x2, x1)"),
      // Name: interior_48
      // Role: axiom
      List("~element_of_set(x0, interior(x3, x2, x1))","subset_sets(x3, x2)"),
      // Name: interior_49
      // Role: axiom
      List("~element_of_set(x0, interior(x3, x2, x1))","element_of_set(x0, f13(x3, x2, x1, x0))"),
      // Name: interior_50
      // Role: axiom
      List("~element_of_set(x0, interior(x3, x2, x1))","subset_sets(f13(x3, x2, x1, x0), x3)"),
      // Name: interior_51
      // Role: axiom
      List("~element_of_set(x0, interior(x3, x2, x1))","open(f13(x3, x2, x1, x0), x2, x1)"),
      // Name: interior_52
      // Role: axiom
      List("~element_of_set(x0, x1)","~open(x1, x3, x2)","~subset_sets(x1, x4)","~subset_sets(x4, x3)","~topological_space(x3, x2)","element_of_set(x0, interior(x4, x3, x2))"),
      // Name: closure_53
      // Role: axiom
      List("~element_of_set(x0, closure(x3, x2, x1))","topological_space(x2, x1)"),
      // Name: closure_54
      // Role: axiom
      List("~element_of_set(x0, closure(x3, x2, x1))","subset_sets(x3, x2)"),
      // Name: closure_55
      // Role: axiom
      List("~closed(x1, x3, x2)","~element_of_set(x0, closure(x4, x3, x2))","~subset_sets(x4, x1)","element_of_set(x0, x1)"),
      // Name: closure_56
      // Role: axiom
      List("~subset_sets(x3, x2)","~topological_space(x2, x1)","element_of_set(x0, closure(x3, x2, x1))","subset_sets(x3, f14(x3, x2, x1, x0))"),
      // Name: closure_57
      // Role: axiom
      List("~subset_sets(x3, x2)","~topological_space(x2, x1)","closed(f14(x3, x2, x1, x0), x2, x1)","element_of_set(x0, closure(x3, x2, x1))"),
      // Name: closure_58
      // Role: axiom
      List("~element_of_set(x0, f14(x3, x2, x1, x0))","~subset_sets(x3, x2)","~topological_space(x2, x1)","element_of_set(x0, closure(x3, x2, x1))"),
      // Name: neighborhood_59
      // Role: axiom
      List("~neighborhood(x0, x3, x2, x1)","topological_space(x2, x1)"),
      // Name: neighborhood_60
      // Role: axiom
      List("~neighborhood(x0, x3, x2, x1)","open(x0, x2, x1)"),
      // Name: neighborhood_61
      // Role: axiom
      List("~neighborhood(x0, x3, x2, x1)","element_of_set(x3, x0)"),
      // Name: neighborhood_62
      // Role: axiom
      List("~element_of_set(x3, x0)","~open(x0, x2, x1)","~topological_space(x2, x1)","neighborhood(x0, x3, x2, x1)"),
      // Name: limit_point_63
      // Role: axiom
      List("~limit_point(x3, x2, x1, x0)","topological_space(x1, x0)"),
      // Name: limit_point_64
      // Role: axiom
      List("~limit_point(x3, x2, x1, x0)","subset_sets(x2, x1)"),
      // Name: limit_point_65
      // Role: axiom
      List("~limit_point(x4, x3, x2, x1)","~neighborhood(x0, x4, x2, x1)","element_of_set(f15(x4, x3, x2, x1, x0), intersection_of_sets(x0, x3))"),
      // Name: limit_point_66
      // Role: axiom
      List("~eq_p(f15(x4, x3, x2, x1, x0), x4)","~limit_point(x4, x3, x2, x1)","~neighborhood(x0, x4, x2, x1)"),
      // Name: limit_point_67
      // Role: axiom
      List("~subset_sets(x2, x1)","~topological_space(x1, x0)","limit_point(x3, x2, x1, x0)","neighborhood(f16(x3, x2, x1, x0), x3, x1, x0)"),
      // Name: limit_point_68
      // Role: axiom
      List("~element_of_set(x0, intersection_of_sets(f16(x4, x3, x2, x1), x3))","~subset_sets(x3, x2)","~topological_space(x2, x1)","eq_p(x0, x4)","limit_point(x4, x3, x2, x1)"),
      // Name: boundary_69
      // Role: axiom
      List("~element_of_set(x0, boundary(x3, x2, x1))","topological_space(x2, x1)"),
      // Name: boundary_70
      // Role: axiom
      List("~element_of_set(x0, boundary(x3, x2, x1))","element_of_set(x0, closure(x3, x2, x1))"),
      // Name: boundary_71
      // Role: axiom
      List("~element_of_set(x0, boundary(x3, x2, x1))","element_of_set(x0, closure(relative_complement_sets(x3, x2), x2, x1))"),
      // Name: boundary_72
      // Role: axiom
      List("~element_of_set(x0, closure(x3, x2, x1))","~element_of_set(x0, closure(relative_complement_sets(x3, x2), x2, x1))","~topological_space(x2, x1)","element_of_set(x0, boundary(x3, x2, x1))"),
      // Name: hausdorff_73
      // Role: axiom
      List("~hausdorff(x1, x0)","topological_space(x1, x0)"),
      // Name: hausdorff_74
      // Role: axiom
      List("~element_of_set(x2, x1)","~element_of_set(x3, x1)","~hausdorff(x1, x0)","eq_p(x2, x3)","neighborhood(f17(x1, x0, x2, x3), x2, x1, x0)"),
      // Name: hausdorff_75
      // Role: axiom
      List("~element_of_set(x2, x1)","~element_of_set(x3, x1)","~hausdorff(x1, x0)","eq_p(x2, x3)","neighborhood(f18(x1, x0, x2, x3), x3, x1, x0)"),
      // Name: hausdorff_76
      // Role: axiom
      List("~element_of_set(x2, x1)","~element_of_set(x3, x1)","~hausdorff(x1, x0)","disjoint_s(f17(x1, x0, x2, x3), f18(x1, x0, x2, x3))","eq_p(x2, x3)"),
      // Name: hausdorff_77
      // Role: axiom
      List("~topological_space(x1, x0)","element_of_set(f19(x1, x0), x1)","hausdorff(x1, x0)"),
      // Name: hausdorff_78
      // Role: axiom
      List("~topological_space(x1, x0)","element_of_set(f20(x1, x0), x1)","hausdorff(x1, x0)"),
      // Name: hausdorff_79
      // Role: axiom
      List("~eq_p(f19(x1, x0), f20(x1, x0))","~topological_space(x1, x0)","hausdorff(x1, x0)"),
      // Name: hausdorff_80
      // Role: axiom
      List("~disjoint_s(x0, x1)","~neighborhood(x0, f19(x3, x2), x3, x2)","~neighborhood(x1, f20(x3, x2), x3, x2)","~topological_space(x3, x2)","hausdorff(x3, x2)"),
      // Name: separation_81
      // Role: axiom
      List("~separation(x0, x1, x3, x2)","topological_space(x3, x2)"),
      // Name: separation_82
      // Role: axiom
      List("~equal_sets(x0, empty_set)","~separation(x0, x1, x3, x2)"),
      // Name: separation_83
      // Role: axiom
      List("~equal_sets(x1, empty_set)","~separation(x0, x1, x3, x2)"),
      // Name: separation_84
      // Role: axiom
      List("~separation(x0, x1, x3, x2)","element_of_collection(x0, x2)"),
      // Name: separation_85
      // Role: axiom
      List("~separation(x0, x1, x3, x2)","element_of_collection(x1, x2)"),
      // Name: separation_86
      // Role: axiom
      List("~separation(x0, x1, x3, x2)","equal_sets(union_of_sets(x0, x1), x3)"),
      // Name: separation_87
      // Role: axiom
      List("~separation(x0, x1, x3, x2)","disjoint_s(x0, x1)"),
      // Name: separation_88
      // Role: axiom
      List("~disjoint_s(x0, x1)","~element_of_collection(x0, x2)","~element_of_collection(x1, x2)","~equal_sets(union_of_sets(x0, x1), x3)","~topological_space(x3, x2)","equal_sets(x0, empty_set)","equal_sets(x1, empty_set)","separation(x0, x1, x3, x2)"),
      // Name: connected_space_89
      // Role: axiom
      List("~connected_space(x1, x0)","topological_space(x1, x0)"),
      // Name: connected_space_90
      // Role: axiom
      List("~connected_space(x3, x2)","~separation(x0, x1, x3, x2)"),
      // Name: connected_space_91
      // Role: axiom
      List("~topological_space(x1, x0)","connected_space(x1, x0)","separation(f21(x1, x0), f22(x1, x0), x1, x0)"),
      // Name: connected_set_92
      // Role: axiom
      List("~connected_set(x0, x2, x1)","topological_space(x2, x1)"),
      // Name: connected_set_93
      // Role: axiom
      List("~connected_set(x0, x2, x1)","subset_sets(x0, x2)"),
      // Name: connected_set_94
      // Role: axiom
      List("~connected_set(x0, x2, x1)","connected_space(x0, subspace_topology(x2, x1, x0))"),
      // Name: connected_set_95
      // Role: axiom
      List("~connected_space(x0, subspace_topology(x2, x1, x0))","~subset_sets(x0, x2)","~topological_space(x2, x1)","connected_set(x0, x2, x1)"),
      // Name: open_covering_96
      // Role: axiom
      List("~open_covering(x0, x2, x1)","topological_space(x2, x1)"),
      // Name: open_covering_97
      // Role: axiom
      List("~open_covering(x0, x2, x1)","subset_collections(x0, x1)"),
      // Name: open_covering_98
      // Role: axiom
      List("~open_covering(x0, x2, x1)","equal_sets(union_of_members(x0), x2)"),
      // Name: open_covering_99
      // Role: axiom
      List("~equal_sets(union_of_members(x0), x2)","~subset_collections(x0, x1)","~topological_space(x2, x1)","open_covering(x0, x2, x1)"),
      // Name: compact_space_100
      // Role: axiom
      List("~compact_space(x1, x0)","topological_space(x1, x0)"),
      // Name: compact_space_101
      // Role: axiom
      List("~compact_space(x2, x1)","~open_covering(x0, x2, x1)","finite(f23(x2, x1, x0))"),
      // Name: compact_space_102
      // Role: axiom
      List("~compact_space(x2, x1)","~open_covering(x0, x2, x1)","subset_collections(f23(x2, x1, x0), x0)"),
      // Name: compact_space_103
      // Role: axiom
      List("~compact_space(x2, x1)","~open_covering(x0, x2, x1)","open_covering(f23(x2, x1, x0), x2, x1)"),
      // Name: compact_space_104
      // Role: axiom
      List("~topological_space(x1, x0)","compact_space(x1, x0)","open_covering(f24(x1, x0), x1, x0)"),
      // Name: compact_space_105
      // Role: axiom
      List("~finite(x0)","~open_covering(x0, x2, x1)","~subset_collections(x0, f24(x2, x1))","~topological_space(x2, x1)","compact_space(x2, x1)"),
      // Name: compact_set_106
      // Role: axiom
      List("~compact_set(x0, x2, x1)","topological_space(x2, x1)"),
      // Name: compact_set_107
      // Role: axiom
      List("~compact_set(x0, x2, x1)","subset_sets(x0, x2)"),
      // Name: compact_set_108
      // Role: axiom
      List("~compact_set(x0, x2, x1)","compact_space(x0, subspace_topology(x2, x1, x0))"),
      // Name: compact_set_109
      // Role: axiom
      List("~compact_space(x0, subspace_topology(x2, x1, x0))","~subset_sets(x0, x2)","~topological_space(x2, x1)","compact_set(x0, x2, x1)"))
  
  def clauses =
     ++
      this.mainClauses
}
