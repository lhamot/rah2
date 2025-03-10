//
// Copyright (c) 2019 Loïc HAMOT
//
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//
#include <rah2/ranges.hpp>
#include <rah2/algorithm.hpp>
#include <clocale>

#ifdef RAH2_USE_EASTL

#include <EASTL/list.h>
#include <EASTL/map.h>
#include <EASTL/vector.h>
#include <EASTL/atomic.h>
#include <EASTL/random.h>
#include <EASTL/algorithm.h>

void* operator new[](
    size_t size,
    char const*, // pName,
    int, // flags,
    unsigned, // debugFlags,
    char const*, // file,
    int // line
)
{
    return new uint8_t[size];
}
void* operator new[](
    size_t size,
    size_t, // alignment,
    size_t, // alignmentOffset,
    char const*, // pName,
    int, // flags,
    unsigned, // debugFlags,
    char const*, // file,
    int // line
)
{
    return new uint8_t[size];
}

namespace EA
{
    namespace StdC
    {
        int Vsnprintf(
            char* EA_RESTRICT pDestination, size_t n, char const* EA_RESTRICT pFormat, va_list arguments)
        {
            return vsnprintf(pDestination, n, pFormat, arguments);
        }
    } // namespace StdC
} // namespace EA
#else

#include <list>
#include <map>
#include <vector>
#include <atomic>
#include <random>
#include <algorithm>

#endif

#include "test_helpers.hpp"

bool is_odd(int val)
{
    return val % 2 == 0;
}

/// [make_pipeable create]
auto test_count(int i)
{
    return RAH2_NS::ranges::make_pipeable([=](auto&& range)
                                          { return RAH2_STD::count(begin(range), end(range), i); });
}
/// [make_pipeable create]

// Test creation of a custom iterator
struct CustomGenerator
    : RAH2_NS::ranges::iterator_facade<CustomGenerator, RAH2_NS::default_sentinel_t, int, RAH2_STD::forward_iterator_tag>
{
    int y = 1;

    CustomGenerator& operator++()
    {
        y *= 2;
        return *this;
    }
    auto operator*() const
    {
        return y;
    }
    bool operator==(CustomGenerator) const
    {
        return y > 10;
    }
};

auto customGenerate()
{
    return RAH2_NS::ranges::subrange<CustomGenerator, CustomGenerator>{};
}

void test_empty_view();
void test_single_view();
void test_iota_view();
void test_istream_view();
void test_repeat_view();
void test_counted_view();
void test_owning_view();
void test_all_view();
void test_filter_view();
void test_take_view();
void test_drop_view();
void test_drop_while_view();
void test_join_view();
void test_split_view();
void test_enumerate_view();
void test_elements_view();
void test_values_view();
void test_keys_view();
void test_zip_view();
void test_zip_transform_view();
void test_adjacent_view();
void test_common_view();
void test_reverse_view();
void test_transform_view();
void test_adjacent_transform();
void test_slide_view();
void test_chunk_view();
void test_stride_view();
void test_ref_view();
void test_unbounded_view();
void test_irange_view();
void test_cycle_view();
void test_generate_view();
void test_slice_view();
void test_concat_view();
void test_set_difference_view();

void test_all_of();
void test_any_of();
void test_none_of();
void test_for_each();
void test_for_each_n();
void test_algo_count();
void test_count_if();
void test_mismatch();
void test_equal();
void test_lexicographical_compare();
void test_find();
void test_find_if();
void test_find_if_not();
void test_find_last();
void test_find_last_if();
void test_find_last_if_not();
void test_find_end();
void test_find_first_of();
void test_adjacent_find();
void test_search();
void test_search_n();
void test_contains();
void test_contains_subrange();
void test_starts_with();
void test_ends_with();
void test_copy();
void test_copy_if();
void test_copy_n();
void test_copy_backward();
void test_move();
void test_move_backward();
void test_fill();
void test_fill_n();
void test_transform();
void test_generate();
void test_generate_n();
void test_remove();
void test_remove_if();
void test_remove_copy();
void test_remove_copy_if();
void test_replace();
void test_replace_if();
void test_replace_copy();
void test_replace_copy_if();
void test_swap_ranges();
void test_reverse();
void test_reverse_copy();
void test_rotate();
void test_rotate_copy();
void test_shuffle();
void test_shift_left();
void test_shift_right();
void test_sample();
void test_unique();
void test_unique_copy();
void test_is_partitioned();
void test_partition();
void test_partition_copy();
void test_stable_partition();
void test_partition_point();
void test_is_sorted();
void test_is_sorted_until();
void test_sort();
void test_partial_sort();
void test_partial_sort_copy();
void test_stable_sort();
void test_nth_element();
void test_lower_bound();
void test_upper_bound();
void test_binary_search();
void test_equal_range();
void test_merge();
void test_inplace_merge();
void test_includes();
void test_set_difference();
void test_set_intersection();
void test_set_symmetric_difference();
void test_set_union();
void test_is_heap();
void test_is_heap_until();
void test_make_heap();
void test_push_heap();
void test_pop_heap();
void test_sort_heap();
void test_max();
void test_max_element();
void test_min();
void test_min_element();
void test_minmax();
void test_minmax_element();
void test_clamp();
void test_is_permutation();
void test_next_permutation();
void test_prev_permutation();
void test_iota();
void test_fold_left();
void test_fold_left_first();
void test_fold_right();
void test_fold_right_last();
void test_fold_left_with_iter();
void test_fold_left_first_with_iter();
void test_uninitialized_copy();
void test_uninitialized_copy_n();
void test_uninitialized_fill();
void test_uninitialized_fill_n();
void test_uninitialized_move();
void test_uninitialized_move_n();
void test_uninitialized_default_construct();
void test_uninitialized_default_construct_n();
void test_uninitialized_value_construct();
void test_uninitialized_value_construct_n();
void test_destroy();
void test_destroy_n();
void test_destroy_at();
void test_construct_at();

TestSuite testSuite;

template <typename T, bool Diagnostic = false>
struct concept_test_impl
{
    template <typename T2>
    using check_build = decltype(RAH2_STD::declval<T2>().a());

    template <typename T2>
    using check_true =
        RAH2_STD::enable_if_t<RAH2_NS::is_same_v<decltype(RAH2_STD::declval<T2>().b()), int>>;

    constexpr static bool value =
        RAH2_NS::concepts::is_true_v<Diagnostic, !RAH2_STD::has_virtual_destructor<T>::value>
        && RAH2_NS::concepts::compiles<Diagnostic, T, check_build>
        && RAH2_NS::concepts::compiles<Diagnostic, T, check_true>;
};

template <typename T>
constexpr bool concept_test = concept_test_impl<T>::value;

void test_concepts()
{
    // Check a code have to build (And in Diagnostic mode we have to wee where)
    // Check a criteria have to be true (And in Diagnostic mode we have to wee where)
    // Check a criteria have to be true AND build (And in Diagnostic mode we have to wee where)

    struct test_ok
    {
        RAH2_NODISCARD auto a() const
        {
            return 0;
        }
        RAH2_NODISCARD auto b() const
        {
            return 0;
        }
    };
    struct check_build_fail
    {
        RAH2_NODISCARD auto b() const
        {
            return 0;
        }
    };
    struct check_true_no_build
    {
        RAH2_NODISCARD auto a() const
        {
            return 0;
        }
    };
    struct check_true_is_false
    {
        RAH2_NODISCARD auto a() const
        {
            return 0;
        }
        RAH2_NODISCARD auto b() const
        {
            return false;
        }
    };
    struct check_traits_is_false // NOLINT(cppcoreguidelines-special-member-functions)
    {
        check_traits_is_false() = default;
        virtual ~check_traits_is_false() = default;

        RAH2_NODISCARD auto a() const
        {
            return 0;
        }
        RAH2_NODISCARD auto b() const
        {
            return 0;
        }
    };

    check_traits_is_false test;

    STATIC_ASSERT(concept_test<test_ok>);
    STATIC_ASSERT(!concept_test<check_build_fail>);
    STATIC_ASSERT(!concept_test<check_true_no_build>);
    STATIC_ASSERT(!concept_test<check_true_is_false>);
    STATIC_ASSERT(!concept_test<check_traits_is_false>);

    assert(concept_test<test_ok>);

    // To check error messages
    // STATIC_ASSERT((concept_test_impl<CheckBuildFail, true>::value)); // Ok with compiles + decltype
    // STATIC_ASSERT((concept_test_impl<CheckTrueNoBuild, true>::value)); // Ok with compiles + enable_if_t
    // STATIC_ASSERT((concept_test_impl<CheckTrueIsFalse, true>::value)); // Ok with compiles + enable_if_t
    // STATIC_ASSERT((concept_test_impl<CheckTraitsIsFalse, true>::value)); // Ok with is_true_v
}

void test_range_traits()
{
    testSuite.test_case("on non-range");
    CHECK(true);
    STATIC_ASSERT(!RAH2_NS::ranges::range<int>);
    STATIC_ASSERT(!RAH2_NS::ranges::borrowed_range<int>);
    STATIC_ASSERT(!RAH2_NS::ranges::sized_range<int>);
    STATIC_ASSERT(!RAH2_NS::ranges::view<int>);
    STATIC_ASSERT(!RAH2_NS::ranges::input_range<int>);
    STATIC_ASSERT((!RAH2_NS::ranges::output_range<int, int>));
    STATIC_ASSERT(!RAH2_NS::ranges::forward_range<int>);
    STATIC_ASSERT(!RAH2_NS::ranges::bidirectional_range<int>);
    STATIC_ASSERT(!RAH2_NS::ranges::random_access_range<int>);
    STATIC_ASSERT(!RAH2_NS::ranges::contiguous_range<int>);
    STATIC_ASSERT(!RAH2_NS::ranges::common_range<int>);
    STATIC_ASSERT(!RAH2_NS::ranges::viewable_range<int>);
    STATIC_ASSERT(!RAH2_NS::ranges::constant_range<int>);
}

struct show_thousands_sep : std::numpunct<char>
{
    show_thousands_sep() = default;
    show_thousands_sep(show_thousands_sep&) = delete;
    show_thousands_sep& operator=(show_thousands_sep&) = delete;
    char do_thousands_sep() const override
    {
        return ',';
    }
    std::string do_grouping() const override
    {
        return "\03";
    }
};

int main()
try
{
    std::cout.imbue(std::locale(std::locale::classic(), new show_thousands_sep));

    testSuite.addTest("Range_concepts", "*", test_range_traits);
    testSuite.addTest("concepts", "*", test_concepts);

    // Range_factories
    auto range_factories = "Range_factories";
    testSuite.addTest(range_factories, "ranges::empty_view", test_empty_view);
    testSuite.addTest(range_factories, "ranges::single_view", test_single_view);
    testSuite.addTest(range_factories, "ranges::iota_view", test_iota_view);
    testSuite.addTest(range_factories, "ranges::basic_istream_view", test_istream_view);
    testSuite.addTest(range_factories, "ranges::repeat_view", test_repeat_view);
    testSuite.addTest(range_factories, "ranges::irange_view", test_irange_view);
    testSuite.addTest(range_factories, "ranges::generate_view", test_generate_view);

    // testSuite.addTest(range_factories, "ranges::cartesian_product_view", test_cartesian_product_view);

    // Range_Adaptors
    auto range_adaptors = "Range_Adaptors";
    testSuite.addTest(range_adaptors, "views::counted", test_counted_view);
    testSuite.addTest(range_adaptors, "ranges::owning_view", test_owning_view);
    testSuite.addTest(range_adaptors, "views::all", test_all_view);
    testSuite.addTest(range_adaptors, "ranges::filter_view", test_filter_view);
    testSuite.addTest(range_adaptors, "ranges::take_view", test_take_view);
    testSuite.addTest(range_adaptors, "ranges::drop_view", test_drop_view);
    testSuite.addTest(range_adaptors, "ranges::drop_while_view", test_drop_while_view);
    testSuite.addTest(range_adaptors, "ranges::join_view", test_join_view);
    testSuite.addTest(range_adaptors, "ranges::split_view", test_split_view);
    testSuite.addTest(range_adaptors, "ranges::enumerate_view", test_enumerate_view);
    testSuite.addTest(range_adaptors, "ranges::elements_view", test_elements_view);
    testSuite.addTest(range_adaptors, "ranges::values_view", test_values_view);
    testSuite.addTest(range_adaptors, "ranges::keys_view", test_keys_view);
    testSuite.addTest(range_adaptors, "ranges::zip_view", test_zip_view);
    testSuite.addTest(range_adaptors, "ranges::test_zip_transform_view", test_zip_transform_view);
    testSuite.addTest(range_adaptors, "ranges::adjacent_view", test_adjacent_view);
    testSuite.addTest(range_adaptors, "ranges::common_view", test_common_view);
    testSuite.addTest(range_adaptors, "ranges::reverse_view", test_reverse_view);
    testSuite.addTest(range_adaptors, "ranges::zip_transform_view", test_transform_view);
    testSuite.addTest(range_adaptors, "ranges::adjacent_transform_view", test_adjacent_transform);
    testSuite.addTest(range_adaptors, "ranges::slide_view", test_slide_view);
    testSuite.addTest(range_adaptors, "ranges::chunk_view", test_chunk_view);
    testSuite.addTest(range_adaptors, "ranges::stride_view", test_stride_view);
    testSuite.addTest(range_adaptors, "ranges::ref_view", test_ref_view);
    testSuite.addTest(range_adaptors, "ranges::unbounded_view", test_unbounded_view);
    testSuite.addTest(range_adaptors, "ranges::cycle_view", test_cycle_view);
    testSuite.addTest(range_adaptors, "ranges::slice_view", test_slice_view);
    testSuite.addTest(range_adaptors, "ranges::concat_view", test_concat_view);
    testSuite.addTest(range_adaptors, "ranges::set_difference_view", test_set_difference_view);

    auto algorithms = "Algorithms";
    testSuite.addTest(algorithms, "ranges::all_of", test_all_of);
    testSuite.addTest(algorithms, "ranges::any_of", test_any_of);
    testSuite.addTest(algorithms, "ranges::none_of", test_none_of);
    testSuite.addTest(algorithms, "ranges::for_each", test_for_each);
    testSuite.addTest(algorithms, "ranges::for_each_n", test_for_each_n);
    testSuite.addTest(algorithms, "ranges::count", test_algo_count);
    testSuite.addTest(algorithms, "ranges::count_if", test_count_if);
    testSuite.addTest(algorithms, "ranges::mismatch", test_mismatch);
    testSuite.addTest(algorithms, "ranges::equal", test_equal);
    testSuite.addTest(algorithms, "ranges::lexicographical_compare", test_lexicographical_compare);
    testSuite.addTest(algorithms, "ranges::find", test_find);
    testSuite.addTest(algorithms, "ranges::find_if", test_find_if);
    testSuite.addTest(algorithms, "ranges::find_if_not", test_find_if_not);
    testSuite.addTest(algorithms, "ranges::find_last", test_find_last);
    testSuite.addTest(algorithms, "ranges::find_last_if", test_find_last_if);
    testSuite.addTest(algorithms, "ranges::find_last_if_not", test_find_last_if_not);
    testSuite.addTest(algorithms, "ranges::find_end", test_find_end);
    testSuite.addTest(algorithms, "ranges::find_first_of", test_find_first_of);
    testSuite.addTest(algorithms, "ranges::adjacent_find", test_adjacent_find);
    testSuite.addTest(algorithms, "ranges::search", test_search);
    testSuite.addTest(algorithms, "ranges::search_n", test_search_n);
    testSuite.addTest(algorithms, "ranges::contains", test_contains);
    testSuite.addTest(algorithms, "ranges::contains_subrange", test_contains_subrange);
    testSuite.addTest(algorithms, "ranges::starts_with", test_starts_with);
    testSuite.addTest(algorithms, "ranges::ends_with", test_ends_with);
    testSuite.addTest(algorithms, "ranges::copy", test_copy);
    testSuite.addTest(algorithms, "ranges::copy_if", test_copy_if);
    testSuite.addTest(algorithms, "ranges::copy_n", test_copy_n);
    testSuite.addTest(algorithms, "ranges::copy_backward", test_copy_backward);
    testSuite.addTest(algorithms, "ranges::move", test_move);
    testSuite.addTest(algorithms, "ranges::move_backward", test_move_backward);
    testSuite.addTest(algorithms, "ranges::fill", test_fill);
    testSuite.addTest(algorithms, "ranges::fill_n", test_fill_n);
    testSuite.addTest(algorithms, "ranges::transform", test_transform);
    testSuite.addTest(algorithms, "ranges::generate", test_generate);
    testSuite.addTest(algorithms, "ranges::generate_n", test_generate_n);
    testSuite.addTest(algorithms, "ranges::remove", test_remove);
    testSuite.addTest(algorithms, "ranges::remove_if", test_remove_if);
    testSuite.addTest(algorithms, "ranges::remove_copy", test_remove_copy);
    testSuite.addTest(algorithms, "ranges::remove_copy_if", test_remove_copy_if);
    testSuite.addTest(algorithms, "ranges::replace", test_replace);
    testSuite.addTest(algorithms, "ranges::replace_if", test_replace_if);
    testSuite.addTest(algorithms, "ranges::replace_copy", test_replace_copy);
    testSuite.addTest(algorithms, "ranges::replace_copy_if", test_replace_copy_if);
    testSuite.addTest(algorithms, "ranges::swap_ranges", test_swap_ranges);
    testSuite.addTest(algorithms, "ranges::reverse", test_reverse);
    testSuite.addTest(algorithms, "ranges::reverse_copy", test_reverse_copy);
    testSuite.addTest(algorithms, "ranges::rotate", test_rotate);
    testSuite.addTest(algorithms, "ranges::rotate_copy", test_rotate_copy);
    testSuite.addTest(algorithms, "ranges::shuffle", test_shuffle);
    testSuite.addTest(algorithms, "ranges::shift_left", test_shift_left);
    testSuite.addTest(algorithms, "ranges::shift_right", test_shift_right);
    testSuite.addTest(algorithms, "ranges::sample", test_sample);
    testSuite.addTest(algorithms, "ranges::unique", test_unique);
    testSuite.addTest(algorithms, "ranges::unique_copy", test_unique_copy);
    testSuite.addTest(algorithms, "ranges::is_partitioned", test_is_partitioned);
    testSuite.addTest(algorithms, "ranges::partition", test_partition);
    testSuite.addTest(algorithms, "ranges::partition_copy", test_partition_copy);
    testSuite.addTest(algorithms, "ranges::stable_partition", test_stable_partition);
    testSuite.addTest(algorithms, "ranges::partition_point", test_partition_point);
    testSuite.addTest(algorithms, "ranges::is_sorted", test_is_sorted);
    testSuite.addTest(algorithms, "ranges::is_sorted_until", test_is_sorted_until);
    testSuite.addTest(algorithms, "ranges::sort", test_sort);
    testSuite.addTest(algorithms, "ranges::partial_sort", test_partial_sort);
    testSuite.addTest(algorithms, "ranges::partial_sort_copy", test_partial_sort_copy);
    testSuite.addTest(algorithms, "ranges::stable_sort", test_stable_sort);
    testSuite.addTest(algorithms, "ranges::nth_element", test_nth_element);
    testSuite.addTest(algorithms, "ranges::lower_bound", test_lower_bound);
    testSuite.addTest(algorithms, "ranges::upper_bound", test_upper_bound);
    testSuite.addTest(algorithms, "ranges::binary_search", test_binary_search);
    testSuite.addTest(algorithms, "ranges::equal_range", test_equal_range);
    testSuite.addTest(algorithms, "ranges::merge", test_merge);
    testSuite.addTest(algorithms, "ranges::inplace_merge", test_inplace_merge);
    testSuite.addTest(algorithms, "ranges::includes", test_includes);
    testSuite.addTest(algorithms, "ranges::set_difference", test_set_difference);
    testSuite.addTest(algorithms, "ranges::set_intersection", test_set_intersection);
    testSuite.addTest(algorithms, "ranges::set_symmetric_difference", test_set_symmetric_difference);
    testSuite.addTest(algorithms, "ranges::set_union", test_set_union);
    testSuite.addTest(algorithms, "ranges::is_heap", test_is_heap);
    testSuite.addTest(algorithms, "ranges::is_heap_until", test_is_heap_until);
    testSuite.addTest(algorithms, "ranges::make_heap", test_make_heap);
    testSuite.addTest(algorithms, "ranges::push_heap", test_push_heap);
    testSuite.addTest(algorithms, "ranges::pop_heap", test_pop_heap);
    testSuite.addTest(algorithms, "ranges::sort_heap", test_sort_heap);
    testSuite.addTest(algorithms, "ranges::max", test_max);
    testSuite.addTest(algorithms, "ranges::max_element", test_max_element);
    testSuite.addTest(algorithms, "ranges::min", test_min);
    testSuite.addTest(algorithms, "ranges::min_element", test_min_element);
    testSuite.addTest(algorithms, "ranges::minmax", test_minmax);
    testSuite.addTest(algorithms, "ranges::minmax_element", test_minmax_element);
    testSuite.addTest(algorithms, "ranges::clamp", test_clamp);
    testSuite.addTest(algorithms, "ranges::is_permutation", test_is_permutation);
    testSuite.addTest(algorithms, "ranges::next_permutation", test_next_permutation);
    testSuite.addTest(algorithms, "ranges::prev_permutation", test_prev_permutation);
    testSuite.addTest(algorithms, "ranges::iota", test_iota);
    testSuite.addTest(algorithms, "ranges::fold_left", test_fold_left);
    testSuite.addTest(algorithms, "ranges::fold_left_first", test_fold_left_first);
    testSuite.addTest(algorithms, "ranges::fold_right", test_fold_right);
    testSuite.addTest(algorithms, "ranges::fold_right_last", test_fold_right_last);
    testSuite.addTest(algorithms, "ranges::fold_left_with_iter", test_fold_left_with_iter);
    testSuite.addTest(
        algorithms, "ranges::fold_left_first_with_iter", test_fold_left_first_with_iter);
    testSuite.addTest(algorithms, "ranges::uninitialized_copy", test_uninitialized_copy);
    testSuite.addTest(algorithms, "ranges::uninitialized_copy_n", test_uninitialized_copy_n);
    testSuite.addTest(algorithms, "ranges::uninitialized_fill", test_uninitialized_fill);
    testSuite.addTest(algorithms, "ranges::uninitialized_fill_n", test_uninitialized_fill_n);
    testSuite.addTest(algorithms, "ranges::uninitialized_move", test_uninitialized_move);
    testSuite.addTest(algorithms, "ranges::uninitialized_move_n", test_uninitialized_move_n);
    testSuite.addTest(
        algorithms, "ranges::uninitialized_default_construct", test_uninitialized_default_construct);
    testSuite.addTest(
        algorithms,
        "ranges::uninitialized_default_construct_n",
        test_uninitialized_default_construct_n);
    testSuite.addTest(
        algorithms, "ranges::uninitialized_value_construct", test_uninitialized_value_construct);
    testSuite.addTest(
        algorithms, "ranges::uninitialized_value_construct_n", test_uninitialized_value_construct_n);
    testSuite.addTest(algorithms, "ranges::destroy", test_destroy);
    testSuite.addTest(algorithms, "ranges::destroy_n", test_destroy_n);
    testSuite.addTest(algorithms, "ranges::destroy_at", test_destroy_at);
    testSuite.addTest(algorithms, "ranges::construct_at", test_construct_at);

    testSuite.run();

    {
        /// [make_pipeable use]
        RAH2_STD::vector<int> vec{0, 1, 2, 2, 3};
        assert((vec | test_count(2)) == 2);
        /// [make_pipeable use]
    }

    // *********************************** views **************************************************

    {
        /// [irange]
        RAH2_STD::vector<int> result;
        for (int i : RAH2_NS::views::irange(10, 19, 2))
            result.push_back(i);
        assert(result == RAH2_STD::vector<int>({10, 12, 14, 16, 18}));
        /// [irange]
        STATIC_ASSERT(
            (RAH2_NS::ranges::random_access_range<decltype(RAH2_NS::views::irange(10, 19, 2))>));
    }

    {
        RAH2_STD::vector<int> result;
        for (int i : RAH2_NS::views::irange(-5, 5, 2))
            result.push_back(i);
        assert(result == RAH2_STD::vector<int>({-5, -3, -1, 1, 3}));
    }

    {
        RAH2_STD::vector<int> result;
        for (int i : RAH2_NS::views::irange(-15, -6, 2))
            result.push_back(i);
        assert(result == RAH2_STD::vector<int>({-15, -13, -11, -9, -7}));
    }

    {
        /// [for_each]
        auto createRange = [](size_t i)
        {
            return RAH2_NS::views::repeat(static_cast<char>('a' + i)) | RAH2_NS::views::take(i);
        };
        auto range = RAH2_NS::views::for_each(RAH2_NS::views::iota(0llu, 5llu), createRange);
        RAH2_STD::string result;
        RAH2_NS::ranges::copy(range, RAH2_NS::back_inserter(result));
        assert(result == "bccdddeeee");
        /// [for_each]
    }

    {
#if (defined(__GNUC__) && !defined(__clang__))
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmaybe-uninitialized"
#endif
        /// [for_each_pipeable]
        auto range =
            RAH2_NS::views::iota(0, 3)
            | RAH2_NS::views::for_each(
                [&](int z)
                {
                    return RAH2_NS::views::iota(3, 6)
                           | RAH2_NS::views::for_each(
                               [&, z](int y)
                               {
                                   return RAH2_NS::views::iota(6, 9)
                                          | RAH2_NS::views::for_each(
                                              [&, y, z](int x)
                                              { return RAH2_NS::views::single(x + y * 3 + z * 9); });
                               });
                });
        assert(RAH2_NS::ranges::equal(range, RAH2_NS::views::iota(15, 42)));
        /// [for_each_pipeable]
    }

    {
        size_t count = 0;
        size_t count2 = 0;
        size_t count3 = 0;
        auto range =
            RAH2_NS::views::iota(0, 3)
            | RAH2_NS::views::for_each(
                [&](int z)
                {
                    ++count;
                    return RAH2_NS::views::iota(3, 6)
                           | RAH2_NS::views::for_each(
                               [&, z](int y)
                               {
                                   ++count2;
                                   return RAH2_NS::views::iota(6, 9)
                                          | RAH2_NS::views::for_each(
                                              [&, y, z](int x)
                                              {
                                                  ++count3;
                                                  return RAH2_NS::views::single(x + y * 3 + z * 9);
                                              });
                               });
                });

        assert(RAH2_NS::ranges::equal(range, RAH2_NS::views::iota(15, 42)));
        assert(count == 3);
        assert(count2 == 9);
        assert(count3 == 27);
#if (defined(__GNUC__) && !defined(__clang__))
#pragma GCC diagnostic pop
#endif
    }

    {
        size_t xSize = 2;
        size_t ySize = 3;
        auto xyIndexes = [=](size_t y)
        {
            return RAH2_NS::views::zip(RAH2_NS::views::repeat(y), RAH2_NS::views::iota(0llu, xSize));
        };
        auto range = RAH2_NS::views::iota(0llu, ySize) | RAH2_NS::views::for_each(xyIndexes);
        RAH2_STD::vector<RAH2_STD::tuple<size_t, size_t>> result;
        RAH2_NS::ranges::copy(range, RAH2_NS::back_inserter(result));
        assert(
            result
            == (RAH2_STD::vector<RAH2_STD::tuple<size_t, size_t>>{
                {0llu, 0llu}, {0llu, 1llu}, {1llu, 0llu}, {1llu, 1llu}, {2llu, 0llu}, {2llu, 1llu}}));

        size_t zSize = 4;
        auto xyzIndexes = [=](size_t z)
        {
            return RAH2_NS::views::zip(
                RAH2_NS::views::repeat(z),
                RAH2_NS::views::iota(0llu, ySize) | RAH2_NS::views::for_each(xyIndexes));
        };
        auto flattenTuple = [](auto&& z_yx)
        {
            using namespace RAH2_STD;
            return RAH2_STD::make_tuple(get<0>(z_yx), get<0>(get<1>(z_yx)), get<1>(get<1>(z_yx)));
        };
        auto rangeZYX = RAH2_NS::views::iota(0llu, zSize) | RAH2_NS::views::for_each(xyzIndexes)
                        | RAH2_NS::views::transform(flattenTuple);
        RAH2_STD::vector<RAH2_STD::tuple<size_t, size_t, size_t>> resultZYX;
        RAH2_NS::ranges::copy(rangeZYX, RAH2_NS::back_inserter(resultZYX));
        assert(
            resultZYX
            == (RAH2_STD::vector<RAH2_STD::tuple<size_t, size_t, size_t>>{
                {0lu, 0lu, 0lu}, {0lu, 0lu, 1lu}, {0lu, 1lu, 0lu}, {0lu, 1lu, 1lu}, {0lu, 2lu, 0lu},
                {0lu, 2lu, 1lu}, {1lu, 0lu, 0lu}, {1lu, 0lu, 1lu}, {1lu, 1lu, 0lu}, {1lu, 1lu, 1lu},
                {1lu, 2lu, 0lu}, {1lu, 2lu, 1lu}, {2lu, 0lu, 0lu}, {2lu, 0lu, 1lu}, {2lu, 1lu, 0lu},
                {2lu, 1lu, 1lu}, {2lu, 2lu, 0lu}, {2lu, 2lu, 1lu}, {3lu, 0lu, 0lu}, {3lu, 0lu, 1lu},
                {3lu, 1lu, 0lu}, {3lu, 1lu, 1lu}, {3lu, 2lu, 0lu}, {3lu, 2lu, 1lu}}));
    }

    {
        RAH2_STD::vector<int> in{0, 1, 2};
        auto range = RAH2_NS::views::drop(in, 6);
        RAH2_STD::vector<int> out;
        RAH2_STD::copy(
            RAH2_NS::ranges::begin(range), RAH2_NS::ranges::end(range), RAH2_STD::back_inserter(out));
        assert(out.empty());
    }

    {
        /// [unbounded]
        RAH2_STD::vector<int> in{0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
        auto range = RAH2_NS::views::unbounded(in.begin());
        RAH2_STD::vector<int> out;
        RAH2_STD::copy_n(RAH2_NS::ranges::begin(range), 5, RAH2_STD::back_inserter(out));
        assert(out == RAH2_STD::vector<int>({0, 1, 2, 3, 4}));
        /// [unbounded]
    }

    {
        RAH2_STD::vector<int> in{0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
        auto range = RAH2_NS::views::unbounded(in.begin()) | RAH2_NS::views::slice(0, 5);
        RAH2_STD::vector<int> out;
        RAH2_NS::ranges::copy(range, RAH2_NS::back_inserter(out));
        assert(out == RAH2_STD::vector<int>({0, 1, 2, 3, 4}));
    }

    {
        int in[] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
        auto range = RAH2_NS::views::unbounded(static_cast<int const*>(RAH2_STD::begin(in)))
                     | RAH2_NS::views::slice(0, 5);
        RAH2_STD::vector<int> out;
        RAH2_NS::ranges::copy(range, RAH2_NS::back_inserter(out));
        assert(out == RAH2_STD::vector<int>({0, 1, 2, 3, 4}));
    }

    {
        // Pass a rvalue container to a views is possible thought a owning_view
        auto getVec = []
        {
            return RAH2_STD::vector<int>{0, 1, 2, 3};
        };
        RAH2_STD::vector<int> result;
        for (int i : RAH2_NS::views::reverse(getVec()))
            result.push_back(i);
        assert(result == RAH2_STD::vector<int>({3, 2, 1, 0}));
    }

    {
        auto range = RAH2_NS::views::generate_n(5, []() { return rand(); })
                     | RAH2_NS::views::filter([](auto&& val) { return val % 2 == 0; });
        RAH2_STD::vector<int> result;
        for (int i : range | RAH2_NS::views::common)
            result.push_back(i);
    }
    {
        RAH2_STD::vector<int> vec_01234{0, 1, 2, 3, 4};
        RAH2_STD::vector<int> result;
        for (int i : RAH2_NS::views::filter(vec_01234, &is_odd))
            result.push_back(i);
        assert(result == RAH2_STD::vector<int>({0, 2, 4}));
    }
    {
        enum class Tutu
        {
            a,
            b,
            c,
            d,
            e
        };
        RAH2_STD::vector<Tutu> vec_01234{Tutu::a, Tutu::b, Tutu::c, Tutu::d, Tutu::e};
        RAH2_STD::vector<Tutu> result;
        for (Tutu i : RAH2_NS::views::filter(vec_01234, [](Tutu a) { return a != Tutu::c; }))
            result.push_back(i);
        assert(result == RAH2_STD::vector<Tutu>({Tutu::a, Tutu::b, Tutu::d, Tutu::e}));
    }

    {
        int vec_01234[] = {0, 1, 2, 3, 4};
        RAH2_STD::vector<int> result;
        for (int i : RAH2_NS::views::filter(vec_01234, [](auto a) { return a % 2 == 0; }))
            result.push_back(i);
        assert(result == RAH2_STD::vector<int>({0, 2, 4}));
    }
    {
        RAH2_STD::vector<RAH2_STD::vector<int>> vec_01234 = {
            {0},
            {1},
            {2},
            {3},
            {4},
        };
        RAH2_STD::vector<bool> vec_bool = {
            true,
            true,
            true,
            true,
            true,
        };
        RAH2_STD::vector<RAH2_STD::vector<int>> result;
        for (auto&& i : RAH2_NS::views::zip(vec_01234, vec_bool)
                            | RAH2_NS::views::filter(
                                [](auto&& a) { return RAH2_STD::get<0>(a).front() % 2 == 0; })
                            | RAH2_NS::views::common)
            result.push_back(RAH2_STD::get<0>(i));
        assert(result == (RAH2_STD::vector<RAH2_STD::vector<int>>{{0}, {2}, {4}}));
        assert(vec_01234 == (RAH2_STD::vector<RAH2_STD::vector<int>>{{0}, {1}, {2}, {3}, {4}}));
    }
    {
        /// [filter_pipeable]
        RAH2_STD::vector<int> vec_01234{0, 1, 2, 3, 4};
        RAH2_STD::vector<int> result;
        for (int i : vec_01234 | RAH2_NS::views::filter([](auto a) { return a % 2 == 0; }))
            result.push_back(i);
        assert(result == RAH2_STD::vector<int>({0, 2, 4}));
        /// [filter_pipeable]
    }

    {
        // test filter with the first elements filtered
        auto range1 = RAH2_NS::views::iota(1, 10)
                      | RAH2_NS::views::filter([](auto&& val) { return val % 2 == 0; });
        assert(RAH2_NS::ranges::none_of(range1, [](auto v) { return (v % 2) == 1; }));

        // test generate + filter
        auto range2 = RAH2_NS::views::generate_n(100, []() { return rand(); })
                      | RAH2_NS::views::filter([](auto&& val) { return val % 2 == 0; });

        assert(RAH2_NS::ranges::none_of(range2, [](auto v) { return (v % 2) == 1; }));

        // Can create some compilation issue about lambda copy
        auto range3 =
            RAH2_NS::views::iota(0, 5)
            | RAH2_NS::views::for_each(
                [](auto)
                {
                    return RAH2_NS::views::generate_n(5, []() { return rand(); })
                           | RAH2_NS::views::filter([](auto&& val) { return val % 2 == 0; });
                });
        assert(RAH2_NS::ranges::none_of(range3, [](auto v) { return (v % 2) == 1; }));
    }

    // *********************************** algos **************************************************

    {
        /// [rah2::to_container_pipeable]
        RAH2_STD::vector<RAH2_STD::pair<int, char>> in1{{4, 'a'}, {5, 'b'}, {6, 'c'}, {7, 'd'}};
        RAH2_STD::map<int, char> map_4a_5b_6c_7d =
            in1 | RAH2_NS::ranges::to<RAH2_STD::map<int, char>>();
        assert(map_4a_5b_6c_7d == (RAH2_STD::map<int, char>{{4, 'a'}, {5, 'b'}, {6, 'c'}, {7, 'd'}}));

        RAH2_STD::list<int> in2{4, 5, 6, 7};
        RAH2_STD::vector<int> out = in2 | RAH2_NS::ranges::to<RAH2_STD::vector<int>>();
        assert(out == (RAH2_STD::vector<int>{4, 5, 6, 7}));
        /// [rah2::to_container_pipeable]
    }
    {
        /// [rah2::to]
        RAH2_STD::vector<RAH2_STD::pair<int, char>> in1{{4, 'a'}, {5, 'b'}, {6, 'c'}, {7, 'd'}};
        auto map_4a_5b_6c_7d = RAH2_NS::ranges::to<RAH2_STD::map<int, char>>(in1);
        assert(map_4a_5b_6c_7d == (RAH2_STD::map<int, char>{{4, 'a'}, {5, 'b'}, {6, 'c'}, {7, 'd'}}));

        RAH2_STD::list<int> in2{4, 5, 6, 7};
        auto out = RAH2_NS::ranges::to<RAH2_STD::vector<int>>(in2);
        assert(out == (RAH2_STD::vector<int>{4, 5, 6, 7}));
        /// [rah2::to]
    }

    {
        /// [rah2::size]
        RAH2_STD::vector<int> vec3{1, 2, 3};
        assert(RAH2_NS::ranges::size(vec3) == 3);
        /// [rah2::size]
    }

    /// [rah2::empty]
    assert(not(RAH2_NS::ranges::empty(RAH2_STD::vector<int>{1, 2, 3})));
    assert(RAH2_NS::ranges::empty(RAH2_STD::vector<int>()));
    /// [rah2::empty]

    // ********************************* test return ref and non-ref ******************************

    using namespace RAH2_NS;
    using namespace views;
    using namespace RAH2_STD;

    struct Elt
    {
        int member;
        bool operator==(Elt const& elt) const
        {
            return member == elt.member;
        }
        bool operator!=(Elt const& elt) const
        {
            return member != elt.member;
        }
    };

    // Test return reference

    {
        RAH2_STD::vector<Elt> vec = {{0}, {1}, {2}, {3}, {4}};
        auto& r = vec;
        for (auto iter = RAH2_STD::begin(r), end_iter = RAH2_STD::end(r); iter != end_iter; ++iter)
        {
            iter->member = 42; // Check for mutability
        }
        EQUAL_RANGE(r, (il<Elt>({{42}, {42}, {42}, {42}, {42}})));
        for (auto&& elt : r)
        {
            static_assert(
                RAH2_NS::is_reference_v<decltype(elt)>, "elt is expected to be a reference");
            elt.member = 78; // Check for mutability
        }
        EQUAL_RANGE(r, (il<Elt>({{78}, {78}, {78}, {78}, {78}})));
    }
    {
        RAH2_STD::vector<int> vec(5);
        for (int& i : vec | RAH2_NS::views::transform([](int& i) -> int& { return i; }))
            i = 42; // Check for mutability
        EQUAL_RANGE(vec, (il<int>({42, 42, 42, 42, 42})));
    }

    // Test return non-reference
    {
        RAH2_STD::vector<int> constVect{0, 1, 2, 3};
        EQUAL_RANGE(
            constVect | RAH2_NS::views::transform([](auto a) { return a * 2; }),
            il<int>({0, 2, 4, 6}));

        RAH2_STD::vector<Elt> vec = {{1}};
        auto r_copy = vec | RAH2_NS::views::transform([](auto a) { return Elt{a.member + 1}; });
        for (auto iter = RAH2_NS::ranges::begin(r_copy), end_iter = RAH2_NS::ranges::end(r_copy);
             iter != end_iter;
             ++iter)
        {
            // Not possible since *iter is a rvalue (can't take the pointer of a rvalue)
            // assert(iter->member == 2); // Check for mutability
            // assert((*iter).member == 2); // Check for mutability
            static_assert(
                RAH2_NS::is_rvalue_reference_v<decltype(*iter)>
                    || (not RAH2_NS::is_reference_v<decltype(*iter)>),
                "*iter is not expected to be a reference");
        }
        for (auto&& elt : r_copy)
        {
            assert(elt.member == 2); // Check for mutability
            static_assert(
                RAH2_NS::is_rvalue_reference_v<decltype(elt)>
                    || (not RAH2_NS::is_reference_v<decltype(elt)>),
                "elt is not expected to be a reference");
        }
        auto r_ref = vec | RAH2_NS::views::transform([](auto a) { return a.member; });
        for (auto iter = RAH2_NS::ranges::begin(r_ref), end_iter = RAH2_NS::ranges::end(r_ref);
             iter != end_iter;
             ++iter)
        {
            assert(*iter == 1); // Check for mutability
            static_assert(
                RAH2_NS::is_rvalue_reference_v<decltype(*iter)>
                    || (not RAH2_NS::is_reference_v<decltype(*iter)>),
                "*iter is not expected to be a reference");
        }
        for (auto&& elt : r_ref)
        {
            assert(elt == 1); // Check for mutability
            static_assert(
                RAH2_NS::is_rvalue_reference_v<decltype(elt)>
                    || (not RAH2_NS::is_reference_v<decltype(elt)>),
                "elt is not expected to be a reference");
        }
    }

    // **************************** various compilation test **************************************

    {
        auto genRange = [](size_t i)
        {
            return zip(repeat(i), RAH2_NS::views::iota(0llu, 3llu));
        };
        auto globalRange =
            RAH2_NS::views::iota(0llu, 4llu) | RAH2_NS::views::transform(genRange) | join;

        EQUAL_RANGE(
            globalRange,
            (il<RAH2_STD::tuple<size_t, size_t>>{
                {0lu, 0lu},
                {0lu, 1lu},
                {0lu, 2lu},
                {1lu, 0lu},
                {1lu, 1lu},
                {1lu, 2lu},
                {2lu, 0lu},
                {2lu, 1lu},
                {2lu, 2lu},
                {3lu, 0lu},
                {3lu, 1lu},
                {3lu, 2lu}}));
    }

    EQUAL_RANGE(
        (RAH2_NS::views::iota(0, 3) | RAH2_NS::views::transform([](auto i) { return i * 2; })
         | enumerate),
        (il<RAH2_STD::pair<intptr_t, int>>{{0, 0}, {1, 2}, {2, 4}}));

    RAH2_STD::vector<char> vec_abcd{'a', 'b', 'c', 'd'};
    EQUAL_RANGE(
        (vec_abcd | RAH2_NS::views::transform([](char i) { return static_cast<char>(i + 1); })
         | enumerate),
        (il<RAH2_STD::pair<intptr_t, char>>{{0, 'b'}, {1, 'c'}, {2, 'd'}, {3, 'e'}}));

    // TODO : Make Zip bidirectional when possible
    //EQUAL_RANGE(
    //    (iota(0, 3000, 3) | transform([](auto i) { return i * 2; }) | enumerate | slice(10, 13)),
    //    (il<RAH2_STD::pair<size_t, int>>{ { 10, 60 }, { 11, 66 }, { 12, 72 } }));

    EQUAL_RANGE(
        (zip(vec_abcd, RAH2_NS::views::iota(0, 4))),
        (il<RAH2_STD::tuple<char, int>>{{'a', 0}, {'b', 1}, {'c', 2}, {'d', 3}}));

    EQUAL_RANGE(
        (RAH2_NS::views::iota(0, 100) | slice(0, 20) | stride(3)), (il<int>{0, 3, 6, 9, 12, 15, 18}));

    EQUAL_RANGE(
        (RAH2_NS::views::iota(10, 15) | RAH2_NS::views::reverse), (il<int>{14, 13, 12, 11, 10}));

    EQUAL_RANGE(
        (RAH2_NS::views::iota(0, 100) | slice(10, 15) | RAH2_NS::views::reverse),
        (il<int>{14, 13, 12, 11, 10}));

    //EQUAL_RANGE(
    //    (iota(10, 15) | enumerate | reverse),
    //    (il<RAH2_STD::tuple<size_t, int>>{ { 4, 14 }, { 3, 13 }, { 2, 12 }, { 1, 11 }, { 0, 10 } }));

    //EQUAL_RANGE(
    //    (iota(0, 100) | enumerate | slice(10, 15)),
    //    (il<RAH2_STD::tuple<size_t, int>>{ { 10, 10 }, { 11, 11 }, { 12, 12 }, { 13, 13 }, { 14, 14 } }));

    //EQUAL_RANGE(
    //    (iota(0, 100) | enumerate | slice(10, 15) | reverse),
    //    (il<RAH2_STD::tuple<size_t, int>>{ { 14, 14 }, { 13, 13 }, { 12, 12 }, { 11, 11 }, { 10, 10 } }));

    // iota(0, 10) | filter([](int i) { return i % 2 == 0; }) | RAH2_NS::to<RAH2_STD::vector<int>>();

    RAH2_NS::views::iota(0, 10) | filter([](int i) { return i % 2 == 0; }) | slice(1, 9)
        | RAH2_NS::ranges::to<RAH2_STD::vector<int>>();

    {
        // Test creation of a custom iterator
        auto gen = customGenerate();
        RAH2_STD::vector<int> gen_copy;
        RAH2_STD::copy(
            RAH2_NS::ranges::begin(gen), RAH2_NS::ranges::end(gen), RAH2_STD::back_inserter(gen_copy));
        EQUAL_RANGE(gen_copy, RAH2_STD::vector<int>({1, 2, 4, 8}));
    }

    {
        using namespace RAH2_NS;
        using namespace RAH2_NS::views;
        int const width = 5;
        int const height = 6;
        int const start = 8;
        int startX = start % width;
        int startY = start / width;
        int const end = 22;
        int endX = end % width;
        int endY = end / width;
        auto getRangeX = [=](int y)
        {
            if (y == startY)
                return RAH2_STD::make_tuple(y, RAH2_NS::views::iota(startX, width));
            else if (y == endY)
                return RAH2_STD::make_tuple(y, RAH2_NS::views::iota(0, endX));
            else
                return RAH2_STD::make_tuple(y, RAH2_NS::views::iota(0, width));
        };

        RAH2_STD::vector<RAH2_STD::atomic<int>> test_(static_cast<int>(width * height));

        auto updateRaw = [&](auto&& y_xRange)
        {
            auto y = RAH2_STD::get<0>(y_xRange);
            auto xRange = RAH2_STD::get<1>(y_xRange);

            for (auto x : xRange)
                ++test_[x + y * width];
        };

        for (auto ySelector : RAH2_NS::views::iota(0, 3))
        {
            auto rng = RAH2_NS::views::irange(startY + ySelector, endY + 1, 3)
                       | RAH2_NS::views::transform(getRangeX);
            RAH2_NS::ranges::for_each(rng, updateRaw);
        }

        assert(RAH2_NS::ranges::all_of(test_ | slice(0, start), [](auto&& val) { return val == 0; }));
        assert(RAH2_NS::ranges::all_of(
            test_ | slice(start, end), [](auto&& val) { return val == 1; }));
        assert(RAH2_NS::ranges::all_of(
            test_ | slice(end, test_.size()), [](auto&& val) { return val == 0; }));
    }

    testSuite.report();
    return testSuite.all_success ? EXIT_SUCCESS : EXIT_FAILURE;
}
catch (std::exception& ex)
{
    printf("Exception : %s - %s\n", typeid(ex).name(), ex.what());
    return EXIT_FAILURE;
}
catch (...)
{
    printf("Unknown exception occurred\n");
    return EXIT_FAILURE;
}
