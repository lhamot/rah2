//
// Copyright (c) 2019 Loïc HAMOT
//
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//
#include <rah2/ranges.hpp>

#include <iostream>
#include <vector>
#include <map>
#include <list>
#include <ciso646>
#include <sstream>
#include <random>
#include <atomic>
#include <array>

#include <iomanip>

#include "test_helpers.hpp"

bool is_odd(int val)
{
    return val % 2 == 0;
}

/// [make_pipeable create]
auto test_count(int i)
{
    return rah2::make_pipeable([=](auto&& range) { return std::count(begin(range), end(range), i); });
}
/// [make_pipeable create]

// Test creation of a custom iterator
struct CustomGenerator
    : rah2::iterator_facade<CustomGenerator, rah2::default_sentinel, int, RAH2_STD::forward_iterator_tag>
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
    return rah2::subrange<CustomGenerator, CustomGenerator>{};
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
void test_adjacent_view();
void test_common_view();
void test_reverse_view();
void test_transform_view();
void test_adjacent_transform();
void test_slide_view();
void test_chunk_view();
void test_stride_view();
void test_ref_view();

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

void test_range_concepts()
{
    testSuite.test_case("on non-range");
    STATIC_ASSERT(!rah2::range<int>);
    STATIC_ASSERT(!rah2::borrowed_range<int>);
    STATIC_ASSERT(!rah2::sized_range<int>);
    STATIC_ASSERT(!rah2::view<int>);
    STATIC_ASSERT(!rah2::input_range<int>);
    STATIC_ASSERT((!rah2::output_range<int, int>));
    STATIC_ASSERT(!rah2::forward_range<int>);
    STATIC_ASSERT(!rah2::bidirectional_range<int>);
    STATIC_ASSERT(!rah2::random_access_range<int>);
    STATIC_ASSERT(!rah2::contiguous_range<int>);
    STATIC_ASSERT(!rah2::common_range<int>);
    STATIC_ASSERT(!rah2::viewable_range<int>);
    STATIC_ASSERT(!rah2::constant_range<int>);
}

int main()
{
    // std::cout.imbue(std::locale("en_EN"));

    testSuite.addTest("Range_concepts", "*", test_range_concepts);

    // Range_factories
    char const* range_factories = "Range_factories";
    testSuite.addTest(range_factories, "ranges::empty_view", test_empty_view);
    testSuite.addTest(range_factories, "ranges::single_view", test_single_view);
    testSuite.addTest(range_factories, "ranges::iota_view", test_iota_view);
    testSuite.addTest(range_factories, "ranges::basic_istream_view", test_istream_view);
    testSuite.addTest(range_factories, "ranges::repeat_view", test_repeat_view);
    // testSuite.addTest("ranges::cartesian_product_view", test_cartesian_product_view);

    // Range_Adaptors
    char const* range_adaptors = "Range_Adaptors";
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
    testSuite.addTest(range_adaptors, "ranges::values_view", test_elements_view);
    testSuite.addTest(range_adaptors, "ranges::keys_view", test_elements_view);
    testSuite.addTest(range_adaptors, "ranges::zip_view", test_zip_view);
    testSuite.addTest(range_adaptors, "ranges::adjacent_view", test_adjacent_view);
    testSuite.addTest(range_adaptors, "ranges::common_view", test_common_view);
    testSuite.addTest(range_adaptors, "ranges::reverse_view", test_reverse_view);
    testSuite.addTest(range_adaptors, "ranges::zip_transform_view", test_transform_view);
    testSuite.addTest(range_adaptors, "ranges::adjacent_transform_view", test_adjacent_transform);
    testSuite.addTest(range_adaptors, "ranges::slide_view", test_slide_view);
    testSuite.addTest(range_adaptors, "ranges::chunk_view", test_chunk_view);
    testSuite.addTest(range_adaptors, "ranges::stride_view", test_stride_view);
    testSuite.addTest(range_adaptors, "ranges::ref_view", test_ref_view);

    char const* algorithms = "Algorithms";
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
        std::vector<int> vec{0, 1, 2, 2, 3};
        toto(vec);
        toto({0, 1, 2, 2, 3});
    }
    {
        /// [make_pipeable use]
        std::vector<int> vec{0, 1, 2, 2, 3};
        assert((vec | test_count(2)) == 2);
        /// [make_pipeable use]
    }

    // *********************************** views **************************************************

    {
        /// [irange]
        std::vector<int> result;
        for (int i : rah2::views::irange(10, 19, 2))
            result.push_back(i);
        assert(result == std::vector<int>({10, 12, 14, 16, 18}));
        /// [irange]
        STATIC_ASSERT((rah2::random_access_range<decltype(rah2::views::irange(10, 19, 2))>));
    }

    {
        std::vector<int> result;
        for (int i : rah2::views::irange(-5, 5, 2))
            result.push_back(i);
        assert(result == std::vector<int>({-5, -3, -1, 1, 3}));
    }

    {
        std::vector<int> result;
        for (int i : rah2::views::irange(-15, -6, 2))
            result.push_back(i);
        assert(result == std::vector<int>({-15, -13, -11, -9, -7}));
    }

    {
        /// [for_each]
        auto createRange = [](int i)
        {
            return rah2::views::repeat(char('a' + i)) | rah2::views::take(i);
        };
        auto range = rah2::views::for_each(rah2::views::iota(0, 5), createRange);
        std::string result;
        rah2::copy(range, std::back_inserter(result));
        assert(result == "bccdddeeee");
        /// [for_each]
    }

    {
        /// [for_each_pipeable]
        auto range =
            rah2::views::iota(0, 3)
            | rah2::views::for_each(
                [&](int z)
                {
                    return rah2::views::iota(3, 6)
                           | rah2::views::for_each(
                               [&, z](int y)
                               {
                                   return rah2::views::iota(6, 9)
                                          | rah2::views::for_each(
                                              [&, y, z](int x)
                                              { return rah2::views::single(x + y * 3 + z * 9); });
                               });
                });
        assert(equal(range, rah2::views::iota(15, 42)));
        /// [for_each_pipeable]
    }

    {
        size_t count = 0;
        size_t count2 = 0;
        size_t count3 = 0;
        auto range =
            rah2::views::iota(0, 3)
            | rah2::views::for_each(
                [&](int z)
                {
                    ++count;
                    return rah2::views::iota(3, 6)
                           | rah2::views::for_each(
                               [&, z](int y)
                               {
                                   ++count2;
                                   return rah2::views::iota(6, 9)
                                          | rah2::views::for_each(
                                              [&, y, z](int x)
                                              {
                                                  ++count3;
                                                  return rah2::views::single(x + y * 3 + z * 9);
                                              });
                               });
                });

        assert(equal(range, rah2::views::iota(15, 42)));
        assert(count == 3);
        assert(count2 == 9);
        assert(count3 == 27);
    }

    {
        size_t xSize = 2;
        size_t ySize = 3;
        auto xyIndexes = [=](size_t y)
        {
            return rah2::views::zip(rah2::views::repeat(y), rah2::views::iota<size_t>(0, xSize));
        };
        auto range = rah2::views::iota<size_t>(0, ySize) | rah2::views::for_each(xyIndexes);
        std::vector<std::tuple<size_t, size_t>> result;
        rah2::copy(range, std::back_inserter(result));
        assert(
            result
            == (std::vector<std::tuple<size_t, size_t>>{
                {0, 0}, {0, 1}, {1, 0}, {1, 1}, {2, 0}, {2, 1}}));

        size_t zSize = 4;
        auto xyzIndexes = [=](size_t z)
        {
            return rah2::views::zip(
                rah2::views::repeat(z),
                rah2::views::iota<size_t>(0, ySize) | rah2::views::for_each(xyIndexes));
        };
        auto flattenTuple = [](auto&& z_yx)
        {
            using namespace std;
            return std::make_tuple(get<0>(z_yx), get<0>(get<1>(z_yx)), get<1>(get<1>(z_yx)));
        };
        auto rangeZYX = rah2::views::iota<size_t>(0, zSize) | rah2::views::for_each(xyzIndexes)
                        | rah2::views::transform(flattenTuple);
        std::vector<std::tuple<size_t, size_t, size_t>> resultZYX;
        rah2::copy(rangeZYX, std::back_inserter(resultZYX));
        assert(
            resultZYX
            == (std::vector<std::tuple<size_t, size_t, size_t>>{
                {0, 0, 0}, {0, 0, 1}, {0, 1, 0}, {0, 1, 1}, {0, 2, 0}, {0, 2, 1},
                {1, 0, 0}, {1, 0, 1}, {1, 1, 0}, {1, 1, 1}, {1, 2, 0}, {1, 2, 1},
                {2, 0, 0}, {2, 0, 1}, {2, 1, 0}, {2, 1, 1}, {2, 2, 0}, {2, 2, 1},
                {3, 0, 0}, {3, 0, 1}, {3, 1, 0}, {3, 1, 1}, {3, 2, 0}, {3, 2, 1}}));
    }

    {
        /// [generate]
        int y = 1;
        auto gen = rah2::views::generate(
            [&y]() mutable
            {
                auto prev = y;
                y *= 2;
                return prev;
            });
        std::vector<int> gen_copy;
        std::copy_n(begin(gen), 4, std::back_inserter(gen_copy));
        assert(gen_copy == std::vector<int>({1, 2, 4, 8}));
        /// [generate]
        static_assert(rah2::input_range<decltype(gen)>, "");
        static_assert(
            RAH2_NAMESPACE::is_same_v<rah2::range_iter_categ_t<decltype(gen)>, std::input_iterator_tag>,
            "");
        static_assert(not rah2::forward_range<decltype(gen)>, "");
        static_assert(not rah2::common_range<decltype(gen)>, "");
    }
    {
        /// [generate_n]
        std::vector<int> result;
        int y = 1;
        auto gen = rah2::views::generate_n(
            4,
            [&y]() mutable
            {
                auto prev = y;
                y *= 2;
                return prev;
            });
        auto i = rah2::begin(gen);
        auto e = rah2::end(gen);
        for (; i != e; ++i)
            result.push_back(*i);
        assert(result == std::vector<int>({1, 2, 4, 8}));
        /// [generate_n]
        static_assert(rah2::input_range<decltype(gen)>, "");
        static_assert(
            RAH2_NAMESPACE::is_same_v<rah2::range_iter_categ_t<decltype(gen)>, std::input_iterator_tag>,
            "");
        static_assert(not rah2::forward_range<decltype(gen)>, "");
        static_assert(not rah2::common_range<decltype(gen)>, "");
    }

    {
        /// [cycle]
        std::vector<int> in{0, 1, 2};
        auto cy = rah2::views::cycle(in);
        std::vector<int> out;
        std::copy_n(cy.begin(), 8, std::back_inserter(out));
        assert(out == std::vector<int>({0, 1, 2, 0, 1, 2, 0, 1}));
        /// [cycle]
        STATIC_ASSERT((rah2::bidirectional_range<decltype(cy)>));
        STATIC_ASSERT((not rah2::random_access_range<decltype(cy)>));
    }
    { // Cycle + input/sentinel => input/sentinel
        auto cyInputSent =
            rah2::views::cycle(make_test_view<Sentinel, std::input_iterator_tag, false>());
        STATIC_ASSERT(is_input_not_common<decltype(cyInputSent)>);
    }
    {
        auto cyForwardSent =
            rah2::views::cycle(make_test_view<Sentinel, std::forward_iterator_tag, false>());
        STATIC_ASSERT(is_forward_not_common<decltype(cyForwardSent)>);
    }
    {
        auto cyForwardCommon =
            rah2::views::cycle(make_test_view<Common, std::forward_iterator_tag, false>());
        STATIC_ASSERT(is_forward_not_common<decltype(cyForwardCommon)>);
    }
    { // Cycle can't be bidir if we can't assign sentinel to iterator
        auto cyBidirSent =
            rah2::views::cycle(make_test_view<Sentinel, std::bidirectional_iterator_tag, false>());
        STATIC_ASSERT(is_forward_not_common<decltype(cyBidirSent)>);
    }
    {
        auto cyBidirCommon =
            rah2::views::cycle(make_test_view<Common, std::bidirectional_iterator_tag, false>());
        STATIC_ASSERT(is_bidirectional_not_common<decltype(cyBidirCommon)>);
    }
    { // Cycle can't be bidir if we can't assign sentinel to iterator
        auto cyRandomSent =
            rah2::views::cycle(make_test_view<Sentinel, std::random_access_iterator_tag, true>());
        STATIC_ASSERT(is_forward_not_common<decltype(cyRandomSent)>);
    }
    {
        auto cyRandomCommon =
            rah2::views::cycle(make_test_view<Common, std::random_access_iterator_tag, true>());
        STATIC_ASSERT(is_bidirectional_not_common<decltype(cyRandomCommon)>);
    }
    { // Cycle can't be bidir if we can't assign sentinel to iterator
        auto cyContiSent =
            rah2::views::cycle(make_test_view<Sentinel, rah2::contiguous_iterator_tag, true>());
        STATIC_ASSERT(is_forward_not_common<decltype(cyContiSent)>);
    }
    {
        auto cyContiCommon =
            rah2::views::cycle(make_test_view<Common, rah2::contiguous_iterator_tag, true>());
        STATIC_ASSERT(is_bidirectional_not_common<decltype(cyContiCommon)>);
    }

    {
        /// [cycle_pipeable]
        std::vector<int> in{0, 1, 2};
        auto cy = in | rah2::views::cycle();
        std::vector<int> out;
        std::copy_n(cy.begin(), 8, std::back_inserter(out));
        assert(out == std::vector<int>({0, 1, 2, 0, 1, 2, 0, 1}));
        /// [cycle_pipeable]
    }

    {
        std::vector<int> in{0, 1, 2};
        auto cy = rah2::views::cycle(in) | rah2::views::take(8);
        std::vector<int> out;
        // static_assert(RAH2_NAMESPACE::range<decltype(std::back_inserter(out))>, "dkjh");
        rah2::copy(cy, std::back_inserter(out));
        assert(out == std::vector<int>({0, 1, 2, 0, 1, 2, 0, 1}));
    }

    {
        std::vector<int> in{0, 1, 2};
        auto range = rah2::views::drop(in, 6);
        std::vector<int> out;
        std::copy(rah2::begin(range), rah2::end(range), std::back_inserter(out));
        assert(out.empty());
    }

    {
        /// [unbounded]
        std::vector<int> in{0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
        auto range = rah2::views::unbounded(in.begin());
        std::vector<int> out;
        std::copy_n(rah2::begin(range), 5, std::back_inserter(out));
        assert(out == std::vector<int>({0, 1, 2, 3, 4}));
        /// [unbounded]
    }

    {
        std::vector<int> in{0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
        auto range = rah2::views::unbounded(in.begin()) | rah2::views::slice(0, 5);
        std::vector<int> out;
        rah2::copy(range, std::back_inserter(out));
        assert(out == std::vector<int>({0, 1, 2, 3, 4}));
    }

    {
        int in[] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
        auto range =
            rah2::views::unbounded((int const* const)std::begin(in)) | rah2::views::slice(0, 5);
        std::vector<int> out;
        rah2::copy(range, std::back_inserter(out));
        assert(out == std::vector<int>({0, 1, 2, 3, 4}));
    }

    {
        /// [counted_pipeable]
        std::vector<int> in{0, 1, 2, 3, 4, 5};
        auto range = in | rah2::views::take(9);
        std::vector<int> out;
        auto a = rah2::begin(range);
        auto b = rah2::end(range);
        volatile auto dist = std::distance(a, b);
        (void)dist;
        std::copy(rah2::begin(range), rah2::end(range), std::back_inserter(out));
        assert(out == std::vector<int>({0, 1, 2, 3, 4, 5}));
        /// [counted_pipeable]
    }

    {
        /// [slice]
        std::vector<int> vec{0, 1, 2, 3, 4, 5, 6, 7};
        std::vector<int> result;
        for (int i : rah2::views::slice(vec, 2, 6))
            result.push_back(i);
        assert(result == std::vector<int>({2, 3, 4, 5}));
        std::vector<int> result2;
        for (int i : rah2::views::slice(vec, 2, 6))
            result2.push_back(i);
        assert(result2 == std::vector<int>({2, 3, 4, 5}));
        /// [slice]
    }
    {
        /// [slice_pipeable]
        std::vector<int> vec{0, 1, 2, 3, 4, 5, 6, 7};
        std::vector<int> result;
        for (int i : vec | rah2::views::slice(2, 6))
            result.push_back(i);
        assert(result == std::vector<int>({2, 3, 4, 5}));
        /// [slice_pipeable]
    }

    {
        // Pass a rvalue container to a views is possible thought a owning_view
        auto getVec = []
        {
            return std::vector<int>{0, 1, 2, 3};
        };
        std::vector<int> result;
        for (int i : rah2::views::reverse(getVec()))
            result.push_back(i);
        assert(result == std::vector<int>({3, 2, 1, 0}));
    }

    {
        auto range = rah2::views::generate_n(5, []() { return rand(); })
                     | rah2::views::filter([](auto&& val) { return val % 2 == 0; });
        std::vector<int> result;
        for (int i : range)
            result.push_back(i);
    }
    {
        std::vector<int> vec_01234{0, 1, 2, 3, 4};
        std::vector<int> result;
        for (int i : rah2::views::filter(vec_01234, &is_odd))
            result.push_back(i);
        assert(result == std::vector<int>({0, 2, 4}));
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
        std::vector<Tutu> vec_01234{Tutu::a, Tutu::b, Tutu::c, Tutu::d, Tutu::e};
        std::vector<Tutu> result;
        for (Tutu i : rah2::views::filter(vec_01234, [](Tutu a) { return a != Tutu::c; }))
            result.push_back(i);
        assert(result == std::vector<Tutu>({Tutu::a, Tutu::b, Tutu::d, Tutu::e}));
    }

    {
        int vec_01234[] = {0, 1, 2, 3, 4};
        std::vector<int> result;
        for (int i : rah2::views::filter(vec_01234, [](auto a) { return a % 2 == 0; }))
            result.push_back(i);
        assert(result == std::vector<int>({0, 2, 4}));
    }
    {
        std::vector<std::vector<int>> vec_01234 = {
            {0},
            {1},
            {2},
            {3},
            {4},
        };
        std::vector<bool> vec_bool = {
            true,
            true,
            true,
            true,
            true,
        };
        std::vector<std::vector<int>> result;
        for (auto&& i :
             rah2::views::zip(vec_01234, vec_bool)
                 | rah2::views::filter([](auto&& a) { return std::get<0>(a).front() % 2 == 0; })
                 | rah2::views::common())
            result.push_back(std::get<0>(i));
        assert(result == (std::vector<std::vector<int>>{{0}, {2}, {4}}));
        assert(vec_01234 == (std::vector<std::vector<int>>{{0}, {1}, {2}, {3}, {4}}));
    }
    {
        /// [filter_pipeable]
        std::vector<int> vec_01234{0, 1, 2, 3, 4};
        std::vector<int> result;
        for (int i : vec_01234 | rah2::views::filter([](auto a) { return a % 2 == 0; }))
            result.push_back(i);
        assert(result == std::vector<int>({0, 2, 4}));
        /// [filter_pipeable]
    }

    {
        // test filter with the first elements filtered
        auto range1 =
            rah2::views::iota(1, 10) | rah2::views::filter([](auto&& val) { return val % 2 == 0; });
        assert(rah2::none_of(range1, [](auto v) { return (v % 2) == 1; }));

        // test generate + filter
        auto range2 = rah2::views::generate_n(100, []() { return rand(); })
                      | rah2::views::filter([](auto&& val) { return val % 2 == 0; });

        assert(rah2::none_of(range2, [](auto v) { return (v % 2) == 1; }));

        // Can create some compilation issue about lambda copy
        auto range3 = rah2::views::iota(0, 5)
                      | rah2::views::for_each(
                          [](auto)
                          {
                              return rah2::views::generate_n(5, []() { return rand(); })
                                     | rah2::views::filter([](auto&& val) { return val % 2 == 0; });
                          });
        assert(rah2::none_of(range3, [](auto v) { return (v % 2) == 1; }));
    }

    {
        /// [concat]
        std::vector<int> inputA{0, 1, 2, 3};
        std::vector<int> inputB{4, 5, 6};
        std::vector<int> inputC{7, 8, 9, 10, 11};
        {
            std::vector<int> result;
            for (int i : rah2::views::concat(inputA))
                result.push_back(i);
            assert(result == std::vector<int>({0, 1, 2, 3}));
        }
        {
            std::vector<int> result;
            for (int i : rah2::views::concat(inputA, inputB))
                result.push_back(i);
            assert(result == std::vector<int>({0, 1, 2, 3, 4, 5, 6}));
        }
        {
            std::vector<int> result;
            for (int i : rah2::views::concat(inputA, inputB, inputC))
                result.push_back(i);
            assert(result == std::vector<int>({0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11}));
        }
        /// [concat]
    }
    {
        std::vector<int> inputA{};
        std::vector<int> inputB{1, 2, 3, 4};
        {
            std::vector<int> result;
            for (int i : rah2::views::concat(inputA, inputB))
                result.push_back(i);
            assert(result == std::vector<int>({1, 2, 3, 4}));
        }
        {
            std::vector<int> result;
            for (int i : rah2::views::concat(inputA, inputB))
                result.push_back(i);
            assert(result == std::vector<int>({1, 2, 3, 4}));
        }
        {
            std::vector<int> result;
            for (int i : rah2::views::concat(inputA, inputA))
                result.push_back(i);
            assert(result == std::vector<int>({}));
        }
    }

    {
        /// [views::set_difference]
        std::vector<int> in1 = {1, 2, 3, 4, 5, 6};
        std::vector<int> in2 = {2, 4, 6, 7, 8, 9, 10};
        std::vector<int> out;
        for (int val : rah2::views::set_difference(in1, in2))
            out.push_back(val);
        assert(out == std::vector<int>({1, 3, 5}));
        /// [views::set_difference]
    }

    {
        /// views::set_difference
        auto test_set_difference = [](std::vector<int> const& in1,
                                      std::vector<int> const& in2,
                                      std::vector<int> const& expected)
        {
            std::vector<int> out;
            for (int val : rah2::views::set_difference(in1, in2))
                out.push_back(val);
            assert(out == expected);
        };

        test_set_difference({}, {2, 4, 6, 7, 8, 9, 10}, {});
        test_set_difference({1, 2, 3, 4, 5, 6}, {}, {1, 2, 3, 4, 5, 6});
        test_set_difference({1, 2, 3, 4, 5, 6, 7}, {2, 4, 6}, {1, 3, 5, 7});
        test_set_difference({1, 2, 4, 6}, {3, 5, 7}, {1, 2, 4, 6});
        test_set_difference({1, 2, 4, 6}, {1, 2, 4, 6}, {});
        test_set_difference({1, 2, 4, 6, 7, 8, 9}, {1, 2, 4, 6}, {7, 8, 9});

        for (int x = 0; x < 100; ++x)
        {
            std::vector<int> in1;
            std::vector<int> in2;
            size_t const size1 = rand() % 100;
            size_t const size2 = rand() % 100;
            for (size_t i = 0; i < size1; ++i)
                in1.push_back(rand() % 100);
            for (size_t i = 0; i < size2; ++i)
                in2.push_back(rand() % 100);
            rah2::sort(in1);
            rah2::sort(in2);
            std::vector<int> outRef;
            std::set_difference(
                begin(in1), end(in1), begin(in2), end(in2), std::back_inserter(outRef));
            std::vector<int> out;
            for (int val : in1 | rah2::views::set_difference(in2))
                out.push_back(val);
            assert(out == outRef);
        }
    }

    // *********************************** algos **************************************************

    {
        /// [rah2::to_container_pipeable]
        std::vector<std::pair<int, char>> in1{{4, 'a'}, {5, 'b'}, {6, 'c'}, {7, 'd'}};
        std::map<int, char> map_4a_5b_6c_7d = in1 | rah2::to<std::map<int, char>>();
        assert(map_4a_5b_6c_7d == (std::map<int, char>{{4, 'a'}, {5, 'b'}, {6, 'c'}, {7, 'd'}}));

        std::list<int> in2{4, 5, 6, 7};
        std::vector<int> out = in2 | rah2::to<std::vector<int>>();
        assert(out == (std::vector<int>{4, 5, 6, 7}));
        /// [rah2::to_container_pipeable]
    }
    {
        /// [rah2::to]
        std::vector<std::pair<int, char>> in1{{4, 'a'}, {5, 'b'}, {6, 'c'}, {7, 'd'}};
        std::map<int, char> map_4a_5b_6c_7d = rah2::to<std::map<int, char>>(in1);
        assert(map_4a_5b_6c_7d == (std::map<int, char>{{4, 'a'}, {5, 'b'}, {6, 'c'}, {7, 'd'}}));

        std::list<int> in2{4, 5, 6, 7};
        std::vector<int> out = rah2::to<std::vector<int>>(in2);
        assert(out == (std::vector<int>{4, 5, 6, 7}));
        /// [rah2::to]
    }

    {
        /// [rah2::size]
        std::vector<int> vec3{1, 2, 3};
        assert(rah2::size(vec3) == 3);
        /// [rah2::size]
    }

    /// [rah2::empty]
    assert(not(rah2::empty(std::vector<int>{1, 2, 3})));
    assert(rah2::empty(std::vector<int>()));
    /// [rah2::empty]

    // ********************************* test return ref and non-ref ******************************

    using namespace rah2;
    using namespace rah2::views;
    using namespace std;

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
        std::vector<Elt> vec = {{0}, {1}, {2}, {3}, {4}};
        auto& r = vec;
        for (auto iter = std::begin(r), end_iter = std::end(r); iter != end_iter; ++iter)
        {
            iter->member = 42; // Check for mutability
        }
        EQUAL_RANGE(r, (il<Elt>({{42}, {42}, {42}, {42}, {42}})));
        for (auto&& elt : r)
        {
            static_assert(
                RAH2_NAMESPACE::is_reference_v<decltype(elt)>, "elt is expected to be a reference");
            elt.member = 78; // Check for mutability
        }
        EQUAL_RANGE(r, (il<Elt>({{78}, {78}, {78}, {78}, {78}})));
    }
    {
        std::vector<int> vec(5);
        for (int& i : vec | rah2::views::transform([](int& i) -> int& { return i; }))
            i = 42; // Check for mutability
        EQUAL_RANGE(vec, (il<int>({42, 42, 42, 42, 42})));
    }

    // Test return non-reference
    {
        std::vector<int> constVect{0, 1, 2, 3};
        EQUAL_RANGE(constVect | transform([](auto a) { return a * 2; }), il<int>({0, 2, 4, 6}));

        std::vector<Elt> vec = {{1}};
        auto r_copy = vec | transform([](auto a) { return Elt{a.member + 1}; });
        for (auto iter = rah2::begin(r_copy), end_iter = rah2::end(r_copy); iter != end_iter; ++iter)
        {
            assert(iter->member == 2); // Check for mutability
            assert((*iter).member == 2); // Check for mutability
            static_assert(
                RAH2_NAMESPACE::is_rvalue_reference_v<decltype(*iter)>
                    || (not RAH2_NAMESPACE::is_reference_v<decltype(*iter)>),
                "*iter is not expected to be a reference");
        }
        for (auto&& elt : r_copy)
        {
            assert(elt.member == 2); // Check for mutability
            static_assert(
                RAH2_NAMESPACE::is_rvalue_reference_v<decltype(elt)>
                    || (not RAH2_NAMESPACE::is_reference_v<decltype(elt)>),
                "elt is not expected to be a reference");
        }
        auto r_ref = vec | transform([](auto a) { return a.member; });
        for (auto iter = rah2::begin(r_ref), end_iter = rah2::end(r_ref); iter != end_iter; ++iter)
        {
            assert(*iter == 1); // Check for mutability
            static_assert(
                RAH2_NAMESPACE::is_rvalue_reference_v<decltype(*iter)>
                    || (not RAH2_NAMESPACE::is_reference_v<decltype(*iter)>),
                "*iter is not expected to be a reference");
        }
        for (auto&& elt : r_ref)
        {
            assert(elt == 1); // Check for mutability
            static_assert(
                RAH2_NAMESPACE::is_rvalue_reference_v<decltype(elt)>
                    || (not RAH2_NAMESPACE::is_reference_v<decltype(elt)>),
                "elt is not expected to be a reference");
        }
    }

    // **************************** divers compination test ***************************************

    {
        auto genRange = [](size_t i)
        {
            return rah2::views::zip(rah2::views::repeat(i), rah2::views::iota<size_t>(0, 3));
        };
        auto globalRange = rah2::views::iota<size_t>(0, 4) | rah2::views::transform(genRange)
                           | rah2::views::join();

        EQUAL_RANGE(
            globalRange,
            (il<std::tuple<size_t, size_t>>{
                {0, 0},
                {0, 1},
                {0, 2},
                {1, 0},
                {1, 1},
                {1, 2},
                {2, 0},
                {2, 1},
                {2, 2},
                {3, 0},
                {3, 1},
                {3, 2}}));
    }

    EQUAL_RANGE(
        (views::iota(0, 3) | transform([](auto i) { return i * 2; }) | enumerate()),
        (il<std::pair<int64_t, int>>{{0, 0}, {1, 2}, {2, 4}}));

    std::vector<char> vec_abcd{'a', 'b', 'c', 'd'};
    EQUAL_RANGE(
        (vec_abcd | transform([](char i) { return char(i + 1); }) | enumerate()),
        (il<std::pair<int64_t, char>>{{0, 'b'}, {1, 'c'}, {2, 'd'}, {3, 'e'}}));

    // TODO : Make Zip bidirectional when possible
    //EQUAL_RANGE(
    //    (iota(0, 3000, 3) | transform([](auto i) { return i * 2; }) | enumerate() | slice(10, 13)),
    //    (il<std::pair<size_t, int>>{ { 10, 60 }, { 11, 66 }, { 12, 72 } }));

    EQUAL_RANGE(
        (zip(vec_abcd, views::iota(0, 4))),
        (il<std::tuple<char, int>>{{'a', 0}, {'b', 1}, {'c', 2}, {'d', 3}}));

    EQUAL_RANGE((views::iota(0, 100) | slice(0, 20) | stride(3)), (il<int>{0, 3, 6, 9, 12, 15, 18}));

    EQUAL_RANGE((views::iota(10, 15) | reverse()), (il<int>{14, 13, 12, 11, 10}));

    EQUAL_RANGE((views::iota(0, 100) | slice(10, 15) | reverse()), (il<int>{14, 13, 12, 11, 10}));

    //EQUAL_RANGE(
    //    (iota(10, 15) | enumerate() | reverse()),
    //    (il<std::tuple<size_t, int>>{ { 4, 14 }, { 3, 13 }, { 2, 12 }, { 1, 11 }, { 0, 10 } }));

    //EQUAL_RANGE(
    //    (iota(0, 100) | enumerate() | slice(10, 15)),
    //    (il<std::tuple<size_t, int>>{ { 10, 10 }, { 11, 11 }, { 12, 12 }, { 13, 13 }, { 14, 14 } }));

    //EQUAL_RANGE(
    //    (iota(0, 100) | enumerate() | slice(10, 15) | reverse()),
    //    (il<std::tuple<size_t, int>>{ { 14, 14 }, { 13, 13 }, { 12, 12 }, { 11, 11 }, { 10, 10 } }));

    // iota(0, 10) | filter([](int i) { return i % 2 == 0; }) | rah2::to<std::vector<int>>();

    views::iota(0, 10) | filter([](int i) { return i % 2 == 0; }) | slice(1, 9)
        | rah2::to<std::vector<int>>();

    {
        // Test creation of a custom iterator
        auto gen = customGenerate();
        std::vector<int> gen_copy;
        std::copy(rah2::begin(gen), rah2::end(gen), std::back_inserter(gen_copy));
        EQUAL_RANGE(gen_copy, std::vector<int>({1, 2, 4, 8}));
    }

    {
        using namespace rah2;
        using namespace rah2::views;
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
                return std::make_tuple(y, views::iota(startX, width));
            else if (y == endY)
                return std::make_tuple(y, views::iota(0, endX));
            else
                return std::make_tuple(y, views::iota(0, width));
        };

        std::vector<std::atomic<int>> test(width * height);

        auto updateRaw = [&](auto&& y_xRange)
        {
            auto y = std::get<0>(y_xRange);
            auto xRange = std::get<1>(y_xRange);

            for (int x : xRange)
                ++test[x + y * width];
        };

        for (int ySelector : rah2::views::iota(0, 3))
        {
            auto rng = rah2::views::irange(startY + ySelector, endY + 1, 3) | transform(getRangeX);
            rah2::for_each(rng, updateRaw);
        }

        assert(all_of(test | slice(0, start), [](auto&& val) { return val == 0; }));
        assert(all_of(test | slice(start, end), [](auto&& val) { return val == 1; }));
        assert(all_of(test | slice(end, test.size()), [](auto&& val) { return val == 0; }));
    }

    testSuite.report();
    return EXIT_SUCCESS;
}
