//
// Copyright (c) 2019 Loïc HAMOT
//
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//
#include "rah4.hpp"
#include "eastl_sort.h"

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

#include "range_algorithms.hpp"
#include "test_helpers.hpp"

bool is_odd(int val)
{
    return val % 2 == 0;
}

/// [make_pipeable create]
auto test_count(int i)
{
    return rah::make_pipeable([=](auto&& range) { return std::count(begin(range), end(range), i); });
}
/// [make_pipeable create]

// Test creation of a custom iterator
struct CustomGenerator
    : rah::iterator_facade<CustomGenerator, rah::default_sentinel, int, RAH_STD::forward_iterator_tag>
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
    return rah::subrange<CustomGenerator, CustomGenerator>{};
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

TestSuite testSuite;

void test_range_concepts()
{
    STATIC_ASSERT(!rah::range<int>);
    STATIC_ASSERT(!rah::borrowed_range<int>);
    STATIC_ASSERT(!rah::sized_range<int>);
    STATIC_ASSERT(!rah::view<int>);
    STATIC_ASSERT(!rah::input_range<int>);
    STATIC_ASSERT((!rah::output_range<int, int>));
    STATIC_ASSERT(!rah::forward_range<int>);
    STATIC_ASSERT(!rah::bidirectional_range<int>);
    STATIC_ASSERT(!rah::random_access_range<int>);
    STATIC_ASSERT(!rah::contiguous_range<int>);
    STATIC_ASSERT(!rah::common_range<int>);
    STATIC_ASSERT(!rah::viewable_range<int>);
    STATIC_ASSERT(!rah::constant_range<int>);
}

int main()
{
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
        for (int i : rah::views::irange(10, 19, 2))
            result.push_back(i);
        assert(result == std::vector<int>({10, 12, 14, 16, 18}));
        /// [irange]
        STATIC_ASSERT((rah::random_access_range<decltype(rah::views::irange(10, 19, 2))>));
    }

    {
        std::vector<int> result;
        for (int i : rah::views::irange(-5, 5, 2))
            result.push_back(i);
        assert(result == std::vector<int>({-5, -3, -1, 1, 3}));
    }

    {
        std::vector<int> result;
        for (int i : rah::views::irange(-15, -6, 2))
            result.push_back(i);
        assert(result == std::vector<int>({-15, -13, -11, -9, -7}));
    }

    {
        /// [for_each]
        auto createRange = [](int i)
        {
            return rah::views::repeat(char('a' + i)) | rah::views::take(i);
        };
        auto range = rah::views::for_each(rah::views::iota(0, 5), createRange);
        std::string result;
        rah::copy(range, std::back_inserter(result));
        assert(result == "bccdddeeee");
        /// [for_each]
    }

    {
        /// [for_each_pipeable]
        auto range =
            rah::views::iota(0, 3)
            | rah::views::for_each(
                [&](int z)
                {
                    return rah::views::iota(3, 6)
                           | rah::views::for_each(
                               [&, z](int y)
                               {
                                   return rah::views::iota(6, 9)
                                          | rah::views::for_each(
                                              [&, y, z](int x)
                                              { return rah::views::single(x + y * 3 + z * 9); });
                               });
                });
        assert(equal(range, rah::views::iota(15, 42)));
        /// [for_each_pipeable]
    }

    {
        size_t count = 0;
        size_t count2 = 0;
        size_t count3 = 0;
        auto range =
            rah::views::iota(0, 3)
            | rah::views::for_each(
                [&](int z)
                {
                    ++count;
                    return rah::views::iota(3, 6)
                           | rah::views::for_each(
                               [&, z](int y)
                               {
                                   ++count2;
                                   return rah::views::iota(6, 9)
                                          | rah::views::for_each(
                                              [&, y, z](int x)
                                              {
                                                  ++count3;
                                                  return rah::views::single(x + y * 3 + z * 9);
                                              });
                               });
                });

        assert(equal(range, rah::views::iota(15, 42)));
        assert(count == 3);
        assert(count2 == 9);
        assert(count3 == 27);
    }

    {
        size_t xSize = 2;
        size_t ySize = 3;
        auto xyIndexes = [=](size_t y)
        {
            return rah::views::zip(rah::views::repeat(y), rah::views::iota<size_t>(0, xSize));
        };
        auto range = rah::views::iota<size_t>(0, ySize) | rah::views::for_each(xyIndexes);
        std::vector<std::tuple<size_t, size_t>> result;
        rah::copy(range, std::back_inserter(result));
        assert(
            result
            == (std::vector<std::tuple<size_t, size_t>>{
                {0, 0}, {0, 1}, {1, 0}, {1, 1}, {2, 0}, {2, 1}}));

        size_t zSize = 4;
        auto xyzIndexes = [=](size_t z)
        {
            return rah::views::zip(
                rah::views::repeat(z),
                rah::views::iota<size_t>(0, ySize) | rah::views::for_each(xyIndexes));
        };
        auto flattenTuple = [](auto&& z_yx)
        {
            using namespace std;
            return std::make_tuple(get<0>(z_yx), get<0>(get<1>(z_yx)), get<1>(get<1>(z_yx)));
        };
        auto rangeZYX = rah::views::iota<size_t>(0, zSize) | rah::views::for_each(xyzIndexes)
                        | rah::views::transform(flattenTuple);
        std::vector<std::tuple<size_t, size_t, size_t>> resultZYX;
        rah::copy(rangeZYX, std::back_inserter(resultZYX));
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
        auto gen = rah::views::generate(
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
        static_assert(rah::input_range<decltype(gen)>, "");
        static_assert(
            std::is_same_v<rah::range_iter_categ_t<decltype(gen)>, std::input_iterator_tag>, "");
        static_assert(not rah::forward_range<decltype(gen)>, "");
        static_assert(not rah::common_range<decltype(gen)>, "");
    }
    {
        /// [generate_n]
        std::vector<int> result;
        int y = 1;
        auto gen = rah::views::generate_n(
            4,
            [&y]() mutable
            {
                auto prev = y;
                y *= 2;
                return prev;
            });
        auto i = rah::begin(gen);
        auto e = rah::end(gen);
        for (; i != e; ++i)
            result.push_back(*i);
        assert(result == std::vector<int>({1, 2, 4, 8}));
        /// [generate_n]
        static_assert(rah::input_range<decltype(gen)>, "");
        static_assert(
            std::is_same_v<rah::range_iter_categ_t<decltype(gen)>, std::input_iterator_tag>, "");
        static_assert(not rah::forward_range<decltype(gen)>, "");
        static_assert(not rah::common_range<decltype(gen)>, "");
    }

    {
        /// [cycle]
        std::vector<int> in{0, 1, 2};
        auto cy = rah::views::cycle(in);
        std::vector<int> out;
        std::copy_n(cy.begin(), 8, std::back_inserter(out));
        assert(out == std::vector<int>({0, 1, 2, 0, 1, 2, 0, 1}));
        /// [cycle]
        STATIC_ASSERT((rah::bidirectional_range<decltype(cy)>));
        STATIC_ASSERT((not rah::random_access_range<decltype(cy)>));
    }
    { // Cycle + input/sentinel => input/sentinel
        auto cyInputSent =
            rah::views::cycle(make_test_view<Sentinel, std::input_iterator_tag, false>());
        STATIC_ASSERT(is_input_not_common<decltype(cyInputSent)>);
    }
    {
        auto cyForwardSent =
            rah::views::cycle(make_test_view<Sentinel, std::forward_iterator_tag, false>());
        STATIC_ASSERT(is_forward_not_common<decltype(cyForwardSent)>);
    }
    {
        auto cyForwardCommon =
            rah::views::cycle(make_test_view<Common, std::forward_iterator_tag, false>());
        STATIC_ASSERT(is_forward_not_common<decltype(cyForwardCommon)>);
    }
    { // Cycle can't be bidir if we can't assign sentinel to iterator
        auto cyBidirSent =
            rah::views::cycle(make_test_view<Sentinel, std::bidirectional_iterator_tag, false>());
        STATIC_ASSERT(is_forward_not_common<decltype(cyBidirSent)>);
    }
    {
        auto cyBidirCommon =
            rah::views::cycle(make_test_view<Common, std::bidirectional_iterator_tag, false>());
        STATIC_ASSERT(is_bidirectional_not_common<decltype(cyBidirCommon)>);
    }
    { // Cycle can't be bidir if we can't assign sentinel to iterator
        auto cyRandomSent =
            rah::views::cycle(make_test_view<Sentinel, std::random_access_iterator_tag, true>());
        STATIC_ASSERT(is_forward_not_common<decltype(cyRandomSent)>);
    }
    {
        auto cyRandomCommon =
            rah::views::cycle(make_test_view<Common, std::random_access_iterator_tag, true>());
        STATIC_ASSERT(is_bidirectional_not_common<decltype(cyRandomCommon)>);
    }
    { // Cycle can't be bidir if we can't assign sentinel to iterator
        auto cyContiSent =
            rah::views::cycle(make_test_view<Sentinel, rah::contiguous_iterator_tag, true>());
        STATIC_ASSERT(is_forward_not_common<decltype(cyContiSent)>);
    }
    {
        auto cyContiCommon =
            rah::views::cycle(make_test_view<Common, rah::contiguous_iterator_tag, true>());
        STATIC_ASSERT(is_bidirectional_not_common<decltype(cyContiCommon)>);
    }

    {
        /// [cycle_pipeable]
        std::vector<int> in{0, 1, 2};
        auto cy = in | rah::views::cycle();
        std::vector<int> out;
        std::copy_n(cy.begin(), 8, std::back_inserter(out));
        assert(out == std::vector<int>({0, 1, 2, 0, 1, 2, 0, 1}));
        /// [cycle_pipeable]
    }

    {
        std::vector<int> in{0, 1, 2};
        auto cy = rah::views::cycle(in) | rah::views::take(8);
        std::vector<int> out;
        // static_assert(RAH_NAMESPACE::range<decltype(std::back_inserter(out))>, "dkjh");
        rah::copy(cy, std::back_inserter(out));
        assert(out == std::vector<int>({0, 1, 2, 0, 1, 2, 0, 1}));
    }

    {
        std::vector<int> in{0, 1, 2};
        auto range = rah::views::drop(in, 6);
        std::vector<int> out;
        std::copy(rah::begin(range), rah::end(range), std::back_inserter(out));
        assert(out.empty());
    }

    {
        /// [unbounded]
        std::vector<int> in{0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
        auto range = rah::views::unbounded(in.begin());
        std::vector<int> out;
        std::copy_n(rah::begin(range), 5, std::back_inserter(out));
        assert(out == std::vector<int>({0, 1, 2, 3, 4}));
        /// [unbounded]
    }

    {
        std::vector<int> in{0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
        auto range = rah::views::unbounded(in.begin()) | rah::views::slice(0, 5);
        std::vector<int> out;
        rah::copy(range, std::back_inserter(out));
        assert(out == std::vector<int>({0, 1, 2, 3, 4}));
    }

    {
        int in[] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
        auto range =
            rah::views::unbounded((int const* const)std::begin(in)) | rah::views::slice(0, 5);
        std::vector<int> out;
        rah::copy(range, std::back_inserter(out));
        assert(out == std::vector<int>({0, 1, 2, 3, 4}));
    }

    {
        /// [counted_pipeable]
        std::vector<int> in{0, 1, 2, 3, 4, 5};
        auto range = in | rah::views::take(9);
        std::vector<int> out;
        auto a = rah::begin(range);
        auto b = rah::end(range);
        volatile auto dist = std::distance(a, b);
        (void)dist;
        std::copy(rah::begin(range), rah::end(range), std::back_inserter(out));
        assert(out == std::vector<int>({0, 1, 2, 3, 4, 5}));
        /// [counted_pipeable]
    }

    {
        /// [slice]
        std::vector<int> vec{0, 1, 2, 3, 4, 5, 6, 7};
        std::vector<int> result;
        for (int i : rah::views::slice(vec, 2, 6))
            result.push_back(i);
        assert(result == std::vector<int>({2, 3, 4, 5}));
        std::vector<int> result2;
        for (int i : rah::views::slice(vec, 2, 6))
            result2.push_back(i);
        assert(result2 == std::vector<int>({2, 3, 4, 5}));
        /// [slice]
    }
    {
        /// [slice_pipeable]
        std::vector<int> vec{0, 1, 2, 3, 4, 5, 6, 7};
        std::vector<int> result;
        for (int i : vec | rah::views::slice(2, 6))
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
        for (int i : rah::views::reverse(getVec()))
            result.push_back(i);
        assert(result == std::vector<int>({3, 2, 1, 0}));
    }

    {
        auto range = rah::views::generate_n(5, []() { return rand(); })
                     | rah::views::filter([](auto&& val) { return val % 2 == 0; });
        std::vector<int> result;
        for (int i : range)
            result.push_back(i);
    }
    {
        std::vector<int> vec_01234{0, 1, 2, 3, 4};
        std::vector<int> result;
        for (int i : rah::views::filter(vec_01234, &is_odd))
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
        for (Tutu i : rah::views::filter(vec_01234, [](Tutu a) { return a != Tutu::c; }))
            result.push_back(i);
        assert(result == std::vector<Tutu>({Tutu::a, Tutu::b, Tutu::d, Tutu::e}));
    }

    {
        int vec_01234[] = {0, 1, 2, 3, 4};
        std::vector<int> result;
        for (int i : rah::views::filter(vec_01234, [](auto a) { return a % 2 == 0; }))
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
             rah::views::zip(vec_01234, vec_bool)
                 | rah::views::filter([](auto&& a) { return std::get<0>(a).front() % 2 == 0; })
                 | rah::views::common())
            result.push_back(std::get<0>(i));
        assert(result == (std::vector<std::vector<int>>{{0}, {2}, {4}}));
        assert(vec_01234 == (std::vector<std::vector<int>>{{0}, {1}, {2}, {3}, {4}}));
    }
    {
        /// [filter_pipeable]
        std::vector<int> vec_01234{0, 1, 2, 3, 4};
        std::vector<int> result;
        for (int i : vec_01234 | rah::views::filter([](auto a) { return a % 2 == 0; }))
            result.push_back(i);
        assert(result == std::vector<int>({0, 2, 4}));
        /// [filter_pipeable]
    }

    {
        // test filter with the first elements filtered
        auto range1 =
            rah::views::iota(1, 10) | rah::views::filter([](auto&& val) { return val % 2 == 0; });
        assert(rah::none_of(range1, [](auto v) { return (v % 2) == 1; }));

        // test generate + filter
        auto range2 = rah::views::generate_n(100, []() { return rand(); })
                      | rah::views::filter([](auto&& val) { return val % 2 == 0; });

        assert(rah::none_of(range2, [](auto v) { return (v % 2) == 1; }));

        // Can create some compilation issue about lambda copy
        auto range3 = rah::views::iota(0, 5)
                      | rah::views::for_each(
                          [](auto)
                          {
                              return rah::views::generate_n(5, []() { return rand(); })
                                     | rah::views::filter([](auto&& val) { return val % 2 == 0; });
                          });
        assert(rah::none_of(range3, [](auto v) { return (v % 2) == 1; }));
    }

    {
        /// [concat]
        std::vector<int> inputA{0, 1, 2, 3};
        std::vector<int> inputB{4, 5, 6};
        std::vector<int> inputC{7, 8, 9, 10, 11};
        {
            std::vector<int> result;
            for (int i : rah::views::concat(inputA))
                result.push_back(i);
            assert(result == std::vector<int>({0, 1, 2, 3}));
        }
        {
            std::vector<int> result;
            for (int i : rah::views::concat(inputA, inputB))
                result.push_back(i);
            assert(result == std::vector<int>({0, 1, 2, 3, 4, 5, 6}));
        }
        {
            std::vector<int> result;
            for (int i : rah::views::concat(inputA, inputB, inputC))
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
            for (int i : rah::views::concat(inputA, inputB))
                result.push_back(i);
            assert(result == std::vector<int>({1, 2, 3, 4}));
        }
        {
            std::vector<int> result;
            for (int i : rah::views::concat(inputA, inputB))
                result.push_back(i);
            assert(result == std::vector<int>({1, 2, 3, 4}));
        }
        {
            std::vector<int> result;
            for (int i : rah::views::concat(inputA, inputA))
                result.push_back(i);
            assert(result == std::vector<int>({}));
        }
    }

    {
        /// [views::set_difference]
        std::vector<int> in1 = {1, 2, 3, 4, 5, 6};
        std::vector<int> in2 = {2, 4, 6, 7, 8, 9, 10};
        std::vector<int> out;
        for (int val : rah::views::set_difference(in1, in2))
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
            for (int val : rah::views::set_difference(in1, in2))
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
            rah::sort(in1);
            rah::sort(in2);
            std::vector<int> outRef;
            std::set_difference(
                begin(in1), end(in1), begin(in2), end(in2), std::back_inserter(outRef));
            std::vector<int> out;
            for (int val : in1 | rah::views::set_difference(in2))
                out.push_back(val);
            assert(out == outRef);
        }
    }

    // *********************************** algos **************************************************

    {
        /// [rah::equal_range]
        std::vector<int> vecIn1{1, 2, 2, 3, 4};
        {
            std::vector<int> out;
            for (int i : rah::equal_range(vecIn1, 0))
                out.push_back(i);
            assert(out == std::vector<int>({}));
        }
        {
            std::vector<int> out;
            for (int i : rah::equal_range(vecIn1, 1))
                out.push_back(i);
            assert(out == std::vector<int>({1}));
        }
        {
            std::vector<int> out;
            for (int i : rah::equal_range(vecIn1, 2))
                out.push_back(i);
            assert(out == std::vector<int>({2, 2}));
        }
        /// [rah::equal_range]
    }
    {
        /// [rah::equal_range_pred_0]
        struct S
        {
            int value;
            char test;
            bool operator==(S rhs) const
            {
                return value == rhs.value && test == rhs.test;
            }
        };
        struct FindS
        {
            bool operator()(S s, int val) const
            {
                return s.value < val;
            }
            bool operator()(int val, S s) const
            {
                return val < s.value;
            }
        };
        /// [rah::equal_range_pred_0]
        {
            /// [rah::equal_range_pred]
            std::vector<S> vecIn1{{1, 'a'}, {2, 'b'}, {2, 'c'}, {3, 'd'}, {4, 'e'}};
            {
                std::vector<S> out;
                for (S i : rah::equal_range(vecIn1, 0, FindS{}))
                    out.push_back(i);
                assert(out == std::vector<S>({}));
            }
            {
                std::vector<S> out;
                for (S i : rah::equal_range(vecIn1, 1, FindS{}))
                    out.push_back(i);
                assert(out == std::vector<S>({{1, 'a'}}));
            }
            {
                std::vector<S> out;
                for (S i : rah::equal_range(vecIn1, 2, FindS{}))
                    out.push_back(i);
                assert(out == std::vector<S>({{2, 'b'}, {2, 'c'}}));
            }
            /// [rah::equal_range_pred]
        }
    }

    {
        /// [rah::binary_search]
        std::vector<int> vecIn1{1, 2, 2, 3, 4};
        assert(not rah::binary_search(vecIn1, 0));
        assert(rah::binary_search(vecIn1, 1));
        assert(rah::binary_search(vecIn1, 2));
        /// [rah::binary_search]
    }
    {
        std::vector<int> vecIn1{0, 1, 2, 3};
        std::vector<int> vecOut{0, 0, 0, 0};
        rah::transform(vecIn1, begin(vecOut), [](int a) { return a + 1; });
        assert(vecOut == std::vector<int>({1, 2, 3, 4}));
    }
    {
        /// [rah::transform3]
        std::vector<int> vecIn1{0, 1, 2, 3};
        std::vector<int> vecOut;
        rah::transform(vecIn1, std::back_inserter(vecOut), [](int a) { return a + 1; });
        assert(vecOut == std::vector<int>({1, 2, 3, 4}));
        /// [rah::transform3]
    }
    {
        /// [rah::transform4]
        std::vector<int> vecIn1{0, 1, 2, 3};
        std::vector<int> vecIn2{4, 3, 2, 1};
        std::vector<int> vecOut;
        rah::transform(
            vecIn1, vecIn2, std::back_inserter(vecOut), [](int a, int b) { return a + b; });
        assert(vecOut == std::vector<int>({4, 4, 4, 4}));
        /// [rah::transform4]
    }

    {
        /// [rah::reduce]
        std::vector<int> vecIn1{1, 2, 3, 4};
        assert(rah::fold_left(vecIn1, 0, [](auto a, auto b) { return a + b; }) == 10);
        /// [rah::reduce]
    }

    /// [rah::any_of]
    assert(rah::any_of(std::initializer_list<int>{3, 0, 1, 3, 4, 6}, [](auto a) { return a == 3; }));
    /// [rah::any_of]

    /// [rah::all_of]
    assert(rah::all_of(std::initializer_list<int>{4, 4, 4, 4}, [](auto a) { return a == 4; }));
    /// [rah::all_of]
    assert(
        rah::all_of(std::initializer_list<int>{4, 4, 3, 4}, [](auto a) { return a == 4; }) == false);

    /// [rah::none_of]
    assert((rah::none_of(std::initializer_list<int>{7, 8, 9, 10}, [](auto a) { return a == 11; })));
    /// [rah::none_of]

    /// [rah::count]
    assert(rah::count(std::initializer_list<int>{4, 4, 4, 3}, 3) == 1);
    /// [rah::count]

    /// [rah::count_if]
    assert(rah::count_if(std::initializer_list<int>{4, 4, 4, 3}, [](auto a) { return a == 4; }) == 3);
    /// [rah::count_if]

    {
        /// [rah::for_each]
        std::vector<int> testFE{4, 4, 4, 4};
        rah::for_each(testFE, [](auto& value) { return ++value; });
        EQUAL_RANGE(testFE, std::initializer_list<int>({5, 5, 5, 5}));
        /// [rah::for_each]
    }

    {
        /// [rah::to_container_pipeable]
        std::vector<std::pair<int, char>> in1{{4, 'a'}, {5, 'b'}, {6, 'c'}, {7, 'd'}};
        std::map<int, char> map_4a_5b_6c_7d = in1 | rah::to<std::map<int, char>>();
        assert(map_4a_5b_6c_7d == (std::map<int, char>{{4, 'a'}, {5, 'b'}, {6, 'c'}, {7, 'd'}}));

        std::list<int> in2{4, 5, 6, 7};
        std::vector<int> out = in2 | rah::to<std::vector<int>>();
        assert(out == (std::vector<int>{4, 5, 6, 7}));
        /// [rah::to_container_pipeable]
    }
    {
        /// [rah::to]
        std::vector<std::pair<int, char>> in1{{4, 'a'}, {5, 'b'}, {6, 'c'}, {7, 'd'}};
        std::map<int, char> map_4a_5b_6c_7d = rah::to<std::map<int, char>>(in1);
        assert(map_4a_5b_6c_7d == (std::map<int, char>{{4, 'a'}, {5, 'b'}, {6, 'c'}, {7, 'd'}}));

        std::list<int> in2{4, 5, 6, 7};
        std::vector<int> out = rah::to<std::vector<int>>(in2);
        assert(out == (std::vector<int>{4, 5, 6, 7}));
        /// [rah::to]
    }

    {
        /// [rah::mismatch]
        std::vector<int> in1 = {1, 2, 3, 4};
        std::vector<int> in2 = {1, 2, 42, 42};
        auto r1_r2 = rah::mismatch(in1, in2);
        std::vector<int> out1;
        std::vector<int> out2;
        std::copy(std::get<0>(r1_r2), end(in1), std::back_inserter(out1));
        std::copy(std::get<1>(r1_r2), end(in2), std::back_inserter(out2));
        assert(out1 == std::vector<int>({3, 4}));
        assert(out2 == std::vector<int>({42, 42}));
        /// [rah::mismatch]
    }

    {
        /// [rah::find]
        std::vector<int> in{1, 2, 3, 4};
        auto iter = rah::find(in, 3);
        assert(rah::equal(rah::make_subrange(iter, end(in)), std::initializer_list<int>({3, 4})));
        /// [rah::find]
    }

    {
        /// [rah::find_if]
        std::vector<int> in{1, 2, 3, 4};
        auto iter = rah::find_if(in, [](int i) { return i == 3; });
        assert(rah::equal(rah::make_subrange(iter, end(in)), std::initializer_list<int>({3, 4})));
        /// [rah::find_if]
    }
    {
        /// [rah::find_if_not]
        std::vector<int> in{1, 2, 3, 4};
        auto iter = rah::find_if_not(in, [](int i) { return i < 3; });
        assert(rah::equal(rah::make_subrange(iter, end(in)), std::initializer_list<int>({3, 4})));
        /// [rah::find_if_not]
    }

    {
        /// [rah::max_element]
        std::vector<int> in{1, 5, 3, 4};
        auto iter = rah::max_element(in);
        assert(*iter == 5);
        /// [rah::max_element]
    }
    {
        /// [rah::max_element_pred]
        std::vector<std::pair<int, int>> in{{100, 3}, {0, 5}, {0, 1}, {0, 4}};
        auto iter = rah::max_element(in, [](auto&& a, auto& b) { return a.second < b.second; });
        assert(*iter == (std::pair<int, int>{0, 5}));
        /// [rah::max_element_pred]
    }

    {
        /// [rah::min_element]
        std::vector<int> in{1, -5, 3, 4};
        auto iter = rah::min_element(in);
        assert(*iter == -5);
        /// [rah::min_element]
    }

    {
        /// [rah::min_element_pred]
        std::vector<std::pair<int, int>> in{{-100, 3}, {0, -5}, {0, 1}, {0, 4}};
        auto iter = rah::min_element(in, [](auto&& a, auto& b) { return a.second < b.second; });
        assert(*iter == (std::pair<int, int>{0, -5}));
        /// [rah::min_element_pred]
    }

    {
        /// [rah::size]
        std::vector<int> vec3{1, 2, 3};
        assert(rah::size(vec3) == 3);
        /// [rah::size]
    }

    {
        /// [rah::equal]
        std::vector<int> in1{1, 2, 3};
        std::vector<int> in2{1, 2, 3};
        std::vector<int> in3{11, 12, 13};
        assert(rah::equal(in1, in2));
        assert(rah::equal(in1, in3) == false);
        /// [rah::equal]
    }

    /// [rah::empty]
    assert(not(rah::empty(std::vector<int>{1, 2, 3})));
    assert(rah::empty(std::vector<int>()));
    /// [rah::empty]

    {
        /// [rah::copy]
        std::vector<int> in{1, 2, 3};
        std::vector<int> out{0, 0, 0, 4, 5};
        // std::vector<int> out{ 0, 0 }; // Trigger an assert
        assert(rah::equal(
            rah::make_subrange(rah::copy(in, out.begin()).out, end(out)),
            std::initializer_list<int>({4, 5})));
        assert(out == (std::vector<int>{1, 2, 3, 4, 5}));
        /// [rah::copy]
    }

    {
        /// [rah::copy_if]
        std::vector<int> in{1, 2, 3, 4};
        std::vector<int> out{0, 0, 5, 6};
        assert(rah::equal(
            rah::make_subrange(
                rah::copy_if(in, out.begin(), [](int i) { return i % 2 == 0; }).out, end(out)),
            std::initializer_list<int>({5, 6})));
        assert(out == (std::vector<int>{2, 4, 5, 6}));
        /// [rah::copy_if]
    }

    {
        /// [rah::fill]
        std::vector<int> out{0, 0, 0, 4, 5};
        rah::fill(out, 42);
        assert(out == (std::vector<int>{42, 42, 42, 42, 42}));
        /// [rah::fill]
    }

    {
        std::vector<int> out{0, 0, 0, 4, 5};
        rah::fill(out | rah::views::take(3), 42);
        assert(out == (std::vector<int>{42, 42, 42, 4, 5}));
    }

    {
        /// [rah::remove_if]
        std::vector<int> in{1, 2, 3, 4, 5};
        auto range_to_erase_begin = rah::remove_if(in, [](auto a) { return a < 4; });
        in.erase(range_to_erase_begin.begin(), range_to_erase_begin.end());
        std::sort(in.begin(), in.end());
        assert(in == std::vector<int>({4, 5}));
        /// [rah::remove_if]
    }
    {
        /// [rah::remove]
        std::vector<int> in{1, 2, 1, 3, 1};
        auto range_to_erase_begin = rah::remove(in, 1);
        in.erase(range_to_erase_begin.begin(), range_to_erase_begin.end());
        std::sort(in.begin(), in.end());
        assert(in == std::vector<int>({2, 3}));
        /// [rah::remove]
    }
    {
        /// [rah::partition]
        std::vector<int> in{1, 2, 3, 4, 5};
        auto boundary = rah::partition(in, [](auto a) { return a >= 4; });
        assert(boundary.begin() == in.begin() + 2);
        std::sort(in.begin(), boundary.begin());
        std::sort(boundary.begin(), in.end());
        assert(in == std::vector<int>({4, 5, 1, 2, 3}));
        /// [rah::partition]
    }
    {
        /// [rah::stable_partition]
        std::vector<int> in{1, 2, 3, 4, 5};
        auto boundary = rah::stable_partition(in, [](auto a) { return a >= 4; });
        assert(boundary == in.begin() + 2);
        assert(in == std::vector<int>({4, 5, 1, 2, 3}));
        /// [rah::stable_partition]
    }
    {
        /// [rah::sort]
        std::vector<int> in{2, 1, 5, 3, 4};
        rah::sort(in);
        assert(in == std::vector<int>({1, 2, 3, 4, 5}));
        /// [rah::sort]
    }
    {
        /// [rah::sort_pred]
        std::vector<int> in{2, 1, 5, 3, 4};
        rah::sort(in, [](auto a, auto b) { return a < b; });
        assert(in == std::vector<int>({1, 2, 3, 4, 5}));
        /// [rah::sort_pred]
    }

    /// [rah::stable_sort]
    struct CmpA
    {
        int a;
        int b;
        bool operator<(CmpA rhs) const
        {
            return a < rhs.a;
        }
        bool operator==(CmpA rhs) const
        {
            return a == rhs.a && b == rhs.b;
        }
    };

    {
        std::vector<CmpA> in{{4, 1}, {2, 1}, {4, 2}, {1, 1}, {4, 3}, {2, 2}, {4, 4}};
        rah::stable_sort(in);
        assert(in == std::vector<CmpA>({{1, 1}, {2, 1}, {2, 2}, {4, 1}, {4, 2}, {4, 3}, {4, 4}}));
    }
    /// [rah::stable_sort]
    {
        /// [rah::stable_sort_pred]
        std::vector<CmpA> in{{4, 1}, {2, 1}, {4, 2}, {1, 1}, {4, 3}, {2, 2}, {4, 4}};
        rah::stable_sort(in, [](CmpA l, CmpA r) { return l.b < r.b; });
        assert(in == std::vector<CmpA>({{4, 1}, {2, 1}, {1, 1}, {4, 2}, {2, 2}, {4, 3}, {4, 4}}));
        /// [rah::stable_sort_pred]
    }
    {
        /// [rah::shuffle]
        std::random_device rd;
        std::mt19937 g(rd());
        std::vector<int> in{1, 2, 3, 4, 5, 6};
        rah::shuffle(in, g);
        /// [rah::shuffle]
    }
    {
        /// [rah::unique]
        std::vector<int> in{2, 1, 1, 1, 5, 3, 3, 4};
        in.erase(rah::unique(in).begin(), end(in));
        assert(in == std::vector<int>({2, 1, 5, 3, 4}));
        /// [rah::unique]
    }
    {
        /// [rah::unique_pred]
        std::vector<int> in{2, 1, 1, 1, 5, 3, 3, 4};
        in.erase(rah::unique(in, [](auto a, auto b) { return a == b; }).begin(), end(in));
        assert(in == std::vector<int>({2, 1, 5, 3, 4}));
        /// [rah::unique_pred]
    }
    {
        /// [rah::set_difference]
        std::vector<int> in1{1, 3, 4};
        std::vector<int> in2{1, 2, 3};
        std::vector<int> out{0, 0, 0, 0};
        rah::set_difference(in1, in2, out.begin());
        assert(out == std::vector<int>({4, 0, 0, 0}));
        /// [rah::set_difference]
    }
    {
        /// [rah::set_intersection]
        std::vector<int> in1{1, 3, 4};
        std::vector<int> in2{1, 2, 3};
        std::vector<int> out{0, 0, 0, 0};
        rah::set_intersection(in1, in2, out.begin());
        assert(out == std::vector<int>({1, 3, 0, 0}));
        /// [rah::set_intersection]
    }

    // ********************************* test return ref and non-ref ******************************

    using namespace rah;
    using namespace rah::views;
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
            static_assert(test::is_reference_v<decltype(elt)>, "elt is expected to be a reference");
            elt.member = 78; // Check for mutability
        }
        EQUAL_RANGE(r, (il<Elt>({{78}, {78}, {78}, {78}, {78}})));
    }
    {
        std::vector<int> vec(5);
        for (int& i : vec | rah::views::transform([](int& i) -> int& { return i; }))
            i = 42; // Check for mutability
        EQUAL_RANGE(vec, (il<int>({42, 42, 42, 42, 42})));
    }

    // Test return non-reference
    {
        std::vector<int> constVect{0, 1, 2, 3};
        EQUAL_RANGE(constVect | transform([](auto a) { return a * 2; }), il<int>({0, 2, 4, 6}));

        std::vector<Elt> vec = {{1}};
        auto r_copy = vec | transform([](auto a) { return Elt{a.member + 1}; });
        for (auto iter = rah::begin(r_copy), end_iter = rah::end(r_copy); iter != end_iter; ++iter)
        {
            assert(iter->member == 2); // Check for mutability
            assert((*iter).member == 2); // Check for mutability
            static_assert(
                test::is_rvalue_reference_v<decltype(*iter)>
                    || (not test::is_reference_v<decltype(*iter)>),
                "*iter is not expected to be a reference");
        }
        for (auto&& elt : r_copy)
        {
            assert(elt.member == 2); // Check for mutability
            static_assert(
                test::is_rvalue_reference_v<decltype(elt)>
                    || (not test::is_reference_v<decltype(elt)>),
                "elt is not expected to be a reference");
        }
        auto r_ref = vec | transform([](auto a) { return a.member; });
        for (auto iter = rah::begin(r_ref), end_iter = rah::end(r_ref); iter != end_iter; ++iter)
        {
            assert(*iter == 1); // Check for mutability
            static_assert(
                test::is_rvalue_reference_v<decltype(*iter)>
                    || (not test::is_reference_v<decltype(*iter)>),
                "*iter is not expected to be a reference");
        }
        for (auto&& elt : r_ref)
        {
            assert(elt == 1); // Check for mutability
            static_assert(
                test::is_rvalue_reference_v<decltype(elt)>
                    || (not test::is_reference_v<decltype(elt)>),
                "elt is not expected to be a reference");
        }
    }

    // **************************** divers compination test ***************************************

    {
        auto genRange = [](size_t i)
        {
            return rah::views::zip(rah::views::repeat(i), rah::views::iota<size_t>(0, 3));
        };
        auto globalRange =
            rah::views::iota<size_t>(0, 4) | rah::views::transform(genRange) | rah::views::join();

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
        (iota(0, 3) | transform([](auto i) { return i * 2; }) | enumerate()),
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
        (zip(vec_abcd, iota(0, 4))),
        (il<std::tuple<char, int>>{{'a', 0}, {'b', 1}, {'c', 2}, {'d', 3}}));

    EQUAL_RANGE((iota(0, 100) | slice(0, 20) | stride(3)), (il<int>{0, 3, 6, 9, 12, 15, 18}));

    EQUAL_RANGE((iota(10, 15) | reverse()), (il<int>{14, 13, 12, 11, 10}));

    EQUAL_RANGE((iota(0, 100) | slice(10, 15) | reverse()), (il<int>{14, 13, 12, 11, 10}));

    //EQUAL_RANGE(
    //    (iota(10, 15) | enumerate() | reverse()),
    //    (il<std::tuple<size_t, int>>{ { 4, 14 }, { 3, 13 }, { 2, 12 }, { 1, 11 }, { 0, 10 } }));

    //EQUAL_RANGE(
    //    (iota(0, 100) | enumerate() | slice(10, 15)),
    //    (il<std::tuple<size_t, int>>{ { 10, 10 }, { 11, 11 }, { 12, 12 }, { 13, 13 }, { 14, 14 } }));

    //EQUAL_RANGE(
    //    (iota(0, 100) | enumerate() | slice(10, 15) | reverse()),
    //    (il<std::tuple<size_t, int>>{ { 14, 14 }, { 13, 13 }, { 12, 12 }, { 11, 11 }, { 10, 10 } }));

    // iota(0, 10) | filter([](int i) { return i % 2 == 0; }) | rah::to<std::vector<int>>();

    iota(0, 10) | filter([](int i) { return i % 2 == 0; }) | slice(1, 9)
        | rah::to<std::vector<int>>();

    {
        // Test creation of a custom iterator
        auto gen = customGenerate();
        std::vector<int> gen_copy;
        std::copy(rah::begin(gen), rah::end(gen), std::back_inserter(gen_copy));
        EQUAL_RANGE(gen_copy, std::vector<int>({1, 2, 4, 8}));
    }

    {
        using namespace rah;
        using namespace rah::views;
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
                return std::make_tuple(y, iota(startX, width));
            else if (y == endY)
                return std::make_tuple(y, iota(0, endX));
            else
                return std::make_tuple(y, iota(0, width));
        };

        std::vector<std::atomic<int>> test(width * height);

        auto updateRaw = [&](auto&& y_xRange)
        {
            auto y = std::get<0>(y_xRange);
            auto xRange = std::get<1>(y_xRange);

            for (int x : xRange)
                ++test[x + y * width];
        };

        for (int ySelector : rah::views::iota(0, 3))
        {
            auto rng = rah::views::irange(startY + ySelector, endY + 1, 3) | transform(getRangeX);
            rah::for_each(rng, updateRaw);
        }

        assert(all_of(test | slice(0, start), [](auto&& val) { return val == 0; }));
        assert(all_of(test | slice(start, end), [](auto&& val) { return val == 1; }));
        assert(all_of(test | slice(end, test.size()), [](auto&& val) { return val == 0; }));
    }

    testSuite.report();
    return EXIT_SUCCESS;
}
