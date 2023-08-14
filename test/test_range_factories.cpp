#include <rah2/ranges.hpp>

#include <sstream>

#ifdef RAH2_USE_EASTL

#include <EASTL/array.h>
#include <EASTL/algorithm.h>

#else

#include <array>
#include <algorithm>

#endif

#include "test_helpers.hpp"

void test_empty_view()
{
    testSuite.test_case("sample");
    /// [empty]
    RAH2_STD::vector<int> result;
    for (int const i : RAH2_NS::views::empty<int>)
        result.push_back(i);
    assert(result.empty());
    /// [empty]

    testSuite.test_case("concept");
    check_range_cat<RAH2_NS::contiguous_iterator_tag, decltype(RAH2_NS::views::empty<int>)>();
}

void test_single_view()
{
    testSuite.test_case("sample");
    /// [single]
    RAH2_STD::vector<int> result;
    for (int const i : RAH2_NS::views::single(20))
        result.push_back(i);
    assert(result == RAH2_STD::vector<int>({20}));
    /// [single]

    testSuite.test_case("concept");
    check_range_cat<RAH2_NS::contiguous_iterator_tag, decltype(RAH2_NS::views::single(20))>();
}

void test_iota_view()
{
    {
        testSuite.test_case("sample");
        /// [iota]
        RAH2_STD::vector<int> result;
        for (int const i : RAH2_NS::views::iota(10, 15))
            result.push_back(i);
        assert(result == RAH2_STD::vector<int>({10, 11, 12, 13, 14}));
        /// [iota]
    }

    {
        RAH2_STD::vector<int> result;
        for (int const i :
             RAH2_NS::views::iota(10) | RAH2_NS::views::slice(2, 5) | RAH2_NS::views::common)
            result.push_back(i);
        assert(result == RAH2_STD::vector<int>({12, 13, 14}));
    }

    {
        RAH2_STD::vector<size_t> result;
        for (size_t const i :
             RAH2_NS::views::iota() | RAH2_NS::views::slice(2, 5) | RAH2_NS::views::common)
            result.push_back(i);
        assert(result == RAH2_STD::vector<size_t>({2, 3, 4}));
    }

    testSuite.test_case("concept");
    check_range_cat<RAH2_NS::contiguous_iterator_tag, decltype(RAH2_NS::views::iota())>();
}

void test_istream_view()
{
    {
        testSuite.test_case("sample");
        /// [views::istream]
        std::stringstream ss("a b c d e f g h i j k l");
        RAH2_STD::vector<std::string> out;
        for (auto&& str : RAH2_NS::views::istream<std::string>(ss) | RAH2_NS::views::common)
        {
            out.push_back(str);
        }
        assert(
            out
            == RAH2_STD::vector<std::string>(
                {"a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l"}));
        /// [views::istream]
    }

    testSuite.test_case("concepts");
    check_range_cat<
        RAH2_NS::input_iterator_tag,
        decltype(RAH2_NS::views::istream<std::string>(RAH2_STD::declval<std::stringstream&>()))>();
}

void test_repeat_view()
{
    testSuite.test_case("sample");
    /// [repeat]
    RAH2_STD::vector<int> out;
    auto range = RAH2_NS::views::repeat(42);
    RAH2_STD::copy_n(RAH2_NS::ranges::begin(range), 5, RAH2_STD::back_inserter(out));
    assert(out == RAH2_STD::vector<int>({42, 42, 42, 42, 42}));
    /// [repeat]

    testSuite.test_case("concepts");
    check_range_cat<RAH2_NS::random_access_iterator_tag, decltype(RAH2_NS::views::repeat(42))>();
}

void test_generate_view()
{
    {
        testSuite.test_case("sample");
        /// [generate]
        int y = 1;
        auto gen = RAH2_NS::views::generate(
            [&y]() mutable
            {
                auto const prev = y;
                y *= 2;
                return prev;
            });
        RAH2_STD::vector<int> gen_copy;
        auto first = RAH2_NS::ranges::begin(gen);
        auto result = RAH2_NS::back_inserter(gen_copy);
        for (auto n = 4; n > 0; --n, ++result, ++first)
            *result = *first;
        assert(gen_copy == RAH2_STD::vector<int>({1, 2, 4, 8}));
        /// [generate]
        STATIC_ASSERT((RAH2_NS::ranges::input_range_impl<decltype(gen), true>::value));
        STATIC_ASSERT(((RAH2_NS::is_same_v<
                        RAH2_NS::ranges::range_iter_categ_t<decltype(gen)>,
                        RAH2_STD::input_iterator_tag>)));
        STATIC_ASSERT(not RAH2_NS::ranges::forward_range<decltype(gen)>);
        STATIC_ASSERT(not RAH2_NS::ranges::common_range<decltype(gen)>);
    }
    {
        /// [generate_n]
        RAH2_STD::vector<int> result;
        int y = 1;
        auto gen = RAH2_NS::views::generate_n(
            4,
            [&y]() mutable
            {
                auto const prev = y;
                y *= 2;
                return prev;
            });
        auto i = RAH2_NS::ranges::begin(gen);
        auto e = RAH2_NS::ranges::end(gen);
        for (; i != e; ++i)
            result.push_back(*i);
        assert(result == RAH2_STD::vector<int>({1, 2, 4, 8}));
        /// [generate_n]
        testSuite.test_case("concepts");
        check_range_cat<RAH2_STD::input_iterator_tag, decltype(gen)>();
    }
}

void test_irange_view()
{
    {
        testSuite.test_case("sample");
        /// [irange]
        RAH2_STD::vector<int> result;
        for (int const i : RAH2_NS::views::irange(10, 25, 3))
            result.push_back(i);
        assert(result == RAH2_STD::vector<int>({10, 13, 16, 19, 22}));
        /// [irange]
    }

    {
        RAH2_STD::vector<int> result;
        for (int const i : RAH2_NS::views::irange(10, 26, 3) | RAH2_NS::views::slice(2, 5)
                               | RAH2_NS::views::common)
            result.push_back(i);
        assert(result == RAH2_STD::vector<int>({16, 19, 22}));
    }
    testSuite.test_case("concepts");
    check_range_cat<RAH2_NS::random_access_iterator_tag, decltype(RAH2_NS::views::irange(10, 25, 3))>();
}
