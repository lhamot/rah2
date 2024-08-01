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

template <CommonOrSent CS, typename Tag, bool Sized>
struct make_join_view
{
    auto make()
    {
        return RAH2_NS::views::join(RAH2_NS::views::transform(
            make_test_view<CS, Tag, Sized>(), [](auto i) { return RAH2_NS::views::iota(0, i); }));
    }
    using BaseView = test_view<CS, Tag, Sized>;
    static constexpr bool is_sized = false;
    static constexpr bool is_common = false;
    static constexpr bool do_test = true;
    static constexpr bool is_borrowed = false;
    using expected_cat =
        RAH2_NS::ranges::details::cap_iterator_tag<Tag, RAH2_STD::input_iterator_tag, RAH2_NS::input_iterator_tag>;
};
void test_join_view()
{
    {
        testSuite.test_case("sample");
        /// [join]
        RAH2_STD::vector<RAH2_STD::vector<int>> in = {
            {},
            {0, 1},
            {},
            {2, 3, 4},
            {5},
            {},
        };
        auto range = RAH2_NS::views::join(in);
        RAH2_STD::vector<int> result;
        RAH2_NS::ranges::copy(range, RAH2_NS::back_inserter(result));
        assert(result == RAH2_STD::vector<int>({0, 1, 2, 3, 4, 5}));
        /// [join]
    }
    {
        testSuite.test_case("sample_pipeable");
        /// [join_pipeable]
        RAH2_STD::vector<RAH2_STD::vector<int>> in = {
            {0, 1},
            {},
            {2, 3, 4},
            {5},
            {},
        };
        auto range = in | RAH2_NS::views::join;
        RAH2_STD::vector<int> result;
        RAH2_NS::ranges::copy(range, RAH2_NS::back_inserter(result));
        assert(result == RAH2_STD::vector<int>({0, 1, 2, 3, 4, 5}));
        /// [join_pipeable]
    }
    {
        testSuite.test_case("rvalue_input");
        // Test join on a range of rvalue
        auto range = RAH2_NS::views::iota(0llu, 6llu)
                     | RAH2_NS::views::transform(
                         [](auto i) { return RAH2_NS::views::repeat(1) | RAH2_NS::views::take(i); })
                     | RAH2_NS::views::join;
        RAH2_STD::vector<int> result;
        RAH2_NS::ranges::copy(range, RAH2_NS::back_inserter(result));
        assert(result == RAH2_STD::vector<int>(15, 1));
    }

    testSuite.test_case("concepts");
    foreach_range_combination<test_range<make_join_view>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct make_split_view
{
    RAH2_STD::array<int, 2> delim = {3, 4};
    auto make()
    {
        return RAH2_NS::views::split(make_test_view<CS, Tag, Sized>(), delim);
    }
    static constexpr bool is_sized = false;
    static constexpr bool is_common = false;
    static constexpr bool do_test = RAH2_NS::ranges::forward_range<test_view<CS, Tag, Sized>>;
    static constexpr bool is_borrowed = false;
    using expected_cat =
        RAH2_NS::ranges::details::cap_iterator_tag<Tag, RAH2_STD::input_iterator_tag, RAH2_NS::input_iterator_tag>;
};
void test_split_view()
{
    testSuite.test_case("sample");
    /// [views::split]
    RAH2_STD::string sentence{"Keep..moving..forward.."};
    RAH2_STD::string const delim{".."};
    auto words = RAH2_NS::views::split(sentence, delim)
                 | RAH2_NS::views::transform([](auto word)
                                             { return RAH2_STD::string(word.begin(), word.end()); });

    EQUAL_RANGE(words, RAH2_STD::vector<RAH2_STD::string>({"Keep", "moving", "forward"}));
    /// [views::split]

    testSuite.test_case("concepts");
    // TODO : Allow forward_iterator
    // TODO : Allow common_range
    // TODO : Check inner_range (reference_t)
    foreach_range_combination<test_range<make_split_view>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct make_counted_view
{
    auto make()
    {
        static auto r = make_test_view<CS, Tag, Sized>();
        return RAH2_NS::views::counted(r.begin(), 8);
    }
    using BaseRange = test_view<CS, Tag, Sized>;
    static constexpr bool is_sized = RAH2_NS::ranges::random_access_range<BaseRange>;
    static constexpr bool is_common =
        RAH2_NS::random_access_iterator<RAH2_NS::ranges::iterator_t<BaseRange>>;
    static constexpr bool do_test = true;
    static constexpr bool is_borrowed = true; // It is actually a subrange
    using expected_cat = RAH2_NS::ranges::details::
        cap_iterator_tag<Tag, RAH2_STD::input_iterator_tag, RAH2_NS::contiguous_iterator_tag>;
};
void test_counted_view()
{
    testSuite.test_case("sample");
    /// [counted]
    RAH2_STD::vector<int> in{0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
    auto range = RAH2_NS::views::counted(in.begin(), 5);
    RAH2_STD::vector<int> out;
    RAH2_STD::copy(
        RAH2_NS::ranges::begin(range), RAH2_NS::ranges::end(range), RAH2_STD::back_inserter(out));
    assert(out == RAH2_STD::vector<int>({0, 1, 2, 3, 4}));
    /// [counted]

    testSuite.test_case("concepts");
    foreach_range_combination<test_range<make_counted_view>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct make_common_view
{
    auto make()
    {
        return RAH2_NS::views::common(make_test_view<CS, Tag, Sized>());
    }
    using BaseRange = test_view<CS, Tag, Sized>;
    static constexpr bool is_sized = RAH2_NS::ranges::sized_range<BaseRange>;
    static constexpr bool is_common = true;
    static constexpr bool do_test = true;
    static constexpr bool is_borrowed = RAH2_NS::ranges::enable_borrowed_range<BaseRange>;
    using base_cat = RAH2_NS::ranges::details::range_iter_categ_t<BaseRange>;
    using expected_cat = RAH2_STD::conditional_t<
        RAH2_NS::ranges::common_range<BaseRange>,
        base_cat,
        RAH2_STD::conditional_t<
            RAH2_NS::ranges::sized_range<BaseRange> && RAH2_NS::ranges::random_access_range<BaseRange>,
            base_cat,
            RAH2_NS::ranges::details::
                cap_iterator_tag<base_cat, RAH2_NS::input_iterator_tag, RAH2_NS::bidirectional_iterator_tag>>>;
};
void test_common_view()
{
    testSuite.test_case("sample");
    /// [rah2::views::common]
    auto c = RAH2_NS::views::iota(0, 5) | RAH2_NS::views::filter([](auto i) { return i % 2 == 0; });
    RAH2_STD::vector<int> result;
    for (auto&& i : c | RAH2_NS::views::common)
        result.push_back(i);
    assert(result == RAH2_STD::vector<int>({0, 2, 4}));
    /// [rah2::views::common]

    testSuite.test_case("concepts");
    foreach_range_combination<test_range<make_common_view>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct make_reverse_view
{
    auto make()
    {
        return RAH2_NS::views::reverse(make_test_view<CS, Tag, Sized>());
    }
    using BaseRange = test_view<CS, Tag, Sized>;
    static constexpr bool is_sized =
        RAH2_NS::ranges::sized_range<
            BaseRange> || RAH2_NS::random_access_iterator<RAH2_NS::ranges::iterator_t<BaseRange>>;
    static constexpr bool is_common = true;
    static constexpr bool do_test = RAH2_NS::ranges::bidirectional_range<BaseRange>;
    static constexpr bool is_borrowed = RAH2_NS::ranges::enable_borrowed_range<BaseRange>;
    using expected_cat = RAH2_NS::ranges::details::
        cap_iterator_tag<Tag, RAH2_STD::input_iterator_tag, RAH2_NS::random_access_iterator_tag>;
};
void test_reverse_view()
{
    {
        testSuite.test_case("sample");
        /// [reverse]
        RAH2_STD::vector<int> vec{0, 1, 2, 3};
        RAH2_STD::vector<int> result;
        for (int const i : RAH2_NS::views::reverse(vec))
            result.push_back(i);
        assert(result == RAH2_STD::vector<int>({3, 2, 1, 0}));
        /// [reverse]
    }
    {
        testSuite.test_case("sample_pipeable");
        /// [reverse_pipeable]
        RAH2_STD::vector<int> vec{0, 1, 2, 3};
        RAH2_STD::vector<int> result;
        for (int const i : vec | RAH2_NS::views::reverse)
            result.push_back(i);
        assert(result == RAH2_STD::vector<int>({3, 2, 1, 0}));
        /// [reverse_pipeable]
    }
    testSuite.test_case("concepts");
    foreach_range_combination<test_range<make_reverse_view>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct make_elements_view
{
    RAH2_STD::vector<RAH2_STD::tuple<bool, char, int>> vec{
        {true, 'a', 1000},
        {false, 'b', 1001},
        {true, 'c', 1002},
        {false, 'd', 1003},
    };

    auto make()
    {
        return RAH2_NS::views::elements<2>(make_test_view_adapter<CS, Tag, Sized>(vec));
    }
    using BaseRange =
        test_view_adapter<CS, Tag, Sized, RAH2_STD::vector<RAH2_STD::tuple<bool, char, int>>>;
    static constexpr bool is_sized = RAH2_NS::ranges::sized_range<BaseRange>;
    static constexpr bool is_common = RAH2_NS::ranges::common_range<BaseRange>;
    static constexpr bool do_test = true;
    static constexpr bool is_borrowed = RAH2_NS::ranges::enable_borrowed_range<BaseRange>;
    using expected_cat = RAH2_NS::ranges::details::
        cap_iterator_tag<Tag, RAH2_STD::input_iterator_tag, RAH2_NS::random_access_iterator_tag>;
};
void test_elements_view()
{
    {
        testSuite.test_case("sample");
        /// [elements_view]
        RAH2_STD::vector<RAH2_STD::tuple<bool, char, int>> vec{
            {true, 'a', 1000},
            {false, 'b', 1001},
            {true, 'c', 1002},
            {false, 'd', 1003},
        };
        RAH2_STD::vector<int> result;
        for (auto i : vec | RAH2_NS::views::elements<2>)
            result.push_back(i);
        assert(result == RAH2_STD::vector<int>({1000, 1001, 1002, 1003}));
        /// [elements_view]
    }

    testSuite.test_case("concepts");
    foreach_range_combination<test_range<make_elements_view>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct make_values_view
{
    RAH2_STD::vector<RAH2_STD::pair<int, std::string>> m{
        {12, "aaa"},
        {19, "bbb"},
        {25, "ccc"},
        {5, "ddd"},
    };

    auto make()
    {
        return RAH2_NS::views::values(make_test_view_adapter<CS, Tag, Sized>(m));
    }
    using BaseRange =
        test_view_adapter<CS, Tag, Sized, RAH2_STD::vector<RAH2_STD::tuple<bool, char, int>>>;
    static constexpr bool is_sized = RAH2_NS::ranges::sized_range<BaseRange>;
    static constexpr bool is_common = RAH2_NS::ranges::common_range<BaseRange>;
    static constexpr bool do_test = true;
    static constexpr bool is_borrowed = RAH2_NS::ranges::enable_borrowed_range<BaseRange>;
    using expected_cat = RAH2_NS::ranges::details::
        cap_iterator_tag<Tag, RAH2_STD::input_iterator_tag, RAH2_NS::random_access_iterator_tag>;
};
void test_values_view()
{
    {
        testSuite.test_case("sample");
        /// [values_view]
        RAH2_STD::vector<RAH2_STD::tuple<bool, char, int>> vec{
            {true, 'a', 1000},
            {false, 'b', 1001},
            {true, 'c', 1002},
            {false, 'd', 1003},
        };
        RAH2_STD::vector<char> result;
        for (auto i : vec | RAH2_NS::views::values)
            result.push_back(i);
        assert(result == RAH2_STD::vector<char>({'a', 'b', 'c', 'd'}));
        /// [values_view]
    }
    testSuite.test_case("concepts");
    foreach_range_combination<test_range<make_values_view>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct make_keys_view
{
    RAH2_STD::vector<RAH2_STD::pair<int, std::string>> m{
        {12, "aaa"},
        {19, "bbb"},
        {25, "ccc"},
        {5, "ddd"},
    };

    auto make()
    {
        return RAH2_NS::views::keys(make_test_view_adapter<CS, Tag, Sized>(m));
    }
    using BaseRange =
        test_view_adapter<CS, Tag, Sized, RAH2_STD::vector<RAH2_STD::tuple<bool, char, int>>>;
    static constexpr bool is_sized = RAH2_NS::ranges::sized_range<BaseRange>;
    static constexpr bool is_common = RAH2_NS::ranges::common_range<BaseRange>;
    static constexpr bool do_test = true;
    static constexpr bool is_borrowed = RAH2_NS::ranges::enable_borrowed_range<BaseRange>;
    using expected_cat = RAH2_NS::ranges::details::
        cap_iterator_tag<Tag, RAH2_STD::input_iterator_tag, RAH2_NS::random_access_iterator_tag>;
};
void test_keys_view()
{
    {
        testSuite.test_case("sample");
        /// [keys_view]
        RAH2_STD::vector<RAH2_STD::tuple<bool, char, int>> vec{
            {true, 'a', 1000},
            {false, 'b', 1001},
            {true, 'c', 1002},
            {false, 'd', 1003},
        };
        RAH2_STD::vector<bool> result;
        for (auto i : vec | RAH2_NS::views::keys)
            result.push_back(i);
        assert(result == RAH2_STD::vector<bool>({true, false, true, false}));
        /// [keys_view]
    }
    testSuite.test_case("concepts");
    foreach_range_combination<test_range<make_keys_view>>();
}
