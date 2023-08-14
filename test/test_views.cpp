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
struct make_counted_iterator
{
    using BaseRange = test_view<CS, Tag, Sized>;
    BaseRange base;
    auto make()
    {
        return RAH2_NS::make_counted_iterator(RAH2_NS::ranges::begin(base), 10);
    }
    static constexpr bool do_test = true;
    using expected_cat = Tag;
};
void test_counted_iterator()
{
    testSuite.test_case("concepts");
    foreach_range_combination<test_iterator<make_counted_iterator>>();
}

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
    STATIC_ASSERT(
        (RAH2_NS::ranges::contiguous_range_impl<RAH2_NS::ranges::empty_view<int>, true>::value));
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
    STATIC_ASSERT(RAH2_NS::ranges::contiguous_range<RAH2_NS::ranges::single_view<int>>);
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
        STATIC_ASSERT(RAH2_NS::ranges::random_access_range<decltype(RAH2_NS::views::iota(10, 15))>);
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
    STATIC_ASSERT((RAH2_NS::ranges::input_range<decltype(RAH2_NS::views::istream<std::string>(
                       RAH2_STD::declval<std::stringstream&>()))>));
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
    STATIC_ASSERT((RAH2_NS::ranges::random_access_range_impl<decltype(range), true>::value));
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct make_owning_view
{
    using BaseRange = test_view<CS, Tag, Sized>;
    auto make()
    {
        return RAH2_NS::views::owning(BaseRange{});
    }
    static constexpr bool is_sized = RAH2_NS::ranges::sized_range<BaseRange>;
    static constexpr bool is_common = RAH2_NS::ranges::common_range<BaseRange>;
    static constexpr bool do_test = true;
    static constexpr bool is_borrowed = false;
    using expected_cat = Tag;
};
void test_owning_view()
{
    testSuite.test_case("sample");
    /// [owning_view]
    RAH2_STD::vector<int> out;
    auto owning = RAH2_NS::views::owning(RAH2_STD::vector<int>{0, 1, 2, 2, 3});
    for (auto&& val : owning)
    {
        out.push_back(val);
    }
    assert(out == (RAH2_STD::vector<int>{0, 1, 2, 2, 3}));
    /// [owning_view]

    testSuite.test_case("concepts");
    foreach_range_combination<test_range<make_owning_view>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct make_all
{
    using BaseRange = test_view<CS, Tag, Sized>;
    BaseRange base;
    auto make()
    {
        return RAH2_NS::views::all(base);
    }
    static constexpr bool is_sized = RAH2_NS::ranges::sized_range<BaseRange>;
    static constexpr bool is_common = RAH2_NS::ranges::common_range<BaseRange>;
    static constexpr bool do_test = true;
    static constexpr bool is_borrowed = RAH2_NS::ranges::view<BaseRange> ?
                                            RAH2_NS::ranges::borrowed_range<BaseRange> :
                                        (!RAH2_NS::is_rvalue_reference_v<BaseRange>) ? true :
                                                                                       false;
    using expected_cat = Tag;
};
void test_all_view()
{
    // Test all
    // A views can't embbed a container
    // EQUAL_RANGE((il<int>{ 0, 1, 2, 3 } | RAH2_NS::views::all), (il<int>{ 0, 1, 2, 3 }));
    int intTab[] = {0, 1, 2, 3};
    testSuite.test_case("lvalue_container");
    EQUAL_RANGE((intTab | RAH2_NS::views::all), (il<int>{0, 1, 2, 3}));
    testSuite.test_case("rvalue_container");
    EQUAL_RANGE((RAH2_STD::vector<int>({0, 1, 2, 3}) | RAH2_NS::views::all), (il<int>{0, 1, 2, 3}));

    testSuite.test_case("sample");
    /// [views::all]
    RAH2_STD::vector<int> out;
    auto all = RAH2_NS::views::all(RAH2_STD::vector<int>{0, 1, 2, 2, 3});
    for (auto&& val : all)
    {
        out.push_back(val);
    }
    assert(out == (RAH2_STD::vector<int>{0, 1, 2, 2, 3}));
    /// [views::all]

    testSuite.test_case("concepts");
    foreach_range_combination<test_range<make_all>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct make_filter_view
{
    RAH2_NODISCARD auto make() const
    {
        return RAH2_NS::views::filter(
            make_test_view<CS, Tag, Sized>(), [](auto a) { return a % 2 == 0; });
    }
    static constexpr bool is_sized = false;
    static constexpr bool is_common = RAH2_NS::ranges::common_range<test_view<CS, Tag, Sized>>;
    static constexpr bool do_test = true;
    static constexpr bool is_borrowed = false;
    using expected_cat =
        RAH2_NS::ranges::cap_iterator_tag<Tag, RAH2_STD::input_iterator_tag, RAH2_NS::bidirectional_iterator_tag>;
};
void test_filter_view()
{
    testSuite.test_case("sample");
    /// [filter]
    RAH2_STD::vector<int> vec_01234{0, 1, 2, 3, 4, 5};
    RAH2_STD::vector<int> result;
    for (int const i : RAH2_NS::views::filter(vec_01234, [](auto a) { return a % 2 == 0; }))
        result.push_back(i);
    assert(result == RAH2_STD::vector<int>({0, 2, 4}));
    /// [filter]

    testSuite.test_case("concepts");
    foreach_range_combination<test_range<make_filter_view>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct make_transform_view
{
    auto make()
    {
        auto v = make_test_view<CS, Tag, Sized>();
        AssertEqual<RAH2_NS::ranges::sized_range<decltype(v)>, Sized>();
        return RAH2_NS::views::transform(
            make_test_view<CS, Tag, Sized>(), [](auto a) { return a % 2 == 0; });
    }
    static constexpr bool is_sized = Sized;
    static constexpr bool is_common = RAH2_NS::ranges::common_range<test_view<CS, Tag, Sized>>;
    static constexpr bool do_test = true;
    static constexpr bool is_borrowed = false;
    using expected_cat =
        RAH2_NS::ranges::cap_iterator_tag<Tag, RAH2_STD::input_iterator_tag, RAH2_NS::random_access_iterator_tag>;
};

void test_transform_view()
{
    // Test transform
    {
        testSuite.test_case("sample");
        /// [rah2::views::transform]
        RAH2_STD::vector<int> vec{0, 1, 2, 3};
        RAH2_STD::vector<int> result;
        for (int const i : RAH2_NS::views::transform(vec, [](auto a) { return a * 2; }))
            result.push_back(i);
        assert(result == RAH2_STD::vector<int>({0, 2, 4, 6}));
        /// [rah2::views::transform]
    }
    testSuite.test_case("various");
    {
        RAH2_STD::vector<int> vec{0, 1, 2, 3};
        auto valueSelector = [](auto a)
        {
            return a * 2;
        };
        auto selectedValuesRange = RAH2_NS::views::transform(vec, valueSelector);
        auto bounds = RAH2_STD::minmax_element(
            RAH2_NS::ranges::begin(selectedValuesRange), RAH2_NS::ranges::end(selectedValuesRange));
        auto const min = *bounds.first;
        assert(min == 0);
        auto const max = *bounds.second;
        assert(max == 6); // 3 * 2
    }
    {
        /// [rah2::views::transform_pipeable]
        RAH2_STD::vector<int> vec{0, 1, 2, 3};
        RAH2_STD::vector<int> result;
        for (int const i : vec | RAH2_NS::views::transform([](auto a) { return a * 2; }))
            result.push_back(i);
        assert(result == RAH2_STD::vector<int>({0, 2, 4, 6}));
        /// [rah2::views::transform_pipeable]
    }

    testSuite.test_case("concept");
    foreach_range_combination<test_range<make_transform_view>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct make_take_view
{
    auto make()
    {
        return RAH2_NS::views::take(make_test_view<CS, Tag, Sized>(), 8);
    }
    static constexpr bool is_sized = Sized;
    using R = test_view<CS, Tag, Sized>;
    static constexpr bool is_common =
        RAH2_NS::ranges::sized_range<R> && RAH2_NS::ranges::random_access_range<R>;
    static constexpr bool do_test = true;
    static constexpr bool is_borrowed = RAH2_NS::ranges::enable_borrowed_range<R>;
    using expected_cat =
        RAH2_NS::ranges::cap_iterator_tag<Tag, RAH2_STD::input_iterator_tag, RAH2_NS::contiguous_iterator_tag>;
};
void test_take_view()
{
    {
        testSuite.test_case("sample");
        /// [take]
        RAH2_STD::vector<int> in{0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
        auto range = RAH2_NS::views::take(in, 5);
        RAH2_STD::vector<int> out;
        RAH2_STD::copy(
            RAH2_NS::ranges::begin(range), RAH2_NS::ranges::end(range), RAH2_STD::back_inserter(out));
        assert(out == RAH2_STD::vector<int>({0, 1, 2, 3, 4}));
        auto range2 = RAH2_NS::views::take(in, 1000);
        RAH2_STD::vector<int> out2;
        RAH2_STD::copy(
            RAH2_NS::ranges::begin(range2),
            RAH2_NS::ranges::end(range2),
            RAH2_STD::back_inserter(out2));
        assert(out2 == RAH2_STD::vector<int>({0, 1, 2, 3, 4, 5, 6, 7, 8, 9}));
        /// [take]
    }
    testSuite.test_case("sample_pipeable");
    {
        /// [take_pipeable]
        RAH2_STD::vector<int> in{0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
        auto range = in | RAH2_NS::views::take(5);
        RAH2_STD::vector<int> out;
        RAH2_STD::copy(
            RAH2_NS::ranges::begin(range), RAH2_NS::ranges::end(range), RAH2_STD::back_inserter(out));
        assert(out == RAH2_STD::vector<int>({0, 1, 2, 3, 4}));
        auto range2 = in | RAH2_NS::views::take(1000);
        RAH2_STD::vector<int> out2;
        RAH2_STD::copy(
            RAH2_NS::ranges::begin(range2),
            RAH2_NS::ranges::end(range2),
            RAH2_STD::back_inserter(out2));
        assert(out2 == RAH2_STD::vector<int>({0, 1, 2, 3, 4, 5, 6, 7, 8, 9}));
        /// [take_pipeable]
    }

    testSuite.test_case("concepts");
    foreach_range_combination<test_range<make_take_view>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct make_drop_view
{
    auto make()
    {
        return RAH2_NS::views::drop(make_test_view<CS, Tag, Sized>(), 2);
    }
    using base_type = test_view<CS, Tag, Sized>;
    static constexpr bool is_sized = Sized;
    static constexpr bool is_common = RAH2_NS::ranges::common_range<base_type>;
    static constexpr bool do_test = true;
    static constexpr bool is_borrowed = RAH2_NS::ranges::enable_borrowed_range<base_type>;
    using expected_cat =
        RAH2_NS::ranges::cap_iterator_tag<Tag, RAH2_STD::input_iterator_tag, RAH2_NS::contiguous_iterator_tag>;
};
void test_drop_view()
{
    {
        testSuite.test_case("sample");

        /// [drop]
        RAH2_STD::vector<int> in{0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
        auto range = RAH2_NS::views::drop(in, 6);
        RAH2_STD::vector<int> out;
        RAH2_STD::copy(
            RAH2_NS::ranges::begin(range), RAH2_NS::ranges::end(range), RAH2_STD::back_inserter(out));
        assert(out == RAH2_STD::vector<int>({6, 7, 8, 9}));
        /// [drop]
    }

    {
        testSuite.test_case("sample_pipeable");
        /// [drop_pipeable]
        RAH2_STD::vector<int> in{0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
        auto range = in | RAH2_NS::views::drop(6);
        RAH2_STD::vector<int> out;
        RAH2_STD::copy(
            RAH2_NS::ranges::begin(range), RAH2_NS::ranges::end(range), RAH2_STD::back_inserter(out));
        assert(out == RAH2_STD::vector<int>({6, 7, 8, 9}));
        /// [drop_pipeable]
    }
    testSuite.test_case("concepts");
    foreach_range_combination<test_range<make_drop_view>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct make_drop_while_view
{
    auto make()
    {
        return RAH2_NS::views::drop_while(
            make_test_view<CS, Tag, Sized>(), [](auto i) { return i < 4; });
    }
    using V = test_view<CS, Tag, Sized>;
    static constexpr bool is_sized =
        RAH2_NS::ranges::forward_range<V>
        && RAH2_NS::sized_sentinel_for<RAH2_NS::ranges::sentinel_t<V>, RAH2_NS::ranges::iterator_t<V>>;
    static constexpr bool is_common = RAH2_NS::ranges::common_range<V>;
    static constexpr bool do_test = true;
    static constexpr bool is_borrowed = RAH2_NS::ranges::enable_borrowed_range<V>;
    using expected_cat =
        RAH2_NS::ranges::cap_iterator_tag<Tag, RAH2_STD::input_iterator_tag, RAH2_NS::contiguous_iterator_tag>;
};
void test_drop_while_view()
{
    {
        testSuite.test_case("sample");
        /// [drop_while]
        RAH2_STD::vector<int> in{0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
        auto range = RAH2_NS::views::drop_while(in, [](auto v) { return v < 6; });
        RAH2_STD::vector<int> out;
        RAH2_STD::copy(
            RAH2_NS::ranges::begin(range), RAH2_NS::ranges::end(range), RAH2_STD::back_inserter(out));
        assert(out == RAH2_STD::vector<int>({6, 7, 8, 9}));
        /// [drop_while]
    }

    {
        testSuite.test_case("sample_pipeable");
        /// [drop_while_pipeable]
        RAH2_STD::vector<int> in{0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
        auto range = in | RAH2_NS::views::drop_while([](auto v) { return v < 6; });
        RAH2_STD::vector<int> out;
        RAH2_STD::copy(
            RAH2_NS::ranges::begin(range), RAH2_NS::ranges::end(range), RAH2_STD::back_inserter(out));
        assert(out == RAH2_STD::vector<int>({6, 7, 8, 9}));
        /// [drop_while_pipeable]
    }

    testSuite.test_case("concepts");
    foreach_range_combination<test_range<make_drop_while_view>>();
}

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
        RAH2_NS::ranges::cap_iterator_tag<Tag, RAH2_STD::input_iterator_tag, RAH2_NS::input_iterator_tag>;
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
        auto range = RAH2_NS::views::iota<size_t>(0, 6)
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
        RAH2_NS::ranges::cap_iterator_tag<Tag, RAH2_STD::input_iterator_tag, RAH2_NS::input_iterator_tag>;
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
    RAH2_STD::array<int, 2> delim = {3, 4};
    auto make()
    {
        auto r = make_test_view<CS, Tag, Sized>();
        return RAH2_NS::views::counted(r.begin(), 8);
    }
    using BaseRange = test_view<CS, Tag, Sized>;
    static constexpr bool is_sized = RAH2_NS::ranges::random_access_range<BaseRange>;
    static constexpr bool is_common =
        RAH2_NS::random_access_iterator<RAH2_NS::ranges::iterator_t<BaseRange>>;
    static constexpr bool do_test = true;
    static constexpr bool is_borrowed = true; // It is actually a subrange
    using expected_cat =
        RAH2_NS::ranges::cap_iterator_tag<Tag, RAH2_STD::input_iterator_tag, RAH2_NS::contiguous_iterator_tag>;
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
    using base_cat = RAH2_NS::ranges::range_iter_categ_t<BaseRange>;
    using expected_cat = RAH2_STD::conditional_t<
        RAH2_NS::ranges::common_range<BaseRange>,
        base_cat,
        RAH2_STD::conditional_t<
            RAH2_NS::ranges::sized_range<BaseRange> && RAH2_NS::ranges::random_access_range<BaseRange>,
            base_cat,
            RAH2_NS::ranges::cap_iterator_tag<base_cat, RAH2_NS::input_iterator_tag, RAH2_NS::bidirectional_iterator_tag>>>;
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
        RAH2_NS::ranges::sized_range<BaseRange>
        || RAH2_NS::random_access_iterator<RAH2_NS::ranges::iterator_t<BaseRange>>;
    static constexpr bool is_common = true;
    static constexpr bool do_test = RAH2_NS::ranges::bidirectional_range<BaseRange>;
    static constexpr bool is_borrowed = RAH2_NS::ranges::enable_borrowed_range<BaseRange>;
    using expected_cat =
        RAH2_NS::ranges::cap_iterator_tag<Tag, RAH2_STD::input_iterator_tag, RAH2_NS::random_access_iterator_tag>;
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
    auto make()
    {
        static RAH2_STD::vector<RAH2_STD::tuple<bool, char, int>> vec{
            {true, 'a', 1000},
            {false, 'b', 1001},
            {true, 'c', 1002},
            {false, 'd', 1003},
        };
        return RAH2_NS::views::elements<2>(make_test_view_adapter<CS, Tag, Sized>(vec));
    }
    using BaseRange =
        test_view_adapter<CS, Tag, Sized, RAH2_STD::vector<RAH2_STD::tuple<bool, char, int>>>;
    static constexpr bool is_sized = RAH2_NS::ranges::sized_range<BaseRange>;
    static constexpr bool is_common = RAH2_NS::ranges::common_range<BaseRange>;
    static constexpr bool do_test = true;
    static constexpr bool is_borrowed = RAH2_NS::ranges::enable_borrowed_range<BaseRange>;
    using expected_cat =
        RAH2_NS::ranges::cap_iterator_tag<Tag, RAH2_STD::input_iterator_tag, RAH2_NS::random_access_iterator_tag>;
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
    auto make()
    {
        static RAH2_STD::vector<RAH2_STD::pair<int, std::string>> m{
            {12, "aaa"},
            {19, "bbb"},
            {25, "ccc"},
            {5, "ddd"},
        };
        return RAH2_NS::views::values(make_test_view_adapter<CS, Tag, Sized>(m));
    }
    using BaseRange =
        test_view_adapter<CS, Tag, Sized, RAH2_STD::vector<RAH2_STD::tuple<bool, char, int>>>;
    static constexpr bool is_sized = RAH2_NS::ranges::sized_range<BaseRange>;
    static constexpr bool is_common = RAH2_NS::ranges::common_range<BaseRange>;
    static constexpr bool do_test = true;
    static constexpr bool is_borrowed = RAH2_NS::ranges::enable_borrowed_range<BaseRange>;
    using expected_cat =
        RAH2_NS::ranges::cap_iterator_tag<Tag, RAH2_STD::input_iterator_tag, RAH2_NS::random_access_iterator_tag>;
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
    auto make()
    {
        static RAH2_STD::vector<RAH2_STD::pair<int, std::string>> m{
            {12, "aaa"},
            {19, "bbb"},
            {25, "ccc"},
            {5, "ddd"},
        };
        return RAH2_NS::views::keys(make_test_view_adapter<CS, Tag, Sized>(m));
    }
    using BaseRange =
        test_view_adapter<CS, Tag, Sized, RAH2_STD::vector<RAH2_STD::tuple<bool, char, int>>>;
    static constexpr bool is_sized = RAH2_NS::ranges::sized_range<BaseRange>;
    static constexpr bool is_common = RAH2_NS::ranges::common_range<BaseRange>;
    static constexpr bool do_test = true;
    static constexpr bool is_borrowed = RAH2_NS::ranges::enable_borrowed_range<BaseRange>;
    using expected_cat =
        RAH2_NS::ranges::cap_iterator_tag<Tag, RAH2_STD::input_iterator_tag, RAH2_NS::random_access_iterator_tag>;
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

template <CommonOrSent CS, typename Tag, bool Sized>
struct make_enumerate_view
{
    auto make()
    {
        return RAH2_NS::views::enumerate(make_test_view<CS, Tag, Sized>());
    }
    using BaseRange = test_view<CS, Tag, Sized>;
    static constexpr bool is_sized = RAH2_NS::ranges::sized_range<BaseRange>;
    static constexpr bool is_common =
        RAH2_NS::ranges::common_range<BaseRange> && RAH2_NS::ranges::sized_range<BaseRange>;
    static constexpr bool do_test = true;
    static constexpr bool is_borrowed = RAH2_NS::ranges::enable_borrowed_range<BaseRange>;
    using expected_cat =
        RAH2_NS::ranges::cap_iterator_tag<Tag, RAH2_STD::input_iterator_tag, RAH2_NS::random_access_iterator_tag>;
};
void test_enumerate_view()
{
    {
        testSuite.test_case("sample");
        /// [enumerate]
        RAH2_STD::vector<int> input{4, 5, 6, 7};
        RAH2_STD::vector<RAH2_STD::tuple<intptr_t, int>> result;
        RAH2_NS::views::enumerate(input);
        for (auto i_value : RAH2_NS::views::enumerate(input))
            result.emplace_back(i_value);
        assert(
            result
            == (RAH2_STD::vector<RAH2_STD::tuple<intptr_t, int>>{{0, 4}, {1, 5}, {2, 6}, {3, 7}}));
        /// [enumerate]
    }
    {
        testSuite.test_case("sample_pipeable");
        /// [enumerate_pipeable]
        RAH2_STD::vector<int> input{4, 5, 6, 7};
        RAH2_STD::vector<RAH2_STD::tuple<intptr_t, int>> result;
        for (auto i_value : input | RAH2_NS::views::enumerate | RAH2_NS::views::common)
            result.emplace_back(i_value);
        assert(
            result
            == (RAH2_STD::vector<RAH2_STD::tuple<intptr_t, int>>{{0, 4}, {1, 5}, {2, 6}, {3, 7}}));
        /// [enumerate_pipeable]
    }

    {
        testSuite.test_case("various");
        // This can't work since enumerate return an rvalue pairs since map_key want an lvalue
        bool bools[] = {false, true, true, false, false, true};
        auto range =
            bools | RAH2_NS::views::enumerate
            | RAH2_NS::views::filter([](auto&& index_bool) { return RAH2_STD::get<1>(index_bool); })
            | RAH2_NS::views::keys;

        RAH2_STD::vector<intptr_t> ref;
        RAH2_NS::ranges::copy(range, RAH2_NS::back_inserter(ref));
        assert(ref == (RAH2_STD::vector<intptr_t>{1, 2, 5}));
    }

    testSuite.test_case("concepts");
    foreach_range_combination<test_range<make_enumerate_view>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct make_adjacent_view
{
    auto make()
    {
        return RAH2_NS::views::adjacent<3>(make_test_view<CS, Tag, Sized>());
    }
    using BaseRange = test_view<CS, Tag, Sized>;
    static constexpr bool is_sized = RAH2_NS::ranges::sized_range<BaseRange>;
    static constexpr bool is_common = RAH2_NS::ranges::common_range<BaseRange>;
    static constexpr bool do_test = RAH2_NS::ranges::forward_range<BaseRange>;
    static constexpr bool is_borrowed = RAH2_NS::ranges::enable_borrowed_range<BaseRange>;
    using expected_cat =
        RAH2_NS::ranges::cap_iterator_tag<Tag, RAH2_STD::forward_iterator_tag, RAH2_NS::random_access_iterator_tag>;
};
void test_adjacent_view()
{
    {
        testSuite.test_case("sample");
        /// [adjacent]
        RAH2_STD::vector<int> in{0, 1, 2, 3, 4, 5};
        RAH2_STD::vector<RAH2_STD::vector<int>> out;
        for (auto&& abc : RAH2_NS::views::adjacent<3>(in))
        {
            out.push_back({RAH2_STD::get<0>(abc), RAH2_STD::get<1>(abc), RAH2_STD::get<2>(abc)});
        }
        assert(
            out
            == (RAH2_STD::vector<RAH2_STD::vector<int>>{{0, 1, 2}, {1, 2, 3}, {2, 3, 4}, {3, 4, 5}}));
        /// [adjacent]
    }
    {
        testSuite.test_case("non_common");
        // adjacent With non common_range
        RAH2_STD::vector<RAH2_STD::vector<int>> out;
        for (auto&& abc : RAH2_NS::views::iota(0) | RAH2_NS::views::take(6)
                              | RAH2_NS::views::adjacent<3> | RAH2_NS::views::common)
        {
            out.push_back({RAH2_STD::get<0>(abc), RAH2_STD::get<1>(abc), RAH2_STD::get<2>(abc)});
        }
        assert(
            out
            == (RAH2_STD::vector<RAH2_STD::vector<int>>{{0, 1, 2}, {1, 2, 3}, {2, 3, 4}, {3, 4, 5}}));
    }
    {
        testSuite.test_case("N > size()");
        // adjacent With N > view.size()
        RAH2_STD::vector<RAH2_STD::vector<int>> out;
        for (auto&& abc : RAH2_NS::views::iota(0) | RAH2_NS::views::take(6)
                              | RAH2_NS::views::adjacent<45> | RAH2_NS::views::common)
        {
            out.push_back({RAH2_STD::get<0>(abc), RAH2_STD::get<1>(abc), RAH2_STD::get<2>(abc)});
        }
        assert(out.empty());
    }
    {
        testSuite.test_case("N == 0");
        // adjacent With N == 0
        RAH2_STD::vector<RAH2_STD::vector<int>> out;
        for (auto&& abc :
             RAH2_NS::views::iota(0) | RAH2_NS::views::take(6) | RAH2_NS::views::adjacent<0>)
        {
            static_assert(
                RAH2_STD::tuple_size<RAH2_STD::remove_reference_t<decltype(abc)>>::value == 0,
                "tuple should be empty");
            out.emplace_back();
        }
        assert(out.empty());
    }

    testSuite.test_case("concepts");
    foreach_range_combination<test_range<make_adjacent_view>>();
}

struct Add
{
    auto operator()(int i) const
    {
        return i;
    }
    template <typename... Args>
    auto operator()(int i, Args... ints) const
    {
        return i + (*this)(ints...);
    }
};
template <CommonOrSent CS, typename Tag, bool Sized>
struct make_adjacent_transform_view
{
    auto make()
    {
        return RAH2_NS::views::adjacent_transform<3>(
            make_test_view<CS, Tag, Sized>(), [](auto a, auto b, auto c) { return a + b + c; });
    }
    using BaseRange = test_view<CS, Tag, Sized>;
    static constexpr bool is_sized = RAH2_NS::ranges::sized_range<BaseRange>;
    static constexpr bool is_common = RAH2_NS::ranges::common_range<BaseRange>;
    static constexpr bool do_test = RAH2_NS::ranges::forward_range<BaseRange>;
    static constexpr bool is_borrowed = RAH2_NS::ranges::enable_borrowed_range<BaseRange>;
    using expected_cat =
        RAH2_NS::ranges::cap_iterator_tag<Tag, RAH2_STD::forward_iterator_tag, RAH2_NS::random_access_iterator_tag>;
};
void test_adjacent_transform()
{
    {
        testSuite.test_case("sample");
        /// [adjacent_transform]
        RAH2_STD::vector<int> in{0, 1, 2, 3, 4, 5};
        RAH2_STD::vector<int> out;
        for (auto abc : RAH2_NS::views::adjacent_transform<3>(
                            in, [](auto a, auto b, auto c) { return a + b + c; })
                            | RAH2_NS::views::common)
        {
            out.push_back(abc);
        }
        assert(out == (RAH2_STD::vector<int>{3, 6, 9, 12}));
        /// [adjacent_transform]
    }
    {
        testSuite.test_case("non common_range");
        // adjacent_transform With non common_range
        RAH2_STD::vector<int> out;
        for (auto abc : RAH2_NS::views::iota(0) | RAH2_NS::views::take(6)
                            | RAH2_NS::views::adjacent_transform<3>([](auto a, auto b, auto c)
                                                                    { return a + b + c; })
                            | RAH2_NS::views::common)
        {
            out.push_back(abc);
        }
        assert(out == (RAH2_STD::vector<int>{3, 6, 9, 12}));
    }
    {
        testSuite.test_case("N > size()");
        // adjacent_transform With N > view.size()
        RAH2_STD::vector<int> out;
        for (auto abc : RAH2_NS::views::iota(0) | RAH2_NS::views::take(6)
                            | RAH2_NS::views::adjacent_transform<45>(Add{}) | RAH2_NS::views::common)
        {
            out.push_back(abc);
        }
        assert(out.empty());
    }
    {
        testSuite.test_case("N == 0");
        // adjacent_transform With N == 0
        RAH2_STD::vector<RAH2_STD::vector<int>> out;
        for (auto&& abc : RAH2_NS::views::iota(0) | RAH2_NS::views::take(6)
                              | RAH2_NS::views::adjacent_transform<0>([](auto i) { return i + 1; }))
        {
            static_assert(
                RAH2_STD::tuple_size<RAH2_STD::remove_reference_t<decltype(abc)>>::value == 0,
                "tuple should be empty");
            out.emplace_back();
        }
        assert(out.empty());
    }
    testSuite.test_case("concepts");
    foreach_range_combination<test_range<make_adjacent_transform_view>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct make_slide_view
{
    auto make()
    {
        return RAH2_NS::views::slide(make_test_view<CS, Tag, Sized>(), 3);
    }
    using BaseRange = test_view<CS, Tag, Sized>;
    static constexpr bool is_sized = RAH2_NS::ranges::sized_range<BaseRange>;
    static constexpr bool is_common =
        RAH2_NS::ranges::common_range<BaseRange> && RAH2_NS::ranges::bidirectional_range<BaseRange>;
    static constexpr bool do_test = RAH2_NS::ranges::forward_range<BaseRange>;
    static constexpr bool is_borrowed = false;
    using expected_cat =
        RAH2_NS::ranges::cap_iterator_tag<Tag, RAH2_STD::input_iterator_tag, RAH2_NS::random_access_iterator_tag>;
};
void test_slide_view()
{
    {
        testSuite.test_case("sample");
        /// [sliding]
        RAH2_STD::vector<int> in{0, 1, 2, 3, 4, 5};
        RAH2_STD::vector<RAH2_STD::vector<int>> out;
        for (auto subRange : RAH2_NS::views::slide(in, 3))
        {
            out.emplace_back();
            RAH2_STD::copy(
                RAH2_NS::ranges::begin(subRange),
                RAH2_NS::ranges::end(subRange),
                RAH2_STD::back_inserter(out.back()));
        }
        assert(
            out
            == (RAH2_STD::vector<RAH2_STD::vector<int>>{{0, 1, 2}, {1, 2, 3}, {2, 3, 4}, {3, 4, 5}}));
        /// [sliding]
    }
    {
        testSuite.test_case("non common_range");
        // slide with non common_range
        RAH2_STD::vector<RAH2_STD::vector<int>> out;
        auto r = RAH2_NS::views::iota(0) | RAH2_NS::views::take(6) | RAH2_NS::views::slide(3);
        auto it = RAH2_NS::ranges::begin(r);
        auto e = RAH2_NS::ranges::end(r);
        for (; it != e; ++it)
        {
            out.emplace_back();
            auto&& subRange = *it;
            RAH2_NS::ranges::copy(subRange, RAH2_NS::back_inserter(out.back()));
        }
        assert(
            out
            == (RAH2_STD::vector<RAH2_STD::vector<int>>{{0, 1, 2}, {1, 2, 3}, {2, 3, 4}, {3, 4, 5}}));
    }

    {
        testSuite.test_case("sample_pipeable");
        /// [sliding_pipeable]
        RAH2_STD::vector<int> in{0, 1, 2, 3, 4, 5};
        RAH2_STD::vector<RAH2_STD::vector<int>> out;
        for (auto subRange : in | RAH2_NS::views::slide(3))
        {
            out.emplace_back();
            RAH2_STD::copy(
                RAH2_NS::ranges::begin(subRange),
                RAH2_NS::ranges::end(subRange),
                RAH2_STD::back_inserter(out.back()));
        }
        assert(
            out
            == (RAH2_STD::vector<RAH2_STD::vector<int>>{{0, 1, 2}, {1, 2, 3}, {2, 3, 4}, {3, 4, 5}}));
        /// [sliding_pipeable]
    }

    {
        testSuite.test_case("various");
        RAH2_STD::vector<int> in{0, 1, 2, 3, 4, 5};
        RAH2_STD::vector<RAH2_STD::vector<int>> out;
        auto range =
            in | RAH2_NS::views::cycle | RAH2_NS::views::slide(3) | RAH2_NS::views::take(in.size());
        for (auto subRange : range | RAH2_NS::views::common)
        {
            out.emplace_back();
            RAH2_STD::copy(
                RAH2_NS::ranges::begin(subRange),
                RAH2_NS::ranges::end(subRange),
                RAH2_STD::back_inserter(out.back()));
        }
        assert(
            out
            == (RAH2_STD::vector<RAH2_STD::vector<int>>{
                {0, 1, 2},
                {1, 2, 3},
                {2, 3, 4},
                {3, 4, 5},
                {4, 5, 0},
                {5, 0, 1},
            }));
    }

    {
        RAH2_STD::vector<int> in{0, 1, 2, 3};
        RAH2_STD::vector<RAH2_STD::vector<int>> out;
        for (auto subRange : RAH2_NS::views::slide(in, 4))
        {
            out.emplace_back();
            RAH2_STD::copy(
                RAH2_NS::ranges::begin(subRange),
                RAH2_NS::ranges::end(subRange),
                RAH2_STD::back_inserter(out.back()));
        }
        assert(out == (RAH2_STD::vector<RAH2_STD::vector<int>>{{0, 1, 2, 3}}));
    }

    {
        RAH2_STD::vector<int> in{0, 1};
        RAH2_STD::vector<RAH2_STD::vector<int>> out;
        for (auto subRange : RAH2_NS::views::slide(in, 4))
        {
            out.emplace_back();
            RAH2_STD::copy(
                RAH2_NS::ranges::begin(subRange),
                RAH2_NS::ranges::end(subRange),
                RAH2_STD::back_inserter(out.back()));
        }
        assert(out.empty());
    }

    {
        RAH2_STD::vector<int> in{0, 1, 2, 3};
        RAH2_STD::vector<RAH2_STD::vector<int>> out;
        for (auto subRange : RAH2_NS::views::slide(in, 0))
        {
            out.emplace_back();
            RAH2_STD::copy(
                RAH2_NS::ranges::begin(subRange),
                RAH2_NS::ranges::end(subRange),
                RAH2_STD::back_inserter(out.back()));
        }
        assert(out == (RAH2_STD::vector<RAH2_STD::vector<int>>{{0}, {1}, {2}, {3}}));
    }

    {
        RAH2_STD::vector<int> in{0, 1, 2, 3};
        RAH2_STD::vector<RAH2_STD::vector<int>> out;
        for (auto subRange : in | RAH2_NS::views::slide(1))
        {
            out.emplace_back();
            RAH2_STD::copy(
                RAH2_NS::ranges::begin(subRange),
                RAH2_NS::ranges::end(subRange),
                RAH2_STD::back_inserter(out.back()));
        }
        assert(
            out
            == (RAH2_STD::vector<RAH2_STD::vector<int>>{
                {0},
                {1},
                {2},
                {3},
            }));
    }
    testSuite.test_case("concepts");
    foreach_range_combination<test_range<make_slide_view>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct make_chunk_view
{
    auto make()
    {
        return RAH2_NS::views::chunk(make_test_view<CS, Tag, Sized>(), 3);
    }
    using BaseRange = test_view<CS, Tag, Sized>;
    static constexpr bool is_sized = RAH2_NS::ranges::sized_range<BaseRange>;
    static constexpr bool is_common = RAH2_NS::ranges::common_range<BaseRange>;
    static constexpr bool do_test = true;
    static constexpr bool is_borrowed = false;
    using expected_cat =
        RAH2_NS::ranges::cap_iterator_tag<Tag, RAH2_STD::input_iterator_tag, RAH2_NS::random_access_iterator_tag>;
};
void test_chunk_view()
{
    {
        testSuite.test_case("sample");
        /// [chunk]
        RAH2_STD::vector<int> vec_01234{0, 1, 2, 3, 4};
        RAH2_STD::vector<RAH2_STD::vector<int>> result;
        for (auto elts : RAH2_NS::views::chunk(vec_01234, 2))
            result.emplace_back(RAH2_NS::ranges::begin(elts), RAH2_NS::ranges::end(elts));
        assert(result == RAH2_STD::vector<RAH2_STD::vector<int>>({{0, 1}, {2, 3}, {4}}));
        /// [chunk]
    }
    {
        testSuite.test_case("sample_pipeable");
        /// [chunk_pipeable]
        RAH2_STD::vector<int> vec_01234{0, 1, 2, 3, 4};
        RAH2_STD::vector<RAH2_STD::vector<int>> result;
        for (auto elts : vec_01234 | RAH2_NS::views::chunk(2))
            result.emplace_back(RAH2_NS::ranges::begin(elts), RAH2_NS::ranges::end(elts));
        assert(result == RAH2_STD::vector<RAH2_STD::vector<int>>({{0, 1}, {2, 3}, {4}}));
        /// [chunk_pipeable]
    }
    {
        testSuite.test_case("non-common_view");
        /// Chunk with non-common_view
        auto vec_01234 = RAH2_NS::views::iota(0) | RAH2_NS::views::take(5);
        RAH2_STD::vector<RAH2_STD::vector<int>> result;
        for (auto elts : RAH2_NS::views::chunk(vec_01234, 2) | RAH2_NS::views::common)
        {
            result.emplace_back();
            auto& back = result.back();
            for (auto i : elts | RAH2_NS::views::common)
            {
                back.emplace_back(i);
            }
        }
        assert(result == RAH2_STD::vector<RAH2_STD::vector<int>>({{0, 1}, {2, 3}, {4}}));
    }
    testSuite.test_case("concepts");
    foreach_range_combination<test_range<make_chunk_view>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct make_stride_view
{
    auto make()
    {
        return RAH2_NS::views::stride(make_test_view<CS, Tag, Sized>(), 3);
    }
    using BaseRange = test_view<CS, Tag, Sized>;
    static constexpr bool is_sized = RAH2_NS::ranges::sized_range<BaseRange>;
    static constexpr bool is_common = RAH2_NS::ranges::common_range<BaseRange>
                                      && (RAH2_NS::ranges::sized_range<BaseRange>
                                          || !RAH2_NS::ranges::bidirectional_range<BaseRange>);
    static constexpr bool do_test = true;
    static constexpr bool is_borrowed = RAH2_NS::ranges::enable_borrowed_range<BaseRange>;
    using expected_cat =
        RAH2_NS::ranges::cap_iterator_tag<Tag, RAH2_STD::input_iterator_tag, RAH2_NS::random_access_iterator_tag>;
};
void test_stride_view()
{
    {
        testSuite.test_case("sample");
        /// [stride]
        RAH2_STD::vector<int> vec{0, 1, 2, 3, 4, 5, 6, 7};
        RAH2_STD::vector<int> result;
        for (int const i : RAH2_NS::views::stride(vec, 2))
            result.push_back(i);
        assert(result == RAH2_STD::vector<int>({0, 2, 4, 6}));
        /// [stride]
    }
    {
        testSuite.test_case("sample_pipeable");
        /// [stride_pipeable]
        RAH2_STD::vector<int> vec{0, 1, 2, 3, 4, 5, 6, 7};
        RAH2_STD::vector<int> result;
        for (int const i : vec | RAH2_NS::views::stride(2))
            result.push_back(i);
        assert(result == RAH2_STD::vector<int>({0, 2, 4, 6}));
        /// [stride_pipeable]
    }

    testSuite.test_case("concepts");
    foreach_range_combination<test_range<make_stride_view>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct make_ref_view
{
    using BaseRange = test_view<CS, Tag, Sized>;
    BaseRange base;
    auto make()
    {
        return RAH2_NS::views::ref(base);
    }
    static constexpr bool is_sized = RAH2_NS::ranges::sized_range<BaseRange>;
    static constexpr bool is_common = RAH2_NS::ranges::common_range<BaseRange>;
    static constexpr bool do_test = true;
    static constexpr bool is_borrowed = true;
    using expected_cat =
        RAH2_NS::ranges::cap_iterator_tag<Tag, RAH2_STD::input_iterator_tag, RAH2_NS::contiguous_iterator_tag>;
};
void test_ref_view()
{
    {
        testSuite.test_case("sample");
        /// [ref]
        RAH2_STD::vector<int> vec{0, 1, 2, 2, 3};
        RAH2_STD::vector<int> out;
        auto const ref = RAH2_NS::views::ref(vec);
        for (auto&& val : ref)
        {
            out.push_back(val);
        }
        assert(out == (RAH2_STD::vector<int>{0, 1, 2, 2, 3}));
        /// [ref]
    }

    testSuite.test_case("concepts");
    foreach_range_combination<test_range<make_ref_view>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct make_subrange
{
    using BaseRange = test_view<CS, Tag, Sized>;
    BaseRange base;
    auto make()
    {
        return RAH2_NS::ranges::make_subrange(base.begin(), base.end());
    }
    static constexpr bool is_sized = RAH2_NS::sized_sentinel_for<
        RAH2_NS::ranges::sentinel_t<BaseRange>,
        RAH2_NS::ranges::iterator_t<BaseRange>>;
    static constexpr bool is_common = RAH2_NS::ranges::common_range<BaseRange>;
    static constexpr bool do_test = true;
    static constexpr bool is_borrowed = true;
    using expected_cat = Tag;
};
void test_subrange()
{
    {
        testSuite.test_case("sample");
        /// [subrange]
        RAH2_STD::vector<int> vec{0, 1, 2, 2, 3};
        auto sub = RAH2_NS::ranges::make_subrange(vec.begin() + 1, vec.begin() + 4);
        RAH2_STD::vector<int> out;
        for (auto&& val : sub)
        {
            out.push_back(val);
        }
        assert(out == (RAH2_STD::vector<int>{1, 2, 2}));
        /// [subrange]
    }

    testSuite.test_case("concepts");
    foreach_range_combination<test_range<make_subrange>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct make_unbounded_view
{
    using BaseRange = test_view<CS, Tag, Sized>;
    BaseRange base;
    auto make()
    {
        return RAH2_NS::views::unbounded(RAH2_NS::ranges::begin(base));
    }
    static constexpr bool is_sized = false;
    static constexpr bool is_common = false;
    static constexpr bool do_test = true;
    static constexpr bool is_borrowed = true;
    using expected_cat =
        RAH2_NS::ranges::cap_iterator_tag<Tag, RAH2_STD::input_iterator_tag, RAH2_NS::contiguous_iterator_tag>;
};
void test_unbounded_view()
{
    {
        testSuite.test_case("sample");
        /// [unbounded_view]
        RAH2_STD::vector<int> vec{0, 1, 2, 2, 3};
        RAH2_STD::vector<int> out;
        auto v = RAH2_NS::views::unbounded(RAH2_NS::ranges::begin(vec));
        for (auto&& val : v | RAH2_NS::views::take(5) | RAH2_NS::views::common)
        {
            out.push_back(val);
        }
        assert(out == (RAH2_STD::vector<int>{0, 1, 2, 2, 3}));
        /// [unbounded_view]
    }

    testSuite.test_case("concepts");
    foreach_range_combination<test_range<make_unbounded_view>>();
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

template <CommonOrSent CS, typename Tag, bool Sized>
struct make_cycle_view
{
    using BaseRange = test_view<CS, Tag, Sized>;
    auto make()
    {
        return RAH2_NS::views::cycle(make_test_view<CS, Tag, Sized>());
    }
    static constexpr bool is_sized = false;
    static constexpr bool is_common = false;
    static constexpr bool do_test = RAH2_NS::ranges::forward_range<BaseRange>;
    static constexpr bool is_borrowed = false;
    using expected_cat = RAH2_STD::conditional_t<
        CS == CommonOrSent::Common,
        RAH2_NS::ranges::cap_iterator_tag<Tag, RAH2_STD::input_iterator_tag, RAH2_NS::bidirectional_iterator_tag>,
        RAH2_NS::ranges::cap_iterator_tag<Tag, RAH2_STD::input_iterator_tag, RAH2_NS::forward_iterator_tag>>;
};
void test_cycle_view()
{
    {
        testSuite.test_case("sample");
        /// [cycle]
        RAH2_STD::vector<int> in{0, 1, 2};
        auto cy = RAH2_NS::views::cycle(in);
        RAH2_STD::vector<int> out;
        RAH2_STD::copy_n(cy.begin(), 8, RAH2_STD::back_inserter(out));
        assert(out == RAH2_STD::vector<int>({0, 1, 2, 0, 1, 2, 0, 1}));
        /// [cycle]
        STATIC_ASSERT((RAH2_NS::ranges::bidirectional_range<decltype(cy)>));
        STATIC_ASSERT((not RAH2_NS::ranges::random_access_range<decltype(cy)>));
    }

    {
        /// [cycle_pipeable]
        RAH2_STD::vector<int> in{0, 1, 2};
        auto cy = in | RAH2_NS::views::cycle;
        RAH2_STD::vector<int> out;
        RAH2_STD::copy_n(cy.begin(), 8, RAH2_STD::back_inserter(out));
        assert(out == RAH2_STD::vector<int>({0, 1, 2, 0, 1, 2, 0, 1}));
        /// [cycle_pipeable]
    }

    {
        RAH2_STD::vector<int> in{0, 1, 2};
        auto cy = RAH2_NS::views::cycle(in) | RAH2_NS::views::take(8);
        RAH2_STD::vector<int> out;
        // static_assert(RAH2_NS::range<decltype(RAH2_NS::back_inserter(out))>, "dkjh");
        RAH2_NS::ranges::copy(cy, RAH2_NS::back_inserter(out));
        assert(out == RAH2_STD::vector<int>({0, 1, 2, 0, 1, 2, 0, 1}));
    }

    testSuite.test_case("concepts");
    foreach_range_combination<test_range<make_cycle_view>>();
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
        (void)check_range_cat<RAH2_STD::input_iterator_tag, decltype(gen)>{};
    }
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct make_slice_view
{
    using BaseRange = test_view<CS, Tag, Sized>;
    auto make()
    {
        return RAH2_NS::views::slice(make_test_view<CS, Tag, Sized>(), 1, 6);
    }
    static constexpr bool is_sized = RAH2_NS::ranges::sized_range<BaseRange>;
    static constexpr bool is_common =
        RAH2_NS::ranges::random_access_range<BaseRange> && RAH2_NS::ranges::sized_range<BaseRange>;
    static constexpr bool do_test = true;
    static constexpr bool is_borrowed = false;
    using expected_cat = Tag;
};
void test_slice_view()
{
    {
        testSuite.test_case("sample");
        /// [slice]
        RAH2_STD::vector<int> vec{0, 1, 2, 3, 4, 5, 6, 7};
        RAH2_STD::vector<int> result;
        for (int i : RAH2_NS::views::slice(vec, 2, 6))
            result.push_back(i);
        assert(result == RAH2_STD::vector<int>({2, 3, 4, 5}));
        RAH2_STD::vector<int> result2;
        for (int i : RAH2_NS::views::slice(vec, 2, 6))
            result2.push_back(i);
        assert(result2 == RAH2_STD::vector<int>({2, 3, 4, 5}));
        /// [slice]
    }
    {
        /// [slice_pipeable]
        RAH2_STD::vector<int> vec{0, 1, 2, 3, 4, 5, 6, 7};
        RAH2_STD::vector<int> result;
        for (int i : vec | RAH2_NS::views::slice(2, 6))
            result.push_back(i);
        assert(result == RAH2_STD::vector<int>({2, 3, 4, 5}));
        /// [slice_pipeable]
    }

    testSuite.test_case("concepts");
    foreach_range_combination<test_range<make_slice_view>>();
}
