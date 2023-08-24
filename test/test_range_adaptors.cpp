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
    using expected_cat = RAH2_NS::ranges::details::
        cap_iterator_tag<Tag, RAH2_STD::input_iterator_tag, RAH2_NS::bidirectional_iterator_tag>;
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
    using expected_cat = RAH2_NS::ranges::details::
        cap_iterator_tag<Tag, RAH2_STD::input_iterator_tag, RAH2_NS::random_access_iterator_tag>;
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
    using expected_cat = RAH2_NS::ranges::details::
        cap_iterator_tag<Tag, RAH2_STD::input_iterator_tag, RAH2_NS::contiguous_iterator_tag>;
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
    using expected_cat = RAH2_NS::ranges::details::
        cap_iterator_tag<Tag, RAH2_STD::input_iterator_tag, RAH2_NS::contiguous_iterator_tag>;
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
    using expected_cat = RAH2_NS::ranges::details::
        cap_iterator_tag<Tag, RAH2_STD::input_iterator_tag, RAH2_NS::contiguous_iterator_tag>;
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
