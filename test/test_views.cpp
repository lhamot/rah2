#include <rah2/ranges.hpp>

#include <sstream>
#include <array>
#include <algorithm>

#include "test_helpers.hpp"

auto inputSentView = make_test_view<Sentinel, std::input_iterator_tag, false>();
auto fwdSentView = make_test_view<Sentinel, std::forward_iterator_tag, false>();
auto fwdCommonView = make_test_view<Common, std::forward_iterator_tag, false>();
auto bidirSentView = make_test_view<Sentinel, std::bidirectional_iterator_tag, false>();
auto bidirCommonView = make_test_view<Common, std::bidirectional_iterator_tag, false>();
auto rdmSentView = make_test_view<Sentinel, std::random_access_iterator_tag, true>();
auto rdmCommonView = make_test_view<Common, std::random_access_iterator_tag, true>();
auto contiSentView = make_test_view<Sentinel, rah2::contiguous_iterator_tag, true>();
auto contiCommonView = make_test_view<Common, rah2::contiguous_iterator_tag, true>();

extern TestSuite testSuite;

void test_counted_iterator()
{
    testSuite.test_case("sample", "");
    {
        auto iter = rah2::make_counted_iterator(begin(inputSentView), 10);
        STATIC_ASSERT(rah2::input_iterator<decltype(iter)>);
        STATIC_ASSERT(not rah2::forward_iterator<decltype(iter)>);
    }
    {
        auto iter = rah2::make_counted_iterator(begin(fwdSentView), 10);
        STATIC_ASSERT(rah2::forward_iterator<decltype(iter)>);
        STATIC_ASSERT(not rah2::bidirectional_iterator<decltype(iter)>);
    }
    {
        auto iter = rah2::make_counted_iterator(begin(fwdCommonView), 10);
        STATIC_ASSERT(rah2::forward_iterator<decltype(iter)>);
        STATIC_ASSERT(not rah2::bidirectional_iterator<decltype(iter)>);
    }
    {
        auto iter = rah2::make_counted_iterator(begin(bidirSentView), 10);
        STATIC_ASSERT((rah2::bidirectional_iterator_impl<decltype(iter), true>::value));
        STATIC_ASSERT(not rah2::random_access_iterator<decltype(iter)>);
    }
    {
        auto iter = rah2::make_counted_iterator(begin(bidirCommonView), 10);
        constexpr auto fsdkjfgqs = rah2::bidirectional_iterator_impl<decltype(iter), true>::value;
        STATIC_ASSERT(fsdkjfgqs);
        STATIC_ASSERT(rah2::bidirectional_iterator<decltype(iter)>);
        STATIC_ASSERT(not rah2::random_access_iterator<decltype(iter)>);
    }
    {
        auto iter = rah2::make_counted_iterator(begin(rdmSentView), 10);
        // STATIC_ASSERT(rah2::totally_ordered<decltype(iter)>);
        STATIC_ASSERT((rah2::random_access_iterator_impl<decltype(iter), true>::value));
        STATIC_ASSERT(rah2::random_access_iterator<decltype(iter)>);
        STATIC_ASSERT(not rah2::contiguous_iterator<decltype(iter)>);
    }
    {
        auto iter = rah2::make_counted_iterator(begin(rdmCommonView), 10);
        STATIC_ASSERT(rah2::random_access_iterator<decltype(iter)>);
        STATIC_ASSERT(not rah2::contiguous_iterator<decltype(iter)>);
    }
    {
        auto iter = rah2::make_counted_iterator(begin(contiSentView), 10);
        STATIC_ASSERT((rah2::contiguous_iterator_impl<decltype(iter), true>::value));
        STATIC_ASSERT(rah2::contiguous_iterator<decltype(iter)>);
    }
    {
        auto iter = rah2::make_counted_iterator(begin(contiCommonView), 10);
        STATIC_ASSERT(rah2::contiguous_iterator<decltype(iter)>);
    }
}

void test_empty_view()
{
    testSuite.test_case("sample");
    /// [empty]
    std::vector<int> result;
    for (int i : rah2::views::empty<int>())
        result.push_back(i);
    assert(result == std::vector<int>());
    /// [empty]

    testSuite.test_case("concept");
    STATIC_ASSERT((rah2::contiguous_range_impl<rah2::views::empty_view<int>, true>::value));
}

void test_single_view()
{
    testSuite.test_case("sample");
    /// [single]
    std::vector<int> result;
    for (int i : rah2::views::single(20))
        result.push_back(i);
    assert(result == std::vector<int>({20}));
    /// [single]

    testSuite.test_case("concept");
    STATIC_ASSERT(rah2::contiguous_range<rah2::views::single_view<int>>);
}

void test_iota_view()
{
    {
        testSuite.test_case("sample");
        /// [iota]
        std::vector<int> result;
        for (int i : rah2::views::iota(10, 15))
            result.push_back(i);
        assert(result == std::vector<int>({10, 11, 12, 13, 14}));
        /// [iota]
        STATIC_ASSERT(rah2::random_access_range<decltype(rah2::views::iota(10, 15))>);
    }

    {
        std::vector<int> result;
        for (int i : rah2::views::iota(10) | rah2::views::slice(2, 5))
            result.push_back(i);
        assert(result == std::vector<int>({12, 13, 14}));
    }

    {
        std::vector<size_t> result;
        for (size_t i : rah2::views::iota() | rah2::views::slice(2, 5))
            result.push_back(i);
        assert(result == std::vector<size_t>({2, 3, 4}));
    }
}

void test_istream_view()
{
    {
        testSuite.test_case("sample");
        /// [views::istream]
        std::stringstream ss("a b c d e f g h i j k l");
        std::vector<std::string> out;
        for (auto&& str : rah2::views::istream<std::string>(ss) | rah2::views::common())
        {
            out.push_back(str);
        }
        assert(
            out
            == std::vector<std::string>({"a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l"}));
        /// [views::istream]
    }

    testSuite.test_case("concepts");
    STATIC_ASSERT((rah2::input_range<decltype(rah2::views::istream<std::string>(
                       std::declval<std::stringstream&>()))>));
}

void test_repeat_view()
{
    testSuite.test_case("sample");
    /// [repeat]
    std::vector<int> out;
    auto range = rah2::views::repeat(42);
    std::copy_n(begin(range), 5, std::back_inserter(out));
    assert(out == std::vector<int>({42, 42, 42, 42, 42}));
    /// [repeat]
    STATIC_ASSERT((rah2::random_access_range_impl<decltype(range), true>::value));
}

void test_owning_view()
{
    testSuite.test_case("sample");
    /// [owning_view]
    std::vector<int> out;
    auto owning = rah2::views::owning(std::vector<int>{0, 1, 2, 2, 3});
    for (auto&& val : owning)
    {
        out.push_back(val);
    }
    assert(out == (std::vector<int>{0, 1, 2, 2, 3}));
    /// [owning_view]
    STATIC_ASSERT((rah2::random_access_range_impl<decltype(owning), true>::value));
}

void test_all_view()
{
    // Test all
    // A views can't embbed a container
    // EQUAL_RANGE((il<int>{ 0, 1, 2, 3 } | rah2::views::all()), (il<int>{ 0, 1, 2, 3 }));
    int intTab[] = {0, 1, 2, 3};
    testSuite.test_case("lvalue_container");
    EQUAL_RANGE((intTab | rah2::views::all()), (il<int>{0, 1, 2, 3}));
    testSuite.test_case("rvalue_container");
    EQUAL_RANGE((std::vector<int>({0, 1, 2, 3}) | rah2::views::all()), (il<int>{0, 1, 2, 3}));

    testSuite.test_case("sample");
    /// [views::all]
    std::vector<int> out;
    auto all = rah2::views::all(std::vector<int>{0, 1, 2, 2, 3});
    for (auto&& val : all)
    {
        out.push_back(val);
    }
    assert(out == (std::vector<int>{0, 1, 2, 2, 3}));
    /// [views::all]

    testSuite.test_case("concepts");
    STATIC_ASSERT((rah2::random_access_range_impl<decltype(all), true>::value));
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct make_filter_view
{
    auto make() const
    {
        return rah2::views::filter(
            make_test_view<CS, Tag, Sized>(), [](auto a) { return a % 2 == 0; });
    }
    static constexpr bool is_sized = false;
    static constexpr bool is_common = rah2::common_range<test_view<CS, Tag, Sized>>;
    static constexpr bool do_test = true;
    static constexpr bool is_borrowed = false;
};
void test_filter_view()
{
    testSuite.test_case("sample");
    /// [filter]
    std::vector<int> vec_01234{0, 1, 2, 3, 4, 5};
    std::vector<int> result;
    for (int i : rah2::views::filter(vec_01234, [](auto a) { return a % 2 == 0; }))
        result.push_back(i);
    assert(result == std::vector<int>({0, 2, 4}));
    /// [filter]

    testSuite.test_case("concepts");
    check_all_cat<rah2::bidirectional_iterator_tag, std::input_iterator_tag, make_filter_view>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct make_transform_view
{
    auto make()
    {
        auto v = make_test_view<CS, Tag, Sized>();
        AssertEqual<rah2::sized_range<decltype(v)>, Sized>();
        return rah2::views::transform(
            make_test_view<CS, Tag, Sized>(), [](auto a) { return a % 2 == 0; });
    }
    static constexpr bool is_sized = Sized;
    static constexpr bool is_common = rah2::common_range<test_view<CS, Tag, Sized>>;
    static constexpr bool do_test = true;
    static constexpr bool is_borrowed = false;
};

void test_transform_view()
{
    // Test transform
    {
        testSuite.test_case("sample");
        /// [rah2::views::transform]
        std::vector<int> vec{0, 1, 2, 3};
        std::vector<int> result;
        for (int i : rah2::views::transform(vec, [](auto a) { return a * 2; }))
            result.push_back(i);
        assert(result == std::vector<int>({0, 2, 4, 6}));
        /// [rah2::views::transform]
    }
    testSuite.test_case("various");
    {
        std::vector<int> vec{0, 1, 2, 3};
        std::vector<int> result;
        auto valueSelector = [](auto a)
        {
            return a * 2;
        };
        auto selectedValuesRange = rah2::views::transform(vec, valueSelector);
        auto bounds =
            std::minmax_element(rah2::begin(selectedValuesRange), rah2::end(selectedValuesRange));
        auto min = *bounds.first;
        assert(min == 0);
        auto max = *bounds.second;
        assert(max == 6); // 3 * 2
    }
    {
        /// [rah2::views::transform_pipeable]
        std::vector<int> vec{0, 1, 2, 3};
        std::vector<int> result;
        for (int i : vec | rah2::views::transform([](auto a) { return a * 2; }))
            result.push_back(i);
        assert(result == std::vector<int>({0, 2, 4, 6}));
        /// [rah2::views::transform_pipeable]
    }

    testSuite.test_case("concept");
    check_all_cat<rah2::random_access_iterator_tag, std::input_iterator_tag, make_transform_view>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct make_take_view
{
    auto make()
    {
        return rah2::views::take(make_test_view<CS, Tag, Sized>(), 8);
    }
    static constexpr bool is_sized = Sized;
    using R = test_view<CS, Tag, Sized>;
    static constexpr bool is_common = rah2::sized_range<R> && rah2::random_access_range<R>;
    static constexpr bool do_test = true;
    static constexpr bool is_borrowed = rah2::enable_borrowed_range<R>;
};
void test_take_view()
{
    {
        testSuite.test_case("sample");
        /// [take]
        std::vector<int> in{0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
        auto range = rah2::views::take(in, 5);
        std::vector<int> out;
        std::copy(rah2::begin(range), rah2::end(range), std::back_inserter(out));
        assert(out == std::vector<int>({0, 1, 2, 3, 4}));
        auto range2 = rah2::views::take(in, 1000);
        std::vector<int> out2;
        std::copy(rah2::begin(range2), rah2::end(range2), std::back_inserter(out2));
        assert(out2 == std::vector<int>({0, 1, 2, 3, 4, 5, 6, 7, 8, 9}));
        /// [take]
    }
    testSuite.test_case("sample_pipeable");
    {
        /// [take_pipeable]
        std::vector<int> in{0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
        auto range = in | rah2::views::take(5);
        std::vector<int> out;
        std::copy(rah2::begin(range), rah2::end(range), std::back_inserter(out));
        assert(out == std::vector<int>({0, 1, 2, 3, 4}));
        auto range2 = in | rah2::views::take(1000);
        std::vector<int> out2;
        std::copy(rah2::begin(range2), rah2::end(range2), std::back_inserter(out2));
        assert(out2 == std::vector<int>({0, 1, 2, 3, 4, 5, 6, 7, 8, 9}));
        /// [take_pipeable]
    }

    testSuite.test_case("concepts");
    check_all_cat<rah2::contiguous_iterator_tag, std::input_iterator_tag, make_take_view>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct make_drop_view
{
    auto make()
    {
        return rah2::views::drop(make_test_view<CS, Tag, Sized>(), 2);
    }
    using base_type = test_view<CS, Tag, Sized>;
    static constexpr bool is_sized = Sized;
    static constexpr bool is_common = rah2::common_range<base_type>;
    static constexpr bool do_test = true;
    static constexpr bool is_borrowed = rah2::enable_borrowed_range<base_type>;
};
void test_drop_view()
{
    {
        testSuite.test_case("sample");

        /// [drop]
        std::vector<int> in{0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
        auto range = rah2::views::drop(in, 6);
        std::vector<int> out;
        std::copy(rah2::begin(range), rah2::end(range), std::back_inserter(out));
        assert(out == std::vector<int>({6, 7, 8, 9}));
        /// [drop]
    }

    {
        testSuite.test_case("sample_pipeable");
        /// [drop_pipeable]
        std::vector<int> in{0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
        auto range = in | rah2::views::drop(6);
        std::vector<int> out;
        std::copy(rah2::begin(range), rah2::end(range), std::back_inserter(out));
        assert(out == std::vector<int>({6, 7, 8, 9}));
        /// [drop_pipeable]
    }
    testSuite.test_case("concepts");
    check_all_cat<rah2::contiguous_iterator_tag, std::input_iterator_tag, make_drop_view>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct make_drop_while_view
{
    auto make()
    {
        return rah2::views::drop_while(
            make_test_view<CS, Tag, Sized>(), [](auto i) { return i < 4; });
    }
    using V = test_view<CS, Tag, Sized>;
    static constexpr bool is_sized =
        rah2::forward_range<V> && rah2::sized_sentinel_for<rah2::sentinel_t<V>, rah2::iterator_t<V>>;
    static constexpr bool is_common = rah2::common_range<V>;
    static constexpr bool do_test = true;
    static constexpr bool is_borrowed = rah2::enable_borrowed_range<V>;
};
void test_drop_while_view()
{
    {
        testSuite.test_case("sample");
        /// [drop_while]
        std::vector<int> in{0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
        auto range = rah2::views::drop_while(in, [](auto v) { return v < 6; });
        std::vector<int> out;
        std::copy(rah2::begin(range), rah2::end(range), std::back_inserter(out));
        assert(out == std::vector<int>({6, 7, 8, 9}));
        /// [drop_while]
    }

    {
        testSuite.test_case("sample_pipeable");
        /// [drop_while_pipeable]
        std::vector<int> in{0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
        auto range = in | rah2::views::drop_while([](auto v) { return v < 6; });
        std::vector<int> out;
        std::copy(rah2::begin(range), rah2::end(range), std::back_inserter(out));
        assert(out == std::vector<int>({6, 7, 8, 9}));
        /// [drop_while_pipeable]
    }

    testSuite.test_case("concepts");
    check_all_cat<rah2::contiguous_iterator_tag, std::input_iterator_tag, make_drop_while_view>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct make_join_view
{
    auto make()
    {
        return rah2::views::join(rah2::views::transform(
            make_test_view<CS, Tag, Sized>(), [](auto i) { return rah2::views::iota(0, i); }));
    }
    using BaseView = test_view<CS, Tag, Sized>;
    static constexpr bool is_sized = false;
    static constexpr bool is_common = false;
    static constexpr bool do_test = true;
    static constexpr bool is_borrowed = false;
};
void test_join_view()
{
    {
        testSuite.test_case("sample");
        /// [join]
        std::vector<std::vector<int>> in = {
            {},
            {0, 1},
            {},
            {2, 3, 4},
            {5},
            {},
        };
        auto range = rah2::views::join(in);
        std::vector<int> result;
        rah2::copy(range, std::back_inserter(result));
        assert(result == std::vector<int>({0, 1, 2, 3, 4, 5}));
        /// [join]
    }
    {
        testSuite.test_case("sample_pipeable");
        /// [join_pipeable]
        std::vector<std::vector<int>> in = {
            {0, 1},
            {},
            {2, 3, 4},
            {5},
            {},
        };
        auto range = in | rah2::views::join();
        std::vector<int> result;
        rah2::copy(range, std::back_inserter(result));
        assert(result == std::vector<int>({0, 1, 2, 3, 4, 5}));
        /// [join_pipeable]
    }
    {
        testSuite.test_case("rvalue_input");
        // Test join on a range of rvalue
        auto range = rah2::views::iota<size_t>(0, 6)
                     | rah2::views::transform(
                         [](auto i) { return rah2::views::repeat(1) | rah2::views::take(i); })
                     | rah2::views::join();
        std::vector<int> result;
        rah2::copy(range, std::back_inserter(result));
        assert(result == std::vector<int>(15, 1));
    }

    testSuite.test_case("concepts");
    check_all_cat<rah2::input_iterator_tag, std::input_iterator_tag, make_join_view>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct make_split_view
{
    std::array<int, 2> delim = {3, 4};
    auto make()
    {
        return rah2::views::split(make_test_view<CS, Tag, Sized>(), delim);
    }
    static constexpr bool is_sized = false;
    static constexpr bool is_common = false;
    static constexpr bool do_test = rah2::forward_range<test_view<CS, Tag, Sized>>;
    static constexpr bool is_borrowed = false;
};
void test_split_view()
{
    testSuite.test_case("sample");
    /// [views::split]
    std::string sentence{"Keep..moving..forward.."};
    std::string delim{".."};
    auto words =
        rah2::views::split(sentence, delim)
        | rah2::views::transform([](auto word) { return std::string(word.begin(), word.end()); });

    EQUAL_RANGE(words, std::vector<std::string>({"Keep", "moving", "forward"}));
    /// [views::split]

    testSuite.test_case("concepts");
    // TODO : Allow forward_iterator
    // TODO : Allow common_range
    // TODO : Check inner_range (reference_t)
    check_all_cat<rah2::input_iterator_tag, std::input_iterator_tag, make_split_view>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct make_counted_view
{
    std::array<int, 2> delim = {3, 4};
    auto make()
    {
        auto r = make_test_view<CS, Tag, Sized>();
        return rah2::views::counted(r.begin(), 8);
    }
    using BaseRange = test_view<CS, Tag, Sized>;
    static constexpr bool is_sized = rah2::random_access_range<BaseRange>;
    static constexpr bool is_common = rah2::random_access_iterator<rah2::iterator_t<BaseRange>>;
    static constexpr bool do_test = true;
    static constexpr bool is_borrowed = true; // It is actually a subrange
};
void test_counted_view()
{
    testSuite.test_case("sample");
    /// [counted]
    std::vector<int> in{0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
    auto range = rah2::views::counted(in.begin(), 5);
    std::vector<int> out;
    std::copy(rah2::begin(range), rah2::end(range), std::back_inserter(out));
    assert(out == std::vector<int>({0, 1, 2, 3, 4}));
    /// [counted]

    testSuite.test_case("concepts");
    check_all_cat<rah2::contiguous_iterator_tag, std::input_iterator_tag, make_counted_view>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct make_common_view
{
    auto make()
    {
        return rah2::views::common(make_test_view<CS, Tag, Sized>());
    }
    using BaseRange = test_view<CS, Tag, Sized>;
    static constexpr bool is_sized = rah2::sized_range<BaseRange>;
    static constexpr bool is_common = true;
    static constexpr bool do_test = true;
    static constexpr bool is_borrowed = rah2::enable_borrowed_range<BaseRange>;
};
void test_common_view()
{
    testSuite.test_case("sample");
    /// [rah2::views::common]
    auto c = rah2::views::iota(0, 5) | rah2::views::filter([](auto i) { return i % 2 == 0; });
    std::vector<int> result;
    for (auto&& i : c | rah2::views::common())
        result.push_back(i);
    assert(result == std::vector<int>({0, 2, 4}));
    /// [rah2::views::common]

    testSuite.test_case("concepts");
    check_all_cat<rah2::forward_iterator_tag, rah2::forward_iterator_tag, make_common_view>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct make_reverse_view
{
    auto make()
    {
        return rah2::views::reverse(make_test_view<CS, Tag, Sized>());
    }
    using BaseRange = test_view<CS, Tag, Sized>;
    static constexpr bool is_sized =
        rah2::sized_range<BaseRange> || rah2::random_access_iterator<rah2::iterator_t<BaseRange>>;
    static constexpr bool is_common = true;
    static constexpr bool do_test = rah2::bidirectional_range<BaseRange>;
    static constexpr bool is_borrowed = rah2::enable_borrowed_range<BaseRange>;
};
void test_reverse_view()
{
    {
        testSuite.test_case("sample");
        /// [reverse]
        std::vector<int> vec{0, 1, 2, 3};
        std::vector<int> result;
        for (int i : rah2::views::reverse(vec))
            result.push_back(i);
        assert(result == std::vector<int>({3, 2, 1, 0}));
        /// [reverse]
    }
    {
        testSuite.test_case("sample_pipeable");
        /// [reverse_pipeable]
        std::vector<int> vec{0, 1, 2, 3};
        std::vector<int> result;
        for (int i : vec | rah2::views::reverse())
            result.push_back(i);
        assert(result == std::vector<int>({3, 2, 1, 0}));
        /// [reverse_pipeable]
    }
    testSuite.test_case("concepts");
    check_all_cat<rah2::random_access_iterator_tag, rah2::input_iterator_tag, make_reverse_view>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct make_elements_view
{
    std::vector<std::tuple<bool, char, int>> vec{
        {true, 'a', 1000},
        {false, 'b', 1001},
        {true, 'c', 1002},
        {false, 'd', 1003},
    };

    auto make()
    {
        return rah2::views::elements<2>(make_test_view_adapter<CS, Tag, Sized>(vec));
    }
    using BaseRange = test_view_adapter<CS, Tag, Sized, std::vector<std::tuple<bool, char, int>>>;
    static constexpr bool is_sized = rah2::sized_range<BaseRange>;
    static constexpr bool is_common = rah2::common_range<BaseRange>;
    static constexpr bool do_test = true;
    static constexpr bool is_borrowed = rah2::enable_borrowed_range<BaseRange>;
};
void test_elements_view()
{
    {
        testSuite.test_case("sample");
        /// [elements_view]
        std::vector<std::tuple<bool, char, int>> vec{
            {true, 'a', 1000},
            {false, 'b', 1001},
            {true, 'c', 1002},
            {false, 'd', 1003},
        };
        std::vector<int> result;
        for (auto i : vec | rah2::views::elements<2>())
            result.push_back(i);
        assert(result == std::vector<int>({1000, 1001, 1002, 1003}));
        /// [elements_view]
    }

    testSuite.test_case("concepts");
    check_all_cat<rah2::random_access_iterator_tag, std::input_iterator_tag, make_elements_view>();
}

void test_values_view()
{
    {
        testSuite.test_case("sample");
        /// [values_view]
        std::vector<std::tuple<bool, char, int>> vec{
            {true, 'a', 1000},
            {false, 'b', 1001},
            {true, 'c', 1002},
            {false, 'd', 1003},
        };
        std::vector<char> result;
        for (auto i : vec | rah2::views::values())
            result.push_back(i);
        assert(result == std::vector<char>({'a', 'b', 'c', 'd'}));
        /// [values_view]
    }
}

void test_keys_view()
{
    {
        testSuite.test_case("sample");
        /// [keys_view]
        std::vector<std::tuple<bool, char, int>> vec{
            {true, 'a', 1000},
            {false, 'b', 1001},
            {true, 'c', 1002},
            {false, 'd', 1003},
        };
        std::vector<bool> result;
        for (auto i : vec | rah2::views::keys())
            result.push_back(i);
        assert(result == std::vector<bool>({true, false, true, false}));
        /// [keys_view]
    }
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct make_enumerate_view
{
    auto make()
    {
        return rah2::views::enumerate(make_test_view<CS, Tag, Sized>());
    }
    using BaseRange = test_view<CS, Tag, Sized>;
    static constexpr bool is_sized = rah2::sized_range<BaseRange>;
    static constexpr bool is_common = rah2::common_range<BaseRange> && rah2::sized_range<BaseRange>;
    static constexpr bool do_test = true;
    static constexpr bool is_borrowed = rah2::enable_borrowed_range<BaseRange>;
};
void test_enumerate_view()
{
    {
        testSuite.test_case("sample");
        /// [enumerate]
        std::vector<int> input{4, 5, 6, 7};
        std::vector<std::tuple<intptr_t, int>> result;
        auto toto = rah2::views::enumerate(input);
        auto prout = toto.end();
        for (auto i_value : rah2::views::enumerate(input))
            result.emplace_back(i_value);
        assert(result == (std::vector<std::tuple<intptr_t, int>>{{0, 4}, {1, 5}, {2, 6}, {3, 7}}));
        /// [enumerate]
    }
    {
        testSuite.test_case("sample_pipeable");
        /// [enumerate_pipeable]
        std::vector<int> input{4, 5, 6, 7};
        std::vector<std::tuple<intptr_t, int>> result;
        for (auto i_value : input | rah2::views::enumerate() | rah2::views::common())
            result.emplace_back(i_value);
        assert(result == (std::vector<std::tuple<intptr_t, int>>{{0, 4}, {1, 5}, {2, 6}, {3, 7}}));
        /// [enumerate_pipeable]
    }

    {
        testSuite.test_case("various");
        // This can't work since enumerate return an rvalue pairs since map_key want an lvalue
        bool bools[] = {false, true, true, false, false, true};
        auto range = bools | rah2::views::enumerate()
                     | rah2::views::filter([](auto&& index_bool) { return std::get<1>(index_bool); })
                     | rah2::views::keys();

        std::vector<intptr_t> ref;
        rah2::copy(range, std::back_inserter(ref));
        assert(ref == (std::vector<intptr_t>{1, 2, 5}));
    }

    testSuite.test_case("concepts");
    check_all_cat<rah2::random_access_iterator_tag, std::input_iterator_tag, make_enumerate_view>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct make_zip_view1
{
    auto make()
    {
        return rah2::views::zip(make_test_view<CS, Tag, Sized>());
    }
    using BaseRange = test_view<CS, Tag, Sized>;
    static constexpr bool is_sized = rah2::sized_range<BaseRange>;
    static constexpr bool is_common =
        rah2::common_range<BaseRange>
        || (rah2::sized_range<BaseRange> && rah2::random_access_range<BaseRange>);
    static constexpr bool do_test = true;
    static constexpr bool is_borrowed = rah2::enable_borrowed_range<BaseRange>;
};
template <CommonOrSent CS, typename Tag, bool Sized>
struct make_zip_view2
{
    auto make()
    {
        return rah2::views::zip(
            make_test_view<CS, Tag, Sized>(),
            make_test_view<Common, rah2::contiguous_iterator_tag, true>());
    }
    using BaseRange1 = test_view<CS, Tag, Sized>;
    using BaseRange2 = test_view<Common, rah2::contiguous_iterator_tag, true>;
    static constexpr bool is_sized = rah2::sized_range<BaseRange1> && rah2::sized_range<BaseRange2>;
    static constexpr bool is_common =
        (rah2::sized_range<BaseRange1> && rah2::random_access_range<BaseRange1>)&&(
            rah2::sized_range<BaseRange2> && rah2::random_access_range<BaseRange2>);
    static constexpr bool do_test = true;
    static constexpr bool is_borrowed =
        rah2::enable_borrowed_range<BaseRange1> && rah2::enable_borrowed_range<BaseRange2>;
};
void test_zip_view()
{
    {
        testSuite.test_case("sample");
        /// [zip]
        std::vector<int> inputA{1, 2, 3, 4};
        std::vector<double> inputB{2.5, 4.5, 6.5, 8.5};
        std::vector<char> inputC{'a', 'b', 'c', 'd', 'e', 'f', 'g'};
        std::vector<std::tuple<int, double, char>> result;
        for (auto a_b_c : rah2::views::zip(inputA, inputB, inputC) | rah2::views::common())
            result.emplace_back(a_b_c);
        assert(
            result
            == (std::vector<std::tuple<int, double, char>>{
                {1, 2.5, 'a'}, {2, 4.5, 'b'}, {3, 6.5, 'c'}, {4, 8.5, 'd'}}));
        /// [zip]
    }

    {
        testSuite.test_case("various");
        std::vector<int> inputA{1, 2, 3, 4};
        std::vector<bool> inputB{false, true, true, false};
        auto range = rah2::views::zip(inputA, inputB)
                     | rah2::views::filter([](auto a_b) { return std::get<1>(a_b); });
        std::vector<std::tuple<int, bool>> result;

        rah2::copy(range, std::back_inserter(result));
        assert(rah2::equal(result, std::vector<std::tuple<int, bool>>({{2, true}, {3, true}})));
    }

    testSuite.test_case("concepts");
    check_all_cat<rah2::random_access_iterator_tag, std::input_iterator_tag, make_zip_view1>();
    check_all_cat<rah2::random_access_iterator_tag, std::input_iterator_tag, make_zip_view2>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct make_adjacent_view
{
    auto make()
    {
        return rah2::views::adjacent<3>(make_test_view<CS, Tag, Sized>());
    }
    using BaseRange = test_view<CS, Tag, Sized>;
    static constexpr bool is_sized = rah2::sized_range<BaseRange>;
    static constexpr bool is_common = rah2::common_range<BaseRange>;
    static constexpr bool do_test = true;
    static constexpr bool is_borrowed = rah2::enable_borrowed_range<BaseRange>;
};
void test_adjacent_view()
{
    {
        testSuite.test_case("sample");
        /// [adjacent]
        std::vector<int> in{0, 1, 2, 3, 4, 5};
        std::vector<std::vector<int>> out;
        for (auto&& abc : rah2::views::adjacent<3>(in))
        {
            out.push_back({std::get<0>(abc), std::get<1>(abc), std::get<2>(abc)});
        }
        assert(out == (std::vector<std::vector<int>>{{0, 1, 2}, {1, 2, 3}, {2, 3, 4}, {3, 4, 5}}));
        /// [adjacent]
    }
    {
        testSuite.test_case("non_common");
        // adjacent With non common_range
        std::vector<std::vector<int>> out;
        for (auto&& abc : rah2::views::iota(0) | rah2::views::take(6) | rah2::views::adjacent<3>())
        {
            out.push_back({std::get<0>(abc), std::get<1>(abc), std::get<2>(abc)});
        }
        assert(out == (std::vector<std::vector<int>>{{0, 1, 2}, {1, 2, 3}, {2, 3, 4}, {3, 4, 5}}));
    }
    {
        testSuite.test_case("N > size()");
        // adjacent With N > view.size()
        std::vector<std::vector<int>> out;
        for (auto&& abc : rah2::views::iota(0) | rah2::views::take(6) | rah2::views::adjacent<45>())
        {
            out.push_back({std::get<0>(abc), std::get<1>(abc), std::get<2>(abc)});
        }
        assert(out == (std::vector<std::vector<int>>{}));
    }
    {
        testSuite.test_case("N == 0");
        // adjacent With N == 0
        std::vector<std::vector<int>> out;
        for (auto&& abc : rah2::views::iota(0) | rah2::views::take(6) | rah2::views::adjacent<0>())
        {
            static_assert(
                RAH2_NAMESPACE::tuple_size_v<std::remove_reference_t<decltype(abc)>> == 0,
                "tuple should be empty");
            out.emplace_back();
        }
        assert(out.empty());
    }

    testSuite.test_case("concepts");
    check_all_cat<rah2::random_access_iterator_tag, std::forward_iterator_tag, make_adjacent_view>();
}

void test_zip_transform()
{
    testSuite.test_case("sample");
    /// [zip_transform]
    std::vector<int> inputA{1, 2, 3, 4};
    std::vector<double> inputB{2.5, 4.5, 6.5, 8.5};
    std::vector<char> inputC{'a', 'b', 'c', 'd', 'e', 'f', 'g'};
    std::vector<std::string> result;
    auto func = [](int a, double d, char c)
    {
        std::stringstream ss;
        ss << a << d << c;
        return ss.str();
    };
    for (auto a_b_c :
         rah2::views::zip_transform(func, inputA, inputB, inputC) | rah2::views::common())
        result.emplace_back(a_b_c);
    assert(result == (std::vector<std::string>{{"12.5a"}, {"24.5b"}, {"36.5c"}, {"48.5d"}}));
    /// [zip_transform]
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
void test_adjacent_transform()
{
    {
        testSuite.test_case("sample");
        /// [adjacent_transform]
        std::vector<int> in{0, 1, 2, 3, 4, 5};
        std::vector<int> out;
        for (auto abc : rah2::views::adjacent_transform<3>(
                 in, [](auto a, auto b, auto c) { return a + b + c; }))
        {
            out.push_back(abc);
        }
        assert(out == (std::vector<int>{3, 6, 9, 12}));
        /// [adjacent_transform]
    }
    {
        testSuite.test_case("non common_range");
        // adjacent_transform With non common_range
        std::vector<int> out;
        for (auto abc : rah2::views::iota(0) | rah2::views::take(6)
                            | rah2::views::adjacent_transform<3>([](auto a, auto b, auto c)
                                                                 { return a + b + c; }))
        {
            out.push_back(abc);
        }
        assert(out == (std::vector<int>{3, 6, 9, 12}));
    }
    {
        testSuite.test_case("N > size()");
        // adjacent_transform With N > view.size()
        std::vector<int> out;
        for (auto abc : rah2::views::iota(0) | rah2::views::take(6)
                            | rah2::views::adjacent_transform<45>(Add{}))
        {
            out.push_back(abc);
        }
        assert(out == (std::vector<int>{}));
    }
    {
        testSuite.test_case("N == 0");
        // adjacent_transform With N == 0
        std::vector<std::vector<int>> out;
        for (auto&& abc : rah2::views::iota(0) | rah2::views::take(6)
                              | rah2::views::adjacent_transform<0>([](auto i) { return i + 1; }))
        {
            static_assert(
                RAH2_NAMESPACE::tuple_size_v<std::remove_reference_t<decltype(abc)>> == 0,
                "tuple should be empty");
            out.push_back({});
        }
        assert(out == (std::vector<std::vector<int>>{}));
    }
}

void test_slide_view()
{
    {
        testSuite.test_case("sample");
        /// [sliding]
        std::vector<int> in{0, 1, 2, 3, 4, 5};
        std::vector<std::vector<int>> out;
        for (auto subRange : rah2::views::slide(in, 3))
        {
            out.emplace_back();
            std::copy(rah2::begin(subRange), rah2::end(subRange), std::back_inserter(out.back()));
        }
        assert(out == (std::vector<std::vector<int>>{{0, 1, 2}, {1, 2, 3}, {2, 3, 4}, {3, 4, 5}}));
        /// [sliding]
    }
    {
        testSuite.test_case("non common_range");
        // slide with non common_range
        std::vector<std::vector<int>> out;
        auto r = rah2::views::iota(0) | rah2::views::take(6) | rah2::views::slide(3);
        auto it = rah2::begin(r);
        auto e = rah2::end(r);
        for (; it != e; ++it)
        {
            out.emplace_back();
            auto&& subRange = *it;
            rah2::copy(subRange, std::back_inserter(out.back()));
        }
        assert(out == (std::vector<std::vector<int>>{{0, 1, 2}, {1, 2, 3}, {2, 3, 4}, {3, 4, 5}}));
    }

    {
        testSuite.test_case("sample_pipeable");
        /// [sliding_pipeable]
        std::vector<int> in{0, 1, 2, 3, 4, 5};
        std::vector<std::vector<int>> out;
        for (auto subRange : in | rah2::views::slide(3))
        {
            out.emplace_back();
            std::copy(rah2::begin(subRange), rah2::end(subRange), std::back_inserter(out.back()));
        }
        assert(out == (std::vector<std::vector<int>>{{0, 1, 2}, {1, 2, 3}, {2, 3, 4}, {3, 4, 5}}));
        /// [sliding_pipeable]
    }

    {
        testSuite.test_case("various");
        std::vector<int> in{0, 1, 2, 3, 4, 5};
        std::vector<std::vector<int>> out;
        auto range = in | rah2::views::cycle() | rah2::views::slide(3) | rah2::views::take(in.size());
        for (auto subRange : range | rah2::views::common())
        {
            out.emplace_back();
            std::copy(rah2::begin(subRange), rah2::end(subRange), std::back_inserter(out.back()));
        }
        assert(
            out
            == (std::vector<std::vector<int>>{
                {0, 1, 2},
                {1, 2, 3},
                {2, 3, 4},
                {3, 4, 5},
                {4, 5, 0},
                {5, 0, 1},
            }));
    }

    {
        std::vector<int> in{0, 1, 2, 3};
        std::vector<std::vector<int>> out;
        for (auto subRange : rah2::views::slide(in, 4))
        {
            out.emplace_back();
            std::copy(rah2::begin(subRange), rah2::end(subRange), std::back_inserter(out.back()));
        }
        assert(out == (std::vector<std::vector<int>>{{0, 1, 2, 3}}));
    }

    {
        std::vector<int> in{0, 1};
        std::vector<std::vector<int>> out;
        for (auto subRange : rah2::views::slide(in, 4))
        {
            out.emplace_back();
            std::copy(rah2::begin(subRange), rah2::end(subRange), std::back_inserter(out.back()));
        }
        assert(out == (std::vector<std::vector<int>>{}));
    }

    {
        std::vector<int> in{0, 1, 2, 3};
        std::vector<std::vector<int>> out;
        for (auto subRange : rah2::views::slide(in, 0))
        {
            out.emplace_back();
            std::copy(rah2::begin(subRange), rah2::end(subRange), std::back_inserter(out.back()));
        }
        assert(out == (std::vector<std::vector<int>>{{0}, {1}, {2}, {3}}));
    }

    {
        std::vector<int> in{0, 1, 2, 3};
        std::vector<std::vector<int>> out;
        for (auto subRange : in | rah2::views::slide(1))
        {
            out.emplace_back();
            std::copy(rah2::begin(subRange), rah2::end(subRange), std::back_inserter(out.back()));
        }
        assert(
            out
            == (std::vector<std::vector<int>>{
                {0},
                {1},
                {2},
                {3},
            }));
    }
}

void test_chunk_view()
{
    {
        testSuite.test_case("sample");
        /// [chunk]
        std::vector<int> vec_01234{0, 1, 2, 3, 4};
        std::vector<std::vector<int>> result;
        for (auto elts : rah2::views::chunk(vec_01234, 2))
            result.emplace_back(rah2::begin(elts), rah2::end(elts));
        assert(result == std::vector<std::vector<int>>({{0, 1}, {2, 3}, {4}}));
        /// [chunk]
    }
    {
        testSuite.test_case("sample_pipeable");
        /// [chunk_pipeable]
        std::vector<int> vec_01234{0, 1, 2, 3, 4};
        std::vector<std::vector<int>> result;
        for (auto elts : vec_01234 | rah2::views::chunk(2))
            result.emplace_back(rah2::begin(elts), rah2::end(elts));
        assert(result == std::vector<std::vector<int>>({{0, 1}, {2, 3}, {4}}));
        /// [chunk_pipeable]
    }
    {
        testSuite.test_case("non-common_view");
        /// Chunk with non-common_view
        auto vec_01234 = rah2::views::iota(0) | rah2::views::take(5);
        std::vector<std::vector<int>> result;
        for (auto elts : rah2::views::chunk(vec_01234, 2))
            result.emplace_back(rah2::begin(elts), rah2::end(elts));
        assert(result == std::vector<std::vector<int>>({{0, 1}, {2, 3}, {4}}));
    }
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct make_stride_view
{
    auto make()
    {
        return rah2::views::stride(make_test_view<CS, Tag, Sized>(), 3);
    }
    using BaseRange = test_view<CS, Tag, Sized>;
    static constexpr bool is_sized = rah2::sized_range<BaseRange>;
    static constexpr bool is_common =
        rah2::common_range<BaseRange>
        && (rah2::sized_range<BaseRange> || !rah2::bidirectional_range<BaseRange>);
    static constexpr bool do_test = true;
    static constexpr bool is_borrowed = rah2::enable_borrowed_range<BaseRange>;
};
void test_stride_view()
{
    {
        testSuite.test_case("sample");
        /// [stride]
        std::vector<int> vec{0, 1, 2, 3, 4, 5, 6, 7};
        std::vector<int> result;
        for (int i : rah2::views::stride(vec, 2))
            result.push_back(i);
        assert(result == std::vector<int>({0, 2, 4, 6}));
        /// [stride]
    }
    {
        testSuite.test_case("sample_pipeable");
        /// [stride_pipeable]
        std::vector<int> vec{0, 1, 2, 3, 4, 5, 6, 7};
        std::vector<int> result;
        for (int i : vec | rah2::views::stride(2))
            result.push_back(i);
        assert(result == std::vector<int>({0, 2, 4, 6}));
        /// [stride_pipeable]
    }

    testSuite.test_case("concepts");
    check_all_cat<rah2::random_access_iterator_tag, std::input_iterator_tag, make_stride_view>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct make_ref_view
{
    using BaseRange = test_view<CS, Tag, Sized>;
    BaseRange base;
    auto make()
    {
        return rah2::views::ref(base);
    }
    static constexpr bool is_sized = rah2::sized_range<BaseRange>;
    static constexpr bool is_common = rah2::common_range<BaseRange>;
    static constexpr bool do_test = true;
    static constexpr bool is_borrowed = true;
};
void test_ref_view()
{
    {
        testSuite.test_case("sample");
        /// [ref]
        std::vector<int> vec{0, 1, 2, 2, 3};
        std::vector<int> out;
        auto ref = rah2::views::ref(vec);
        for (auto&& val : ref)
        {
            out.push_back(val);
        }
        assert(out == (std::vector<int>{0, 1, 2, 2, 3}));
        /// [ref]
    }

    testSuite.test_case("concepts");
    check_all_cat<rah2::contiguous_iterator_tag, std::input_iterator_tag, make_ref_view>();
}