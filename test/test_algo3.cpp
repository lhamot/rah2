#include <rah2/ranges.hpp>
#include <rah2/algorithm.hpp>

#include <cctype>
#include <complex>
#include <random>
#include <forward_list>
#include <cstring>

#ifdef RAH2_USE_EASTL
#include <EASTL/vector.h>
#include <EASTL/array.h>
#include <EASTL/list.h>
#include <EASTL/algorithm.h>
#include <EASTL/sort.h>
#include <EASTL/numeric.h>
#include <EASTL/set.h>
#include <EASTL/initializer_list.h>

#else
#include <array>
#include <list>
#include <algorithm>
#include <numeric>
#include <forward_list>
#include <set>
#include <cstring>
#endif

#if RAH2_CPP20
#include <algorithm>
#endif

#include "test_helpers.hpp"

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_adjacent_find_
{
    template <bool = true>
    void test()
    {
        std::vector<Coord> const vec = {
            {0, 0}, {1, 0}, {2, 0}, {3, 0}, {40, 1}, {40, 1}, {41, 0}, {41, 0}, {5, 1}};
        auto v = make_test_view_adapter<CS, Tag, Sized>(vec);
        {
            testSuite.test_case("iter");
            testSuite.test_case("noproj");
            auto const it = RAH2_NS::ranges::adjacent_find(v.begin(), v.end());
            assert(RAH2_NS::ranges::distance(v.begin(), it) == 4);
        }

        {
            testSuite.test_case("range");
            testSuite.test_case("pred");
            testSuite.test_case("proj");
            auto const it = RAH2_NS::ranges::adjacent_find(v, RAH2_NS::ranges::greater(), &Coord::x);
            assert(RAH2_NS::ranges::distance(v.begin(), it) == 7);
        }

        {
            std::vector<Coord> const vec2;
            auto v2 = make_test_view_adapter<CS, Tag, Sized>(vec2);
            testSuite.test_case("empty");
            auto const it = RAH2_NS::ranges::adjacent_find(v2, RAH2_NS::ranges::greater(), &Coord::x);
            CHECK(it == v2.end());
        }
    }

    template <bool = true>
    void test_perf(char const* range_type)
    {
        testSuite.test_case("perf");
        std::vector<Coord> vec = {
            {0, 0}, {1, 0}, {2, 0}, {3, 0}, {40, 1}, {40, 1}, {41, 0}, {41, 0}, {5, 1}};
        vec.insert(vec.begin(), 1000000LLU * RELEASE_MULTIPLIER, Coord{0, 0});
        for (intptr_t i = 0; i < 1000000LL * RELEASE_MULTIPLIER; ++i)
        {
            vec[i] = {0, i};
        }
        auto v = make_test_view_adapter<CS, Tag, Sized>(vec);

        {
            COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(
                CS == Common,
                "adjacent_find",
                range_type,
                [&]
                {
                    auto const it = STD::adjacent_find(fwd(v.begin()), v.end());
                    assert((*it).x == 40);
                });
        }
        {
            COMPARE_DURATION_TO_STD_RANGES(
                "adjacent_find_proj",
                range_type,
                [&]
                {
                    auto const it = STD::adjacent_find(v, RAH2_NS::ranges::greater(), &Coord::x);
                    assert((*it).x == 41);
                });
        }
    }
    static constexpr bool do_test = RAH2_NS::derived_from<Tag, RAH2_NS::forward_iterator_tag>;
};
void test_adjacent_find()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::adjacent_find]
    auto const v = {0, 1, 2, 3, 40, 40, 41, 41, 5}; /*
                                                ^^          ^^       */
    {
        auto const it = RAH2_NS::ranges::adjacent_find(v.begin(), v.end());
        assert(RAH2_NS::ranges::distance(v.begin(), it) == 4);
    }

    {
        auto const it = RAH2_NS::ranges::adjacent_find(v, RAH2_NS::ranges::greater());
        assert(RAH2_NS::ranges::distance(v.begin(), it) == 7);
    }
    /// [rah2::ranges::adjacent_find]

    foreach_range_combination<test_algo<test_adjacent_find_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_search_
{
    template <bool = true>
    void test()
    {
        RAH2_STD::string haystack_{"abcd abcd"};
        RAH2_STD::string needle_{"bcd"};
        auto haystack = make_test_view_adapter<CS, Tag, Sized>(haystack_);
        auto needle = make_test_view_adapter<CS, Tag, Sized>(needle_);

        // the search uses iterator pairs begin()/end():
        auto const found1 =
            RAH2_NS::ranges::search(haystack.begin(), haystack.end(), needle.begin(), needle.end());
        assert(RAH2_STD::distance(haystack.begin(), found1.begin()) == 1);
        assert(RAH2_STD::distance(haystack.begin(), found1.end()) == 4);

        // the search uses ranges r1, r2:
        auto const found2 = RAH2_NS::ranges::search(haystack, needle);
        assert(RAH2_STD::distance(haystack.begin(), found2.begin()) == 1);
        assert(RAH2_STD::distance(haystack.begin(), found2.end()) == 4);

        // 'needle' range is empty:
        RAH2_STD::string none_;
        auto none = make_test_view_adapter<CS, Tag, Sized>(none_);
        auto const found3 = RAH2_NS::ranges::search(haystack, none);
        assert(RAH2_STD::distance(haystack.begin(), found3.begin()) == 0);
        assert(RAH2_STD::distance(haystack.begin(), found3.end()) == 0);
        auto const found6 = RAH2_NS::ranges::search(none, haystack);
        assert(none.end() == found6.begin());
        assert(none.end() == found6.end());

        // 'needle' will not be found:
        RAH2_STD::string awl_{"efg"};
        auto awl = make_test_view_adapter<CS, Tag, Sized>(awl_);
        auto const found4 = RAH2_NS::ranges::search(haystack, awl);
        assert(RAH2_STD::distance(haystack.begin(), found4.begin()) == 9);
        assert(RAH2_STD::distance(haystack.begin(), found4.end()) == 9);

        // the search uses custom comparator and projections:
        RAH2_STD::string bodkin_{"234"};
        auto bodkin = make_test_view_adapter<CS, Tag, Sized>(bodkin_);
        auto const found5 = RAH2_NS::ranges::search(
            haystack,
            bodkin,
            [](int const x, int const y) { return x == y; }, // pred
            [](int const x) { return std::toupper(x); }, // proj1
            [](int const y) { return y + 'A' - '1'; }); // proj2
        assert(RAH2_STD::distance(haystack.begin(), found5.begin()) == 1);
        assert(RAH2_STD::distance(haystack.begin(), found5.end()) == 4);
    }

    template <typename I, RAH2_STD::enable_if_t<RAH2_NS::input_iterator<RAH2_NS::remove_cvref_t<I>>>* = nullptr>
    static auto get_begin(I&& i) -> decltype(std::forward<I>(i))
    {
        return std::forward<I>(i);
    }

    template <typename I, RAH2_STD::enable_if_t<!RAH2_NS::input_iterator<RAH2_NS::remove_cvref_t<I>>>* = nullptr>
    static auto get_begin(I&& i) -> decltype(i.begin())
    {
        return i.begin();
    }

    template <bool = true>
    void test_perf(char const* range_type)
    {
        RAH2_STD::string haystack_;
        for (size_t i = 0; i < 1000000 * RELEASE_MULTIPLIER; ++i)
        {
            haystack_.push_back('a' + (i % 7));
        }
        haystack_ += "abcdefgh cd";
        auto haystack = make_test_view_adapter<CS, Tag, Sized>(haystack_);

        RAH2_STD::string needle_{"bcdefgh"};
        auto needle = make_test_view_adapter<CS, Tag, Sized>(needle_);

        RAH2_STD::string haystack_proj_;
        for (size_t i = 0; i < 1000000 * RELEASE_MULTIPLIER; ++i)
        {
            haystack_proj_.push_back('a' + (i % 3));
        }
        haystack_proj_ += "abcdefgh cd";

        {
            COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(
                CS == Common,
                "search",
                range_type,
                [&]
                {
                    // the search uses iterator pairs begin()/end():
                    auto const found1 = STD::search(
                        fwd(haystack.begin()), haystack.end(), needle.begin(), needle.end());
                    assert(get_begin(found1) != haystack.end());
                    assert(*get_begin(found1) == *needle.begin());
                });
        }
        {
            COMPARE_DURATION_TO_STD_RANGES(
                "search_proj",
                range_type,
                (
                    [&]
                    {
                        auto haystack_proj = make_test_view_adapter<CS, Tag, Sized>(haystack_proj_);
                        RAH2_STD::string bodkin_{"2345678"};
                        auto bodkin = make_test_view_adapter<CS, Tag, Sized>(bodkin_);
                        auto const found5 = STD::search(
                            haystack_proj,
                            bodkin,
                            [](int const x, int const y) { return x == y; },
                            [](int const x) { return std::toupper(x); },
                            [](int const y) { return (y - '1') + 'A'; });
                        assert(found5.begin() != haystack_proj.end());
                        assert(*found5.begin() == 'b');
                    }));
        }
    }
    static constexpr bool do_test = RAH2_NS::derived_from<Tag, RAH2_NS::forward_iterator_tag>;
};
void test_search()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::search]
    RAH2_STD::string haystack{"abcd abcd"};
    RAH2_STD::string needle{"bcd"};

    // the search uses iterator pairs begin()/end():
    auto const found1 =
        RAH2_NS::ranges::search(haystack.begin(), haystack.end(), needle.begin(), needle.end());
    assert(found1.begin() - haystack.begin() == 1);
    assert(found1.end() - haystack.begin() == 4);

    // the search uses ranges r1, r2:
    auto const found2 = RAH2_NS::ranges::search(haystack, needle);
    assert(found2.begin() - haystack.begin() == 1);
    assert(found2.end() - haystack.begin() == 4);

    // 'needle' range is empty:
    RAH2_STD::string none;
    auto const found3 = RAH2_NS::ranges::search(haystack, none);
    assert(found3.begin() - haystack.begin() == 0);
    assert(found3.end() - haystack.begin() == 0);

    // 'needle' will not be found:
    RAH2_STD::string awl{"efg"};
    auto const found4 = RAH2_NS::ranges::search(haystack, awl);
    assert(found4.begin() - haystack.begin() == 9);
    assert(found4.end() - haystack.begin() == 9);

    // the search uses custom comparator and projections:
    RAH2_STD::string bodkin{"234"};
    auto const found5 = RAH2_NS::ranges::search(
        haystack,
        bodkin,
        [](int const x, int const y) { return x == y; }, // pred
        [](int const x) { return std::toupper(x); }, // proj1
        [](int const y) { return y + 'A' - '1'; }); // proj2
    assert(found5.begin() - haystack.begin() == 1);
    assert(found5.end() - haystack.begin() == 4);
    /// [rah2::ranges::search]

    foreach_range_combination<test_algo<test_search_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_search_n_
{
    template <bool = true>
    void test()
    {
        int count{3};
        Coord value{2, 0};
        RAH2_STD::vector<Coord> nums_{
            {1, 0}, {2, 0}, {2, 0}, {3, 0}, {4, 0}, {1, 0}, {2, 0}, {2, 0}, {2, 0}, {1, 0}};
        auto nums = make_test_view_adapter<CS, Tag, Sized>(nums_);

        testSuite.test_case("iter");
        testSuite.test_case("noproj");
        auto result1 = RAH2_NS::ranges::search_n(nums.begin(), nums.end(), count, value);
        CHECK((RAH2_NS::ranges::distance(result1.begin(), result1.end())) == (count));
        CHECK_EQUAL(RAH2_STD::distance(nums.begin(), result1.begin()), 6);
        CHECK_EQUAL(RAH2_STD::distance(nums.begin(), result1.end()), 9);

        testSuite.test_case("ranges");
        auto result2 = RAH2_NS::ranges::search_n(nums, count, value);
        CHECK((RAH2_NS::ranges::distance(result2.begin(), result2.end())) == (count));
        CHECK_EQUAL(RAH2_STD::distance(nums.begin(), result2.begin()), 6);
        CHECK_EQUAL(RAH2_STD::distance(nums.begin(), result2.end()), 9);

        testSuite.test_case("proj");
        auto result3 = RAH2_NS::ranges::search_n(
            nums, count, 2, [](auto a, auto b) { return a == b; }, &Coord::x);
        CHECK((RAH2_NS::ranges::distance(result3.begin(), result3.end())) == (count));
        CHECK_EQUAL(RAH2_STD::distance(nums.begin(), result3.begin()), 6);
        CHECK_EQUAL(RAH2_STD::distance(nums.begin(), result3.end()), 9);

        RAH2_STD::vector<Coord> empty_{};
        auto empty = make_test_view_adapter<CS, Tag, Sized>(empty_);
        testSuite.test_case("empty");
        testSuite.test_case("notfound");
        auto result4 = RAH2_NS::ranges::search_n(empty, count, value);
        CHECK_EQUAL(RAH2_NS::ranges::distance(result4.begin(), result4.end()), 0);
        CHECK_EQUAL(RAH2_STD::distance(empty.begin(), result4.begin()), 0);
        CHECK_EQUAL(RAH2_STD::distance(empty.begin(), result4.end()), 0);
    }

    template <bool = true>
    void test_perf(char const* range_type)
    {
        int count{3};
        Coord value{2, 0};
        (void)count;
        (void)value;
        RAH2_STD::vector<Coord> nums_{
            {1, 0}, {2, 0}, {2, 0}, {3, 0}, {4, 0}, {1, 0}, {2, 0}, {2, 0}, {2, 0}, {1, 0}};
        nums_.insert(nums_.begin(), 1000000 * RELEASE_MULTIPLIER, {0, 0});
        auto nums = make_test_view_adapter<CS, Tag, Sized>(nums_);
        {
            COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(
                CS == Common,
                "search_n",
                range_type,
                [&]
                {
                    auto result1 = STD::search_n(fwd(nums.begin()), nums.end(), count, value);
                    DONT_OPTIM(result1);
                });
        }
        {
            COMPARE_DURATION_TO_STD_RANGES(
                "search_n_proj",
                range_type,
                (
                    [&]
                    {
                        auto result2 = STD::search_n(nums, count, value);
                        CHECK((RAH2_NS::ranges::distance(result2.begin(), result2.end())) == (count));
                        CHECK_EQUAL(
                            RAH2_STD::distance(nums.begin(), result2.begin()),
                            1000000 * RELEASE_MULTIPLIER + 6);
                        CHECK_EQUAL(
                            RAH2_STD::distance(nums.begin(), result2.end()),
                            1000000 * RELEASE_MULTIPLIER + 9);
                    }));
        }
    }
    static constexpr bool do_test = RAH2_NS::derived_from<Tag, RAH2_NS::forward_iterator_tag>;
};
void test_search_n()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::search_n]
    auto nums = {1, 2, 2, 3, 4, 1, 2, 2, 2, 1};
    constexpr int count{3};
    constexpr int value{2};
    using count_t = int;
    using value_t = int;

    auto result1 = RAH2_NS::ranges::search_n(nums.begin(), nums.end(), count, value);
    assert( // found
        result1.size() == count && RAH2_STD::distance(nums.begin(), result1.begin()) == 6
        && RAH2_STD::distance(nums.begin(), result1.end()) == 9);

    auto result2 = RAH2_NS::ranges::search_n(nums, count, value);
    assert( // found
        result2.size() == count && RAH2_STD::distance(nums.begin(), result2.begin()) == 6
        && RAH2_STD::distance(nums.begin(), result2.end()) == 9);

    auto result3 = RAH2_NS::ranges::search_n(nums, count, value_t{5});
    assert( // not found
        result3.size() == 0 && result3.begin() == result3.end() && result3.end() == nums.end());

    auto result4 = RAH2_NS::ranges::search_n(nums, count_t{0}, value_t{1});
    assert( // not found
        result4.size() == 0 && result4.begin() == result4.end() && result4.end() == nums.begin());

    char const symbol{'B'};
    auto to_ascii = [](int const z) -> char
    {
        return static_cast<char>('A' + z - 1);
    };
    auto is_equ = [](char const x, char const y)
    {
        return x == y;
    };

    auto const result5 = RAH2_NS::ranges::search_n(nums, count, symbol, is_equ, to_ascii);
    assert(result5.begin() - nums.begin() == 6);
    /// [rah2::ranges::search_n]

    foreach_range_combination<test_algo<test_search_n_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_contains_
{
    template <bool = true>
    void test()
    {
        Coord value{5, 0};
        RAH2_STD::vector<Coord> nums_{
            {1, 0}, {2, 0}, {2, 0}, {3, 0}, {4, 0}, {1, 0}, {5, 0}, {2, 0}, {2, 0}, {1, 0}};
        auto nums = make_test_view_adapter<CS, Tag, Sized>(nums_);

        testSuite.test_case("iter");
        testSuite.test_case("noproj");
        auto result1 = RAH2_NS::ranges::contains(nums.begin(), nums.end(), value);
        CHECK(result1);

        testSuite.test_case("ranges");
        auto result2 = RAH2_NS::ranges::contains(nums, value);
        CHECK(result2);

        testSuite.test_case("proj");
        auto result3 = RAH2_NS::ranges::contains(nums, 5, &Coord::x);
        CHECK(result3);

        testSuite.test_case("notfound");
        auto result4 = RAH2_NS::ranges::contains(nums, 18, &Coord::x);
        CHECK(not result4);

        testSuite.test_case("empty");
        RAH2_STD::vector<Coord> empty_{};
        auto empty = make_test_view_adapter<CS, Tag, Sized>(empty_);
        auto result5 = RAH2_NS::ranges::contains(empty, value);
        CHECK(not result5);
    }

    template <bool = true>
    void test_perf(char const* range_type)
    {
        int count{3};
        Coord value{5, 0};
        (void)count;
        (void)value;
        RAH2_STD::vector<Coord> nums_{
            {1, 0}, {2, 0}, {2, 0}, {3, 0}, {4, 0}, {1, 0}, {5, 0}, {2, 0}, {2, 0}, {1, 0}};
        nums_.insert(nums_.begin(), 1000000 * RELEASE_MULTIPLIER, {0, 0});
        auto nums = make_test_view_adapter<CS, Tag, Sized>(nums_);
        {
            COMPARE_DURATION_TO_STD_RANGES_23(
                "contains",
                range_type,
                [&]
                {
                    auto result1 = STD::contains(nums.begin(), nums.end(), value);
                    CHECK(result1);
                });
        }
        {
            COMPARE_DURATION_TO_STD_RANGES_23(
                "contains_proj",
                range_type,
                (
                    [&]
                    {
                        auto result2 = STD::contains(nums, value);
                        CHECK(result2);
                    }));
        }
    }
    static constexpr bool do_test = true;
};
void test_contains()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::contains]
    constexpr auto haystack = RAH2_STD::array<int, 5>{3, 1, 4, 1, 5};
    auto increment = [](int x)
    {
        return ++x;
    };

    assert(RAH2_NS::ranges::contains(haystack, 4));
    assert(not RAH2_NS::ranges::contains(haystack, 6));
    assert(RAH2_NS::ranges::contains(haystack, 6, increment));
    assert(not RAH2_NS::ranges::contains(haystack, 1, increment));
    /// [rah2::ranges::contains]

    foreach_range_combination<test_algo<test_contains_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_contains_subrange_
{
    template <bool = true>
    void test()
    {
        RAH2_STD::string haystack_{"abcd abcd"};
        RAH2_STD::string needle_{"bcd"};
        auto haystack = make_test_view_adapter<CS, Tag, Sized>(haystack_);
        auto needle = make_test_view_adapter<CS, Tag, Sized>(needle_);

        testSuite.test_case("iter");
        auto const found1 = RAH2_NS::ranges::contains_subrange(
            haystack.begin(), haystack.end(), needle.begin(), needle.end());
        CHECK(found1);

        // the search uses ranges r1, r2:
        testSuite.test_case("range");
        auto const found2 = RAH2_NS::ranges::contains_subrange(haystack, needle);
        CHECK(found2);

        // 'needle' range is empty:
        testSuite.test_case("empty");
        RAH2_STD::string none_;
        auto none = make_test_view_adapter<CS, Tag, Sized>(none_);
        auto const found3 = RAH2_NS::ranges::contains_subrange(haystack, none);
        CHECK(found3);

        // 'needle' will not be found:
        testSuite.test_case("notfound");
        RAH2_STD::string awl_{"efg"};
        auto awl = make_test_view_adapter<CS, Tag, Sized>(awl_);
        auto const found4 = RAH2_NS::ranges::contains_subrange(haystack, awl);
        CHECK(not found4);

        // the search uses custom comparator and projections:
        testSuite.test_case("proj");
        RAH2_STD::string bodkin_{"234"};
        auto bodkin = make_test_view_adapter<CS, Tag, Sized>(bodkin_);
        auto const found5 = RAH2_NS::ranges::contains_subrange(
            haystack,
            bodkin,
            [](int const x, int const y) { return x == y; }, // pred
            [](int const x) { return std::toupper(x); }, // proj1
            [](int const y) { return y + 'A' - '1'; }); // proj2
        CHECK(found5);
    }

    template <bool = true>
    void test_perf(char const* range_type)
    {
        testSuite.test_case("perf");
        RAH2_STD::string haystack_;
        for (size_t i = 0; i < 1000000 * RELEASE_MULTIPLIER; ++i)
        {
            haystack_.push_back('a' + (i % 7));
        }
        haystack_ += "abcdefgh cd";
        auto haystack = make_test_view_adapter<CS, Tag, Sized>(haystack_);
        (void)haystack;

        RAH2_STD::string needle_{"bcdefgh"};
        auto needle = make_test_view_adapter<CS, Tag, Sized>(needle_);
        (void)needle;

        RAH2_STD::string haystack_proj_;
        for (size_t i = 0; i < 1000000 * RELEASE_MULTIPLIER; ++i)
        {
            haystack_proj_.push_back('a' + (i % 3));
        }
        haystack_proj_ += "abcdefgh cd";
        (void)haystack_proj_;

        {
            COMPARE_DURATION_TO_STD_RANGES_23(
                "contains_subrange",
                range_type,
                [&]
                {
                    // the search uses iterator pairs begin()/end():
                    auto const found1 = STD::contains_subrange(
                        fwd(haystack.begin()), haystack.end(), needle.begin(), needle.end());
                    CHECK(found1);
                });
        }
        {
            COMPARE_DURATION_TO_STD_RANGES_23(
                "contains_subrange_proj",
                range_type,
                (
                    [&]
                    {
                        auto haystack_proj = make_test_view_adapter<CS, Tag, Sized>(haystack_proj_);
                        RAH2_STD::string bodkin_{"2345678"};
                        auto bodkin = make_test_view_adapter<CS, Tag, Sized>(bodkin_);
                        auto const found5 = STD::contains_subrange(
                            haystack_proj,
                            bodkin,
                            [](int const x, int const y) { return x == y; },
                            [](int const x) { return std::toupper(x); },
                            [](int const y) { return (y - '1') + 'A'; });
                        CHECK(found5);
                    }));
        }
    }
    static constexpr bool do_test = RAH2_NS::derived_from<Tag, RAH2_NS::forward_iterator_tag>;
};
void test_contains_subrange()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::contains_subrange]
    constexpr auto haystack = RAH2_STD::array<int, 5>{3, 1, 4, 1, 5};
    constexpr auto needle = RAH2_STD::array<int, 3>{1, 4, 1};
    constexpr auto bodkin = RAH2_STD::array<int, 3>{2, 5, 2};
    auto increment = [](int x)
    {
        return ++x;
    };
    auto decrement = [](int x)
    {
        return --x;
    };

    assert(RAH2_NS::ranges::contains_subrange(haystack, needle));
    assert(not RAH2_NS::ranges::contains_subrange(haystack, bodkin));
    assert(RAH2_NS::ranges::contains_subrange(haystack, bodkin, {}, increment));
    assert(not RAH2_NS::ranges::contains_subrange(haystack, bodkin, {}, decrement));
    assert(RAH2_NS::ranges::contains_subrange(haystack, bodkin, {}, {}, decrement));
    /// [rah2::ranges::contains_subrange]

    foreach_range_combination<test_algo<test_contains_subrange_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_starts_with_
{
    template <bool = true>
    void test()
    {
        RAH2_STD::string haystack_{"abcd abcd"};
        RAH2_STD::string needle_{"abc"};
        auto haystack = make_test_view_adapter<CS, Tag, Sized>(haystack_);
        auto needle = make_test_view_adapter<CS, Tag, Sized>(needle_);

        testSuite.test_case("iter");
        auto const found1 = RAH2_NS::ranges::starts_with(
            haystack.begin(), haystack.end(), needle.begin(), needle.end());
        CHECK(found1);

        // the search uses ranges r1, r2:
        testSuite.test_case("range");
        auto const found2 = RAH2_NS::ranges::starts_with(haystack, needle);
        CHECK(found2);

        // 'needle' range is empty:
        testSuite.test_case("empty");
        RAH2_STD::string none_;
        auto none = make_test_view_adapter<CS, Tag, Sized>(none_);
        auto const found3 = RAH2_NS::ranges::starts_with(haystack, none);
        CHECK(found3);

        // 'needle' will not be found:
        testSuite.test_case("notfound");
        RAH2_STD::string awl_{"efg"};
        auto awl = make_test_view_adapter<CS, Tag, Sized>(awl_);
        auto const found4 = RAH2_NS::ranges::starts_with(haystack, awl);
        CHECK(not found4);

        // the search uses custom comparator and projections:
        testSuite.test_case("proj");
        RAH2_STD::string bodkin_{"123"};
        auto bodkin = make_test_view_adapter<CS, Tag, Sized>(bodkin_);
        auto const found5 = RAH2_NS::ranges::starts_with(
            haystack,
            bodkin,
            [](int const x, int const y) { return x == y; }, // pred
            [](int const x) { return std::toupper(x); }, // proj1
            [](int const y) { return y + 'A' - '1'; }); // proj2
        CHECK(found5);
    }

    template <bool = true>
    void test_perf(char const* range_type)
    {
        testSuite.test_case("perf");
        RAH2_STD::string haystack_;
        for (size_t i = 0; i < 1000000 * RELEASE_MULTIPLIER; ++i)
        {
            haystack_.push_back('a' + (i % 7));
        }
        haystack_ += "abcdefgh cd";
        auto haystack = make_test_view_adapter<CS, Tag, Sized>(haystack_);
        (void)haystack;

        RAH2_STD::string needle_ = haystack_;
        needle_.pop_back();
        auto needle = make_test_view_adapter<CS, Tag, Sized>(needle_);
        (void)needle;

        RAH2_STD::string haystack_proj_;
        for (size_t i = 0; i < 1000000 * RELEASE_MULTIPLIER; ++i)
        {
            haystack_proj_.push_back('a' + (i % 3));
        }
        haystack_proj_ += "abcdefgh cd";
        (void)haystack_proj_;
        auto haystack_proj = make_test_view_adapter<CS, Tag, Sized>(haystack_proj_);

        RAH2_STD::string needle2_ = haystack_proj_;
        needle2_.pop_back();
        auto needle2 = make_test_view_adapter<CS, Tag, Sized>(needle2_);
        (void)needle2;

        {
            COMPARE_DURATION_TO_STD_RANGES_23(
                "starts_with",
                range_type,
                [&]
                {
                    // the search uses iterator pairs begin()/end():
                    auto const found1 = STD::starts_with(
                        fwd(haystack.begin()), haystack.end(), needle.begin(), needle.end());
                    CHECK(found1);
                });
        }
        {
            COMPARE_DURATION_TO_STD_RANGES_23(
                "starts_with_proj",
                range_type,
                (
                    [&]
                    {
                        auto const found5 = STD::starts_with(
                            haystack_proj,
                            needle2,
                            [](int const x, int const y) { return x == y; },
                            [](int const x) { return std::toupper(x); },
                            [](int const y) { return std::toupper(y); });
                        CHECK(found5);
                    }));
        }
    }
    static constexpr bool do_test = true;
};
void test_starts_with()
{
    testSuite.test_case("sample");
    testSuite.test_case("range");
    /// [rah2::ranges::starts_with]
    using namespace std::literals;

    auto ascii_upper = [](char c)
    {
        return 'a' <= c && c <= 'z' ? static_cast<char>(c + 'A' - 'a') : c;
    };

    auto cmp_ignore_case = [=](char x, char y)
    {
        return ascii_upper(x) == ascii_upper(y);
    };

    assert(RAH2_NS::ranges::starts_with("const_cast", RAH2_STD::string("const")));
    assert(RAH2_NS::ranges::starts_with("constexpr", RAH2_STD::string("const")));
    assert(!RAH2_NS::ranges::starts_with("volatile", RAH2_STD::string("const")));

    assert(RAH2_NS::ranges::starts_with(
        "Constantinopolis", RAH2_STD::string("constant"), {}, ascii_upper, ascii_upper));
    assert(not RAH2_NS::ranges::starts_with(
        "Istanbul", RAH2_STD::string("constant"), {}, ascii_upper, ascii_upper));
    assert(RAH2_NS::ranges::starts_with("Metropolis", RAH2_STD::string("metro"), cmp_ignore_case));
    assert(not RAH2_NS::ranges::starts_with("Acropolis", RAH2_STD::string("metro"), cmp_ignore_case));

    auto v = {1, 3, 5, 7, 9};
    auto odd = [](int x)
    {
        return x % 2;
    };
    assert(RAH2_NS::ranges::starts_with(
        v,
        RAH2_NS::ranges::views::iota(1) | RAH2_NS::ranges::views::filter(odd)
            | RAH2_NS::ranges::views::take(3)));
    /// [rah2::ranges::starts_with]

    testSuite.test_case("iter");
    RAH2_STD::string acropolis = "Acropolis";
    RAH2_STD::string metro = "metro";
    assert(not RAH2_NS::ranges::starts_with(
        acropolis.begin(), acropolis.end(), metro.begin(), metro.end(), cmp_ignore_case));

    foreach_range_combination<test_algo<test_starts_with_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_ends_with_
{
    template <bool = true>
    void test()
    {
        RAH2_STD::string haystack_{"abcd abcd"};
        RAH2_STD::string needle_{"bcd"};
        auto haystack = make_test_view_adapter<CS, Tag, Sized>(haystack_);
        auto needle = make_test_view_adapter<CS, Tag, Sized>(needle_);

        testSuite.test_case("iter");
        auto const found1 = RAH2_NS::ranges::ends_with(
            haystack.begin(), haystack.end(), needle.begin(), needle.end());
        CHECK(found1);

        // the search uses ranges r1, r2:
        testSuite.test_case("range");
        auto const found2 = RAH2_NS::ranges::ends_with(haystack, needle);
        CHECK(found2);

        // 'needle' range is empty:
        testSuite.test_case("empty");
        RAH2_STD::string none_;
        auto none = make_test_view_adapter<CS, Tag, Sized>(none_);
        auto const found3 = RAH2_NS::ranges::ends_with(haystack, none);
        CHECK(found3);

        // 'needle' will not be found:
        testSuite.test_case("notfound");
        RAH2_STD::string awl_{"efg"};
        auto awl = make_test_view_adapter<CS, Tag, Sized>(awl_);
        auto const found4 = RAH2_NS::ranges::ends_with(haystack, awl);
        CHECK(not found4);

        // the search uses custom comparator and projections:
        testSuite.test_case("proj");
        RAH2_STD::string bodkin_{"234"};
        auto bodkin = make_test_view_adapter<CS, Tag, Sized>(bodkin_);
        auto const found5 = RAH2_NS::ranges::ends_with(
            haystack,
            bodkin,
            [](int const x, int const y) { return x == y; }, // pred
            [](int const x) { return std::toupper(x); }, // proj1
            [](int const y) { return y + 'A' - '1'; }); // proj2
        CHECK(found5);
    }

    template <bool = true>
    void test_perf(char const* range_type)
    {
        testSuite.test_case("perf");
        RAH2_STD::string haystack_;
        for (size_t i = 0; i < 1000000 * RELEASE_MULTIPLIER; ++i)
        {
            haystack_.push_back('a' + (i % 7));
        }
        haystack_ += "abcdefgh cd";
        auto haystack = make_test_view_adapter<CS, Tag, Sized>(haystack_);
        (void)haystack;

        auto b = haystack_.begin();
        ++b;
        RAH2_STD::string needle_(b, haystack_.end());
        auto needle = make_test_view_adapter<CS, Tag, Sized>(needle_);
        (void)needle;

        RAH2_STD::string haystack_proj_;
        for (size_t i = 0; i < 1000000 * RELEASE_MULTIPLIER; ++i)
        {
            haystack_proj_.push_back('a' + (i % 3));
        }
        haystack_proj_ += "abcdefghhcd";
        (void)haystack_proj_;

        RAH2_STD::string bodkin_;
        for (size_t i = 1; i < 1000000 * RELEASE_MULTIPLIER; ++i)
        {
            bodkin_.push_back('1' + (i % 3));
        }
        bodkin_ += "12345678834";
        auto bodkin = make_test_view_adapter<CS, Tag, Sized>(bodkin_);

        auto haystack_proj = make_test_view_adapter<CS, Tag, Sized>(haystack_proj_);
        auto const found5 = RAH2_NS::ranges::ends_with(
            haystack_proj,
            bodkin,
            [](int const x, int const y) { return x == y; },
            [](int const x) { return std::toupper(x); },
            [](int const y) { return (y - '1') + 'A'; });
        CHECK(found5);

        {
            COMPARE_DURATION_TO_STD_RANGES_23(
                "ends_with",
                range_type,
                [&]
                {
                    // the search uses iterator pairs begin()/end():
                    auto const found1 = STD::ends_with(
                        fwd(haystack.begin()), haystack.end(), needle.begin(), needle.end());
                    CHECK(found1);
                });
        }

        {
            COMPARE_DURATION_TO_STD_RANGES_23(
                "ends_with_proj",
                range_type,
                (
                    [&]
                    {
                        auto haystack_proj = make_test_view_adapter<CS, Tag, Sized>(haystack_proj_);
                        auto const found5 = STD::ends_with(
                            haystack_proj,
                            bodkin,
                            [](int const x, int const y) { return x == y; },
                            [](int const x) { return std::toupper(x); },
                            [](int const y) { return (y - '1') + 'A'; });
                        CHECK(found5);
                    }));
        }
    }
    static constexpr bool do_test =
        RAH2_NS::derived_from<Tag, RAH2_NS::forward_iterator_tag>; //  Sized mean sized_range but we need sized_sentinel here
};
void test_ends_with()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::ends_with]
    assert(RAH2_NS::ranges::ends_with("static_cast", "cast"));
    assert(RAH2_NS::ranges::ends_with("const_cast", "cast"));
    assert(RAH2_NS::ranges::ends_with("reinterpret_cast", "cast"));
    assert(RAH2_NS::ranges::ends_with("dynamic_cast", "cast"));
    assert(not RAH2_NS::ranges::ends_with("move", "cast"));
    assert(not RAH2_NS::ranges::ends_with("move_if_noexcept", "cast"));
    assert(not RAH2_NS::ranges::ends_with("forward", "cast"));
    assert(!RAH2_NS::ranges::ends_with("as_const", "cast"));
    assert(!!RAH2_NS::ranges::ends_with("bit_cast", "cast"));
    assert(!RAH2_NS::ranges::ends_with("to_underlying", "cast"));
    assert(!!RAH2_NS::ranges::ends_with(
        RAH2_STD::vector<int>{1, 2, 3, 4}, RAH2_STD::vector<int>{3, 4}));
    assert(!RAH2_NS::ranges::ends_with(
        RAH2_STD::vector<int>{1, 2, 3, 4}, RAH2_STD::vector<int>{4, 5}));
    /// [rah2::ranges::ends_with]

    foreach_range_combination<test_algo<test_ends_with_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_copy_
{
    template <bool = true>
    void test()
    {
        RAH2_STD::vector<int> in_{1, 2, 3};
        auto in = make_test_view_adapter<CS, Tag, Sized>(in_);
        RAH2_STD::vector<int> out{0, 0, 0, 4, 5};
        testSuite.test_case("iter");
        auto result =
            RAH2_NS::ranges::copy(RAH2_NS::ranges::begin(in), RAH2_NS::ranges::end(in), out.begin());
        CHECK(result.out == out.begin() + in_.size());
        CHECK(result.in == in.end());
        CHECK(out == (RAH2_STD::vector<int>{1, 2, 3, 4, 5}));

        testSuite.test_case("range");
        auto result2 = RAH2_NS::ranges::copy(in, out.begin());
        CHECK(result2.out == out.begin() + in_.size());
        CHECK(result2.in == in.end());
        CHECK(out == (RAH2_STD::vector<int>{1, 2, 3, 4, 5}));

        testSuite.test_case("empty");
        {
            RAH2_STD::vector<int> empty_in_;
            auto empty_in = make_test_view_adapter<CS, Tag, Sized>(empty_in_);
            RAH2_STD::vector<int> empty_out;

            auto result3 = RAH2_NS::ranges::copy(empty_in, empty_out.begin());
            CHECK(result3.out == empty_out.begin() + empty_in_.size());
            CHECK(result3.in == empty_in.end());
            CHECK(empty_out.empty());
        }
    }

    template <bool = true>
    void test_perf(char const* range_type)
    {
        testSuite.test_case("perf");
        RAH2_STD::vector<int> in_;
        for (size_t i = 0; i < 1000000 * RELEASE_MULTIPLIER; ++i)
        {
            in_.push_back(i % 15);
        }
        auto in = make_test_view_adapter<CS, Tag, Sized>(in_);
        RAH2_STD::vector<int> out;
        out.resize(1000000 * RELEASE_MULTIPLIER);
        out.emplace_back();
        out.emplace_back();

        {
            COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(
                CS == Common,
                "copy_iter",
                range_type,
                [&]
                {
                    auto result = STD::copy(
                        RAH2_NS::ranges::begin(fwd(in)), RAH2_NS::ranges::end(in), out.begin());
                    DONT_OPTIM(result);
                });
        }

        {
            COMPARE_DURATION_TO_STD_RANGES(
                "copy_ranges",
                range_type,
                (
                    [&]
                    {
                        auto result2 = STD::copy(in, out.begin());
                        CHECK(result2.out == out.begin() + in_.size());
                        CHECK(result2.in == in.end());
                    }));
        }
    }
    static constexpr bool do_test = true;
};
void test_copy()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::copy]
    RAH2_STD::vector<int> in{1, 2, 3};
    RAH2_STD::vector<int> out{0, 0, 0, 4, 5};
    assert(RAH2_NS::ranges::equal(
        RAH2_NS::ranges::make_subrange(RAH2_NS::ranges::copy(in, out.begin()).out, end(out)),
        std::initializer_list<int>({4, 5})));
    assert(out == (RAH2_STD::vector<int>{1, 2, 3, 4, 5}));
    /// [rah2::ranges::copy]

    foreach_range_combination<test_algo<test_copy_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_copy_if_
{
    template <bool = true>
    void test()
    {
        RAH2_STD::vector<int> in_{1, 2, 3, 4, 2, 3, 1};
        auto in = make_test_view_adapter<CS, Tag, Sized>(in_);
        RAH2_STD::vector<int> out{0, 0, 0, 0, 0, 0, 4, 5};
        testSuite.test_case("iter");
        auto result = RAH2_NS::ranges::copy_if(
            RAH2_NS::ranges::begin(in),
            RAH2_NS::ranges::end(in),
            out.begin(),
            [](auto i) { return i != 4; });
        CHECK(result.out == out.begin() + (in_.size() - 1));
        CHECK(result.in == in.end());
        CHECK(out == (RAH2_STD::vector<int>{1, 2, 3, 2, 3, 1, 4, 5}));

        testSuite.test_case("range");
        auto result2 = RAH2_NS::ranges::copy_if(in, out.begin(), [](auto i) { return i != 4; });
        CHECK(result2.out == out.begin() + (in_.size() - 1));
        CHECK(result2.in == in.end());
        CHECK(out == (RAH2_STD::vector<int>{1, 2, 3, 2, 3, 1, 4, 5}));

        testSuite.test_case("empty");
        {
            RAH2_STD::vector<int> empty_in_;
            auto empty_in = make_test_view_adapter<CS, Tag, Sized>(empty_in_);
            RAH2_STD::vector<int> empty_out;

            auto result3 =
                RAH2_NS::ranges::copy_if(empty_in, empty_out.begin(), [](auto i) { return i != 4; });
            CHECK(result3.out == empty_out.begin() + empty_in_.size());
            CHECK(result3.in == empty_in.end());
            CHECK(empty_out.empty());
        }
    }

    template <bool = true>
    void test_perf(char const* range_type)
    {
        testSuite.test_case("perf");
        RAH2_STD::vector<int> in_;
        for (size_t i = 0; i < 1000000 * RELEASE_MULTIPLIER; ++i)
        {
            in_.push_back(i % 15);
        }
        auto in = make_test_view_adapter<CS, Tag, Sized>(in_);
        RAH2_STD::vector<int> out;
        out.resize(1000000 * RELEASE_MULTIPLIER);
        out.emplace_back();
        out.emplace_back();

        RAH2_STD::vector<Coord> in2_;
        for (intptr_t i = 0; i < 1000000 * RELEASE_MULTIPLIER; ++i)
        {
            in2_.emplace_back(Coord{i % 15, 0});
        }
        auto in2 = make_test_view_adapter<CS, Tag, Sized>(in2_);

        RAH2_STD::vector<Coord> out2;
        out2.resize(1000000 * RELEASE_MULTIPLIER);
        out2.emplace_back();
        out2.emplace_back();

        {
            COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(
                CS == Common,
                "copy_if",
                range_type,
                [&]
                {
                    auto result = STD::copy_if(
                        fwd(RAH2_NS::ranges::begin(in)),
                        RAH2_NS::ranges::end(in),
                        out.begin(),
                        [](auto) { return true; });
                    DONT_OPTIM(result);
                });
        }

        {
            COMPARE_DURATION_TO_STD_RANGES(
                "copy_if_proj",
                range_type,
                (
                    [&]
                    {
                        auto result2 = STD::copy_if(
                            in2, out2.begin(), [](intptr_t i) { return i >= 0; }, &Coord::x);
                        DONT_OPTIM(result2);
                    }));
        }
    }
    static constexpr bool do_test = true;
};
void test_copy_if()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::copy_if]
    RAH2_STD::vector<int> in{1, 2, 3, 4};
    RAH2_STD::vector<int> out{0, 0, 5, 6};
    assert(RAH2_NS::ranges::equal(
        RAH2_NS::ranges::make_subrange(
            RAH2_NS::ranges::copy_if(in, out.begin(), [](int i) { return i % 2 == 0; }).out, end(out)),
        std::initializer_list<int>({5, 6})));
    assert(out == (RAH2_STD::vector<int>{2, 4, 5, 6}));
    /// [rah2::ranges::copy_if]

    foreach_range_combination<test_algo<test_copy_if_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_copy_n_
{
    template <bool = true>
    void test()
    {
        RAH2_STD::string const in_{"ABCDEFGH"};
        auto in = make_test_view_adapter<CS, Tag, Sized>(in_);
        RAH2_STD::string out = "123456789";

        testSuite.test_case("iter");
        auto const res = RAH2_NS::ranges::copy_n(in.begin(), 5, out.begin());
        assert(out == "ABCDE6789");
        assert(*(res.in) == 'F');
        assert(*(res.out) == '6');
        assert((RAH2_NS::ranges::distance(out.begin(), res.out)) == 5);

        testSuite.test_case("empty");
        out = "123456789";
        auto const res2 = RAH2_NS::ranges::copy_n(in.begin(), 0, out.begin());
        assert(*(res2.in) == 'A');
        assert(*(res2.out) == '1');
        CHECK(out == "123456789");
    }

    template <bool = true>
    void test_perf(char const* range_type)
    {
        testSuite.test_case("perf");

        RAH2_STD::string const in_(1000000 * RELEASE_MULTIPLIER, '1');
        auto in = make_test_view_adapter<CS, Tag, Sized>(in_);
        RAH2_STD::string out(1000000 * RELEASE_MULTIPLIER, '0');

        {
#ifdef RAH2_USE_EASTL
            // EASTL doesn't know about input_iterator type and so use the forward incrementation on iterator. (ex : "*result++ = *first++;")
            constexpr bool static DoTest = RAH2_NS::derived_from<Tag, RAH2_NS::forward_iterator_tag>;
#else
            constexpr bool static DoTest = true;
#endif
            COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(
                DoTest,
                "copy_n",
                range_type,
                (
                    [&]
                    {
                        auto const res = STD::copy_n(
                            fwd(in.begin()), 1000000 * RELEASE_MULTIPLIER - 5, out.begin());
                        DONT_OPTIM(res);
                    }));
        }
    }
    static constexpr bool do_test = true;
};
void test_copy_n()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::copy_n]
    RAH2_STD::string const in{"ABCDEFGH"};
    RAH2_STD::string out;

    RAH2_NS::ranges::copy_n(in.begin(), 4, RAH2_NS::back_inserter(out));
    assert(out == "ABCD");

    out = "abcdefgh";
    auto const res = RAH2_NS::ranges::copy_n(in.begin(), 5, out.begin());
    assert(*(res.in) == 'F');
    assert(*(res.out) == 'f');
    assert(RAH2_STD::distance(RAH2_STD::begin(in), res.in) == 5);
    assert(RAH2_STD::distance(RAH2_STD::begin(out), res.out) == 5);
    /// [rah2::ranges::copy_n]

    foreach_range_combination<test_algo<test_copy_n_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_copy_backward_
{
    template <bool = true>
    void test()
    {
        RAH2_STD::string const in_{"ABCDEFGH"};
        auto in = make_test_view_adapter<CS, Tag, Sized>(in_);
        RAH2_STD::string out = "123456789";

        testSuite.test_case("iter");
        auto const res = RAH2_NS::ranges::copy_backward(in.begin(), in.end(), out.end());
        CHECK_EQUAL(out, "1ABCDEFGH");
        CHECK_EQUAL(res.in, in.end());
        CHECK_EQUAL(*(res.out), 'A');
        CHECK_EQUAL((RAH2_NS::ranges::distance(res.out, out.end())), 8);

        testSuite.test_case("empty");
        out = "123456789";
        auto const res2 = RAH2_NS::ranges::copy_backward(in.begin(), in.begin(), out.end());
        CHECK_EQUAL(res2.in, in.begin());
        CHECK(res2.out == out.end());
        CHECK_EQUAL(out, "123456789");
    }

    template <bool = true>
    void test_perf(char const* range_type)
    {
        testSuite.test_case("perf");

        RAH2_STD::string const in_(1000000 * RELEASE_MULTIPLIER, '1');
        auto in = make_test_view_adapter<CS, Tag, Sized>(in_);
        RAH2_STD::string out(1000000 * RELEASE_MULTIPLIER, '0');

        {
            COMPARE_DURATION_TO_STD_RANGES(
                "copy_backward_range",
                range_type,
                (
                    [&]
                    {
                        auto const res = STD::copy_backward(in, out.end());
                        DONT_OPTIM(res);
                    }));
        }
        {
            COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(
                CS == Common,
                "copy_backward_iter",
                range_type,
                [&]
                {
                    auto const res = STD::copy_backward(fwd(in.begin()), fwd(in.end()), out.end());
                    DONT_OPTIM(res);
                });
        }
    }
    static constexpr bool do_test = RAH2_NS::derived_from<Tag, RAH2_NS::bidirectional_iterator_tag>;
};
void test_copy_backward()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::copy_backward]
    auto const src = {1, 2, 3, 4};

    RAH2_STD::vector<int> dst(src.size() + 2);
    RAH2_NS::ranges::copy_backward(src, dst.end());
    assert(dst == (RAH2_STD::vector<int>{0, 0, 1, 2, 3, 4}));

    RAH2_NS::ranges::fill(dst, 0);
    auto const in_out = RAH2_NS::ranges::copy_backward(src.begin(), src.end() - 2, dst.end());
    assert(dst == (RAH2_STD::vector<int>{0, 0, 0, 0, 1, 2}));

    assert(RAH2_NS::ranges::distance(src.begin(), in_out.in) == 2);
    assert(RAH2_NS::ranges::distance(dst.begin(), in_out.out) == 4);
    /// [rah2::ranges::copy_backward]

    foreach_range_combination<test_algo<test_copy_backward_>>();
}

struct NonCopyable
{
    int i_ = {};
    NonCopyable(int i)
        : i_(i)
    {
    }
    NonCopyable() = default;
    NonCopyable(NonCopyable const&) = delete;
    NonCopyable& operator=(NonCopyable const&) = delete;
    NonCopyable(NonCopyable&& nc)
    {
        i_ = nc.i_;
        nc.i_ = -1;
    }
    NonCopyable& operator=(NonCopyable&& nc)
    {
        i_ = nc.i_;
        nc.i_ = -1;
        return *this;
    }
    ~NonCopyable() = default;
    bool operator==(NonCopyable const& np) const
    {
        return i_ == np.i_;
    }

    friend std::ostream& operator<<(std::ostream& os, NonCopyable const& nc)
    {
        os << "NonCopyable[" << nc.i_ << "]";
        return os;
    }
};

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_move_
{
    template <bool = true>
    void test()
    {
        RAH2_STD::array<NonCopyable, 3> in_{NonCopyable{1}, NonCopyable{2}, NonCopyable{3}};
        auto in = make_test_view_adapter<CS, Tag, Sized>(in_);
        RAH2_STD::array<NonCopyable, 5> out{
            NonCopyable{0}, NonCopyable{0}, NonCopyable{0}, NonCopyable{4}, NonCopyable{5}};
        testSuite.test_case("iter");
        auto result =
            RAH2_NS::ranges::move(RAH2_NS::ranges::begin(in), RAH2_NS::ranges::end(in), out.begin());
        CHECK(result.out == out.begin() + in_.size());
        CHECK(result.in == in.end());
        CHECK(
            out
            == (RAH2_STD::array<NonCopyable, 5>{
                NonCopyable{1}, NonCopyable{2}, NonCopyable{3}, NonCopyable{4}, NonCopyable{5}}));

        testSuite.test_case("range");
        RAH2_STD::array<NonCopyable, 3> in2_{NonCopyable{1}, NonCopyable{2}, NonCopyable{3}};
        auto in2 = make_test_view_adapter<CS, Tag, Sized>(in2_);
        auto result2 = RAH2_NS::ranges::move(in2, out.begin());
        CHECK(result2.out == out.begin() + in_.size());
        CHECK(result2.in == in2.end());
        CHECK(
            out
            == (RAH2_STD::array<NonCopyable, 5>{
                NonCopyable{1}, NonCopyable{2}, NonCopyable{3}, NonCopyable{4}, NonCopyable{5}}));

        testSuite.test_case("empty");
        {
            RAH2_STD::vector<NonCopyable> empty_in_;
            auto empty_in = make_test_view_adapter<CS, Tag, Sized>(empty_in_);
            RAH2_STD::vector<NonCopyable> empty_out;

            auto result3 = RAH2_NS::ranges::move(empty_in, empty_out.begin());
            CHECK(result3.out == empty_out.begin() + empty_in_.size());
            CHECK(result3.in == empty_in.end());
            CHECK(empty_out.empty());
        }
    }

    template <bool = true>
    void test_perf(char const* range_type)
    {
        testSuite.test_case("perf");
        RAH2_STD::vector<NonCopyable> in_;
        for (size_t i = 0; i < 1000000 * RELEASE_MULTIPLIER; ++i)
        {
            in_.push_back(NonCopyable{int(i) % 15});
        }
        auto in = make_test_view_adapter<CS, Tag, Sized>(in_);

        RAH2_STD::vector<NonCopyable> in2_;
        for (size_t i = 0; i < 1000000 * RELEASE_MULTIPLIER; ++i)
        {
            in2_.push_back(NonCopyable{int(i) % 15});
        }
        auto in2 = make_test_view_adapter<CS, Tag, Sized>(in2_);

        RAH2_STD::vector<NonCopyable> out;
        out.resize(1000000 * RELEASE_MULTIPLIER);
        out.emplace_back();
        out.emplace_back();

        {
            COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(
                CS == Common,
                "move_iter",
                range_type,
                [&]
                {
                    auto result = STD::move(
                        RAH2_NS::ranges::begin(fwd(in2)), RAH2_NS::ranges::end(in2), out.begin());
                    DONT_OPTIM(result);
                });
        }

        {
            COMPARE_DURATION_TO_STD_RANGES(
                "move_ranges",
                range_type,
                (
                    [&]
                    {
                        auto result2 = STD::move(in, out.begin());
                        CHECK(result2.out == out.begin() + in_.size());
                        CHECK(result2.in == in.end());
                    }));
        }
    }
    static constexpr bool do_test = true;
};
void test_move()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::move]
    struct NonCopyable
    {
        NonCopyable() = default;
        NonCopyable(NonCopyable const&) = delete;
        NonCopyable& operator=(NonCopyable const&) = delete;
        NonCopyable(NonCopyable&&) = default;
        NonCopyable& operator=(NonCopyable&&) = default;
        ~NonCopyable() = default;
    };
    RAH2_STD::vector<NonCopyable> v;
    v.emplace_back();
    v.emplace_back();
    v.emplace_back();

    RAH2_STD::list<NonCopyable> l;
    auto res = RAH2_NS::ranges::move(v, RAH2_NS::back_inserter(l));
    assert(res.in == v.end());
    /// [rah2::ranges::move]

    foreach_range_combination<test_algo<test_move_>>();
}

auto makeNonCopyableVec = [](RAH2_STD::vector<char> const& inChar)
{
    RAH2_STD::vector<NonCopyable> in_;
    RAH2_STD::copy(inChar.begin(), inChar.end(), RAH2_STD::back_inserter(in_));
    return in_;
};

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_move_backward_
{
    template <bool = true>
    void test()
    {
        RAH2_STD::vector<NonCopyable> in_ =
            makeNonCopyableVec({'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H'});
        auto in = make_test_view_adapter<CS, Tag, Sized>(in_);
        RAH2_STD::vector<NonCopyable> out =
            makeNonCopyableVec({'1', '2', '3', '4', '5', '6', '7', '8', '9'});

        testSuite.test_case("iter");
        auto const res = RAH2_NS::ranges::move_backward(in.begin(), in.end(), out.end());
        CHECK_EQUAL(out, makeNonCopyableVec({'1', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H'}));
        CHECK_EQUAL(res.in, in.end());
        CHECK_EQUAL(*(res.out), 'A');
        CHECK_EQUAL((RAH2_NS::ranges::distance(res.out, out.end())), 8);

        testSuite.test_case("empty");
        out = makeNonCopyableVec({'1', '2', '3', '4', '5', '6', '7', '8', '9'});
        auto const res2 = RAH2_NS::ranges::move_backward(in.begin(), in.begin(), out.end());
        CHECK_EQUAL(res2.in, in.begin());
        CHECK(res2.out == out.end());
        CHECK_EQUAL(out, makeNonCopyableVec({'1', '2', '3', '4', '5', '6', '7', '8', '9'}));
    }

    template <bool = true>
    void test_perf(char const* range_type)
    {
        testSuite.test_case("perf");

        RAH2_STD::string const in_(1000000 * RELEASE_MULTIPLIER, '1');
        auto in = make_test_view_adapter<CS, Tag, Sized>(in_);
        RAH2_STD::string out(1000000 * RELEASE_MULTIPLIER, '0');

        {
            COMPARE_DURATION_TO_STD_RANGES(
                "move_backward",
                range_type,
                (
                    [&]
                    {
                        auto const res = STD::move_backward(in, out.end());
                        DONT_OPTIM(res);
                    }));
        }
        {
            COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(
                CS == Common,
                "move_backward",
                range_type,
                [&]
                {
                    auto const res = STD::move_backward(fwd(in.begin()), fwd(in.end()), out.end());
                    DONT_OPTIM(res);
                });
        }
    }
    static constexpr bool do_test = RAH2_NS::derived_from<Tag, RAH2_NS::bidirectional_iterator_tag>;
};
void test_move_backward()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::move_backward]
    using Vec = RAH2_STD::vector<RAH2_STD::string>;
    Vec a{"▁", "▂", "▃", "▄", "▅", "▆", "▇", "█"};
    Vec b(a.size());

    RAH2_NS::ranges::move_backward(a, b.end());
    assert(a == (Vec{"", "", "", "", "", "", "", ""}));
    assert(b == (Vec{"▁", "▂", "▃", "▄", "▅", "▆", "▇", "█"}));

    RAH2_NS::ranges::move_backward(b.begin(), b.end(), a.end());
    assert(b == (Vec{"", "", "", "", "", "", "", ""}));
    assert(a == (Vec{"▁", "▂", "▃", "▄", "▅", "▆", "▇", "█"}));

    RAH2_NS::ranges::move_backward(a.begin(), a.begin() + 3, a.end());
    a[0] = a[1] = a[2] = ""; // can sometimes swap in place of move
    assert(a == (Vec{"", "", "", "▄", "▅", "▁", "▂", "▃"}));
    /// [rah2::ranges::move_backward]

    foreach_range_combination<test_algo<test_move_backward_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_fill_
{
    template <bool = true>
    void test()
    {
        RAH2_STD::vector<int> out{0, 0, 0, 4, 5};
        testSuite.test_case("iter");
        auto result = RAH2_NS::ranges::fill(out.begin(), out.end(), 12);
        CHECK(result == out.end());
        CHECK(out == (RAH2_STD::vector<int>{12, 12, 12, 12, 12}));

        testSuite.test_case("range");
        auto result2 = RAH2_NS::ranges::fill(out, 72);
        CHECK(result2 == out.end());
        CHECK(out == (RAH2_STD::vector<int>{72, 72, 72, 72, 72}));

        testSuite.test_case("empty");
        {
            RAH2_STD::vector<int> empty_out;

            auto result3 = RAH2_NS::ranges::fill(empty_out.begin(), empty_out.end(), 169);
            CHECK(result3 == empty_out.end());
            CHECK(empty_out.empty());
        }
    }

    template <bool = true>
    void test_perf(char const* range_type)
    {
        testSuite.test_case("perf");
        RAH2_STD::vector<int> out;
        out.resize(1000000 * RELEASE_MULTIPLIER);
        out.emplace_back();
        out.emplace_back();

        {
            COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(
                CS == Common,
                "fill_iter",
                range_type,
                [&]
                {
                    STD::fill(RAH2_NS::ranges::begin(fwd(out)), RAH2_NS::ranges::end(out), 16);
                    DONT_OPTIM(out.back());
                });
        }

        {
            COMPARE_DURATION_TO_STD_RANGES(
                "fill_ranges",
                range_type,
                (
                    [&]
                    {
                        auto result2 = STD::fill(out, 27);
                        CHECK(result2 == out.end());
                    }));
        }
    }
    // Only use an output iterator, so no need to test several input iterator types
    static constexpr bool do_test = true;
};
void test_fill()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::fill]
    RAH2_STD::vector<int> out{0, 0, 0, 4, 5};
    RAH2_NS::ranges::fill(out, 42);
    assert(out == (RAH2_STD::vector<int>{42, 42, 42, 42, 42}));
    RAH2_NS::ranges::fill(out.begin(), out.end(), 78);
    assert(out == (RAH2_STD::vector<int>{78, 78, 78, 78, 78}));
    /// [rah2::ranges::fill]

    foreach_range_combination<test_algo<test_fill_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_fill_n_
{
    template <bool = true>
    void test()
    {
        RAH2_STD::vector<int> out{0, 0, 0, 4, 5};
        testSuite.test_case("iter");
        auto result = RAH2_NS::ranges::fill_n(out.begin(), 4, 12);
        CHECK(result == out.end() - 1);
        CHECK(out == (RAH2_STD::vector<int>{12, 12, 12, 12, 5}));

        testSuite.test_case("empty");
        {
            RAH2_STD::vector<int> empty_out;
            auto result3 = RAH2_NS::ranges::fill_n(empty_out.begin(), 0, 169);
            CHECK(result3 == empty_out.end());
            CHECK(empty_out.empty());
        }
    }

    template <bool = true>
    void test_perf(char const* range_type)
    {
        testSuite.test_case("perf");
        RAH2_STD::vector<int> out;
        out.resize(1000000 * RELEASE_MULTIPLIER);
        out.emplace_back();
        out.emplace_back();

        {
            COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(
                CS == Common,
                "fill_n_iter",
                range_type,
                [&]
                {
                    auto result = STD::fill_n(
                        RAH2_NS::ranges::begin(fwd(out)), 1000000 * RELEASE_MULTIPLIER, 16);
                    CHECK(result == (out.end() - 2));
                });
        }
    }
    // Only use an output iterator, so no need to test several input iterator types
    static constexpr bool do_test = true;
};
void test_fill_n()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::fill_n]
    RAH2_STD::vector<int> out(5);
    RAH2_NS::ranges::fill_n(out.begin(), 4, 42);
    assert(out == (RAH2_STD::vector<int>{42, 42, 42, 42, 0}));
    /// [rah2::ranges::fill_n]

    foreach_range_combination<test_algo<test_fill_n_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_transform_
{
    template <bool = true>
    void test()
    {
        RAH2_STD::vector<int> in_{1, 2, 3};
        RAH2_STD::vector<int> in2_{10, 20, 30};
        auto in = make_test_view_adapter<CS, Tag, Sized>(in_);
        auto in2 = make_test_view_adapter<CS, Tag, Sized>(in2_);
        RAH2_STD::vector<int> out{0, 0, 0, 4, 5};
        testSuite.test_case("iter");
        // one entry
        auto result = RAH2_NS::ranges::transform(
            RAH2_NS::ranges::begin(in),
            RAH2_NS::ranges::end(in),
            out.begin(),
            [](int a) { return a + 1; });
        CHECK(result.out == out.begin() + in_.size());
        CHECK(result.in == in.end());
        CHECK(out == (RAH2_STD::vector<int>{2, 3, 4, 4, 5}));
        // two entry
        auto result2 = RAH2_NS::ranges::transform(
            RAH2_NS::ranges::begin(in),
            RAH2_NS::ranges::end(in),
            RAH2_NS::ranges::begin(in2),
            RAH2_NS::ranges::end(in2),
            out.begin(),
            [](int a, int b) { return a + b; });
        CHECK(result2.out == out.begin() + in_.size());
        CHECK(result2.in1 == in.end());
        CHECK(result2.in2 == in2.end());
        CHECK(out == (RAH2_STD::vector<int>{11, 22, 33, 4, 5}));

        testSuite.test_case("range");
        // one entry
        auto result3 = RAH2_NS::ranges::transform(in, out.begin(), [](auto i) { return i; });
        CHECK(result3.out == out.begin() + in_.size());
        CHECK(result3.in == in.end());
        CHECK(out == (RAH2_STD::vector<int>{1, 2, 3, 4, 5}));
        // two entry
        auto result4 =
            RAH2_NS::ranges::transform(in, in2, out.begin(), [](int a, int b) { return a + b; });
        CHECK(result4.out == out.begin() + in_.size());
        CHECK(result4.in1 == in.end());
        CHECK(result4.in2 == in2.end());
        CHECK(out == (RAH2_STD::vector<int>{11, 22, 33, 4, 5}));

        testSuite.test_case("proj");
        // iter
        // one entry
        auto result5 = RAH2_NS::ranges::transform(
            RAH2_NS::ranges::begin(in),
            RAH2_NS::ranges::end(in),
            out.begin(),
            [](int a) { return a; },
            [](int a) { return a + 1; });
        CHECK(result5.out == out.begin() + in_.size());
        CHECK(result5.in == in.end());
        CHECK(out == (RAH2_STD::vector<int>{2, 3, 4, 4, 5}));
        // two entry
        auto result6 = RAH2_NS::ranges::transform(
            RAH2_NS::ranges::begin(in),
            RAH2_NS::ranges::end(in),
            RAH2_NS::ranges::begin(in2),
            RAH2_NS::ranges::end(in2),
            out.begin(),
            [](int a, int b) { return a + b; },
            [](int a) { return a + 1; },
            [](int a) { return a + 2; });
        CHECK(result6.out == out.begin() + in_.size());
        CHECK(result6.in1 == in.end());
        CHECK(result6.in2 == in2.end());
        CHECK(out == (RAH2_STD::vector<int>{14, 25, 36, 4, 5}));
        // Range
        // one entry
        auto result7 = RAH2_NS::ranges::transform(
            in, out.begin(), [](auto i) { return i; }, [](int a) { return a + 1; });
        CHECK(result7.out == out.begin() + in_.size());
        CHECK(result7.in == in.end());
        CHECK(out == (RAH2_STD::vector<int>{2, 3, 4, 4, 5}));
        // two entry
        auto result8 = RAH2_NS::ranges::transform(
            in,
            in2,
            out.begin(),
            [](int a, int b) { return a + b; },
            [](int a) { return a + 1; },
            [](int a) { return a + 2; });
        CHECK(result8.out == out.begin() + in_.size());
        CHECK(result8.in1 == in.end());
        CHECK(result8.in2 == in2.end());
        CHECK(out == (RAH2_STD::vector<int>{14, 25, 36, 4, 5}));

        testSuite.test_case("empty");
        {
            RAH2_STD::vector<int> empty_in_;
            auto empty_in = make_test_view_adapter<CS, Tag, Sized>(empty_in_);
            RAH2_STD::vector<int> empty_out;

            auto result9 =
                RAH2_NS::ranges::transform(empty_in, empty_out.begin(), [](auto i) { return i; });
            CHECK(result9.out == empty_out.begin() + empty_in_.size());
            CHECK(result9.in == empty_in.end());
            CHECK(empty_out.empty());
        }
    }

    template <bool = true>
    void test_perf(char const* range_type)
    {
        testSuite.test_case("perf");
        RAH2_STD::vector<int> in_;
        for (size_t i = 0; i < 1000000 * RELEASE_MULTIPLIER; ++i)
        {
            in_.push_back(i % 15);
        }
        auto in = make_test_view_adapter<CS, Tag, Sized>(in_);
        RAH2_STD::vector<int> out;
        out.resize(1000000 * RELEASE_MULTIPLIER);
        out.emplace_back();
        out.emplace_back();

        {
            COMPARE_DURATION_TO_STD_RANGES(
                "transform_iter",
                range_type,
                [&]
                {
                    auto result = STD::transform(
                        RAH2_NS::ranges::begin(in),
                        RAH2_NS::ranges::end(in),
                        out.begin(),
                        [](auto i) { return i; });
                    CHECK(result.out == out.begin() + in_.size());
                    CHECK(result.in == in.end());
                });
        }

        {
            COMPARE_DURATION_TO_STD_RANGES(
                "transform_ranges",
                range_type,
                (
                    [&]
                    {
                        auto result2 = STD::transform(in, out.begin(), [](auto i) { return i; });
                        CHECK(result2.out == out.begin() + in_.size());
                        CHECK(result2.in == in.end());
                    }));
        }
    }
    static constexpr bool do_test = true;
};
void test_transform()
{
    testSuite.test_case("sample");
    {
        /// [rah2::ranges::transform]
        RAH2_STD::vector<int> vecIn1{0, 1, 2, 3};
        RAH2_STD::vector<int> vecOut{0, 0, 0, 0};
        RAH2_NS::ranges::transform(vecIn1, begin(vecOut), [](int a) { return a + 1; });
        assert(vecOut == RAH2_STD::vector<int>({1, 2, 3, 4}));
        /// [rah2::ranges::transform]
    }
    {
        /// [rah2::ranges::transform2]
        RAH2_STD::vector<int> vecIn1{0, 1, 2, 3};
        RAH2_STD::vector<int> const vecIn2{4, 3, 2, 1};
        RAH2_STD::vector<int> vecOut;
        RAH2_NS::ranges::transform(
            vecIn1, vecIn2, RAH2_NS::back_inserter(vecOut), [](int a, int b) { return a + b; });
        assert(vecOut == RAH2_STD::vector<int>({4, 4, 4, 4}));
        /// [rah2::ranges::transform2]
    }
    foreach_range_combination<test_algo<test_transform_>>();
}

struct test_generate_
{
    void test()
    {
        RAH2_STD::array<int, 8> v = {};
        testSuite.test_case("range");
        RAH2_NS::ranges::generate(v, [n = 1]() mutable { return n++; });
        assert(v == (RAH2_STD::array<int, 8>{1, 2, 3, 4, 5, 6, 7, 8}));

        testSuite.test_case("iter");
        RAH2_STD::array<int, 8> u = {};
        RAH2_NS::ranges::generate(u.begin(), u.end(), [n = 1]() mutable { return n++; });
        assert(u == (RAH2_STD::array<int, 8>{1, 2, 3, 4, 5, 6, 7, 8}));

        testSuite.test_case("empty");
        RAH2_STD::array<int, 3> e = {};
        RAH2_NS::ranges::generate(e.begin(), e.begin(), [n = 1]() mutable { return n++; });
        assert(e == (RAH2_STD::array<int, 3>{0, 0, 0}));
    }

    void test_perf()
    {
        testSuite.test_case("perf");
        RAH2_STD::vector<int> out;
        out.resize(1000000 * RELEASE_MULTIPLIER);
        char const* range_type = "no";

        {
            COMPARE_DURATION_TO_STD_RANGES(
                "generate_iter",
                range_type,
                [&]
                {
                    auto result =
                        STD::generate(out.begin(), out.end(), [n = 1]() mutable { return n++; });
                    CHECK(result == out.end());
                });
        }

        {
            COMPARE_DURATION_TO_STD_RANGES(
                "generate_ranges",
                range_type,
                (
                    [&]
                    {
                        auto result = STD::generate(out, [n = 1]() mutable { return n++; });
                        CHECK(result == out.end());
                    }));
        }
    }
};
void test_generate()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::generate]
    RAH2_STD::array<int, 8> v = {};
    RAH2_NS::ranges::generate(v, [n = 1]() mutable { return n++; });
    assert(v == (RAH2_STD::array<int, 8>{1, 2, 3, 4, 5, 6, 7, 8}));
    /// [rah2::ranges::generate]

    test_generate_ test_generate;
    test_generate.test();
#if defined(PERF_TEST)
    test_generate.test_perf();
#endif
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_generate_n_
{
    template <bool = true>
    void test()
    {
        testSuite.test_case("iter");
        RAH2_STD::array<int, 8> u_ = {};
        auto u = make_test_view_adapter<CS, Tag, Sized>(u_);
        RAH2_NS::ranges::generate_n(u.begin(), 8, [n = 1]() mutable { return n++; });
        assert(u_ == (RAH2_STD::array<int, 8>{1, 2, 3, 4, 5, 6, 7, 8}));

        testSuite.test_case("empty");
        RAH2_STD::array<int, 3> e_ = {};
        auto e = make_test_view_adapter<CS, Tag, Sized>(e_);
        RAH2_NS::ranges::generate_n(e.begin(), 0, [n = 1]() mutable { return n++; });
        assert(e_ == (RAH2_STD::array<int, 3>{0, 0, 0}));
    }

    template <bool = true>
    void test_perf(char const* range_type)
    {
        testSuite.test_case("perf");
        RAH2_STD::vector<int> out_;
        out_.resize(1000000 * RELEASE_MULTIPLIER);
        auto out = make_test_view_adapter<CS, Tag, Sized>(out_);

        {
            COMPARE_DURATION_TO_STD_RANGES(
                "generate_n_iter",
                range_type,
                [&]
                {
                    auto result = STD::generate_n(
                        out.begin(), 1000000 * RELEASE_MULTIPLIER, [n = 1]() mutable { return n++; });
                    CHECK(result == out.end());
                });
        }
    }
    static constexpr bool do_test = RAH2_NS::derived_from<Tag, RAH2_NS::forward_iterator_tag>;
};
void test_generate_n()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::generate_n]
    RAH2_STD::array<int, 8> v = {};
    RAH2_NS::ranges::generate_n(
        v.begin(), static_cast<intptr_t>(v.size()), [n{0}]() mutable { return n++; });
    assert(v == (RAH2_STD::array<int, 8>{0, 1, 2, 3, 4, 5, 6, 7}));
    /// [rah2::ranges::generate_n]

    foreach_range_combination<test_algo<test_generate_n_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_remove_
{
    template <bool = true>
    void test()
    {
        {
            testSuite.test_case("range");
            RAH2_STD::vector<int> in_{1, 2, 1, 3, 1};
            auto in = make_test_view_adapter<CS, Tag, Sized>(in_);
            auto const removed = RAH2_NS::ranges::remove(in, 1);
            assert(RAH2_STD::distance(in.begin(), removed.begin()) == 2);
            assert(RAH2_STD::distance(removed.begin(), removed.end()) == 3);

            testSuite.test_case("iter");
            RAH2_STD::vector<int> in2_{1, 2, 1, 3, 1};
            auto in2 = make_test_view_adapter<CS, Tag, Sized>(in2_);
            auto const removed2 = RAH2_NS::ranges::remove(in2.begin(), in2.end(), 1);
            assert(RAH2_STD::distance(in2.begin(), removed2.begin()) == 2);
            assert(RAH2_STD::distance(removed2.begin(), removed2.end()) == 3);

            testSuite.test_case("empty");
            RAH2_STD::vector<int> in3_;
            auto in3 = make_test_view_adapter<CS, Tag, Sized>(in3_);
            auto const removed3 = RAH2_NS::ranges::remove(in3.begin(), in3.end(), 1);
            assert(RAH2_STD::distance(in3.begin(), removed3.begin()) == 0);
            assert(RAH2_STD::distance(removed3.begin(), removed3.end()) == 0);
        }

        testSuite.test_case("proj");
        {
            RAH2_STD::vector<Coord> in_{{1, 0}, {2, 0}, {1, 0}, {3, 0}, {1, 0}};
            auto in = make_test_view_adapter<CS, Tag, Sized>(in_);
            auto const removed = RAH2_NS::ranges::remove(in, 1, &Coord::x);
            assert(RAH2_STD::distance(in.begin(), removed.begin()) == 2);
            assert(RAH2_STD::distance(removed.begin(), removed.end()) == 3);

            RAH2_STD::vector<Coord> in2_{{1, 0}, {2, 0}, {1, 0}, {3, 0}, {1, 0}};
            auto in2 = make_test_view_adapter<CS, Tag, Sized>(in2_);
            auto const removed2 = RAH2_NS::ranges::remove(in2.begin(), in2.end(), 1, &Coord::x);
            assert(RAH2_STD::distance(in2.begin(), removed2.begin()) == 2);
            assert(RAH2_STD::distance(removed2.begin(), removed2.end()) == 3);
        }
    }

    template <bool = true>
    void test_perf(char const* range_type)
    {
        {
            RAH2_STD::vector<int> in2_{1, 2, 1, 3, 1};
            in2_.insert(in2_.end(), 1000000 * RELEASE_MULTIPLIER, 18);
            COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(
                CS == Common,
                "remove",
                range_type,
                (
                    [&]
                    {
                        auto in2_copy = in2_;
                        auto in2 = make_test_view_adapter<CS, Tag, Sized>(in2_copy);
                        auto const removed = STD::remove(fwd(in2.begin()), in2.end(), 1);
                        DONT_OPTIM(removed);
                    }));
        }
        {
            RAH2_STD::vector<Coord> in_{{1, 0}, {2, 0}, {1, 0}, {3, 0}, {1, 0}};
            in_.insert(in_.end(), 1000000 * RELEASE_MULTIPLIER, Coord{18, 0});
            COMPARE_DURATION_TO_STD_RANGES(
                "remove_proj",
                range_type,
                (
                    [&]
                    {
                        auto in_copy = in_;
                        auto in = make_test_view_adapter<CS, Tag, Sized>(in_copy);
                        auto const removed = STD::remove(in, 1, &Coord::x);
                        assert(
                            RAH2_STD::distance(in.begin(), removed.begin())
                            == 2 + 1000000 * RELEASE_MULTIPLIER);
                        assert(RAH2_STD::distance(removed.begin(), removed.end()) == 3);
                    }));
        }
    }
    static constexpr bool do_test = RAH2_NS::derived_from<Tag, RAH2_NS::forward_iterator_tag>;
};
void test_remove()
{
    testSuite.test_case("sample");
    testSuite.test_case("return");
    /// [rah2::ranges::remove]
    RAH2_STD::vector<int> in{1, 2, 1, 3, 1};
    auto const to_erase = RAH2_NS::ranges::remove(in, 1);
    in.erase(to_erase.begin(), to_erase.end());
    assert(in == RAH2_STD::vector<int>({2, 3}));
    /// [rah2::ranges::remove]

    foreach_range_combination<test_algo<test_remove_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_remove_if_
{
    template <bool = true>
    void test()
    {
        {
            testSuite.test_case("range");
            RAH2_STD::vector<int> in_{1, 2, 1, 3, 1};
            auto in = make_test_view_adapter<CS, Tag, Sized>(in_);
            auto const removed = RAH2_NS::ranges::remove_if(in, [](auto v) { return v == 1; });
            assert(RAH2_STD::distance(in.begin(), removed.begin()) == 2);
            assert(RAH2_STD::distance(removed.begin(), removed.end()) == 3);

            testSuite.test_case("iter");
            RAH2_STD::vector<int> in2_{1, 2, 1, 3, 1};
            auto in2 = make_test_view_adapter<CS, Tag, Sized>(in2_);
            auto const removed2 =
                RAH2_NS::ranges::remove_if(in2.begin(), in2.end(), [](auto v) { return v == 1; });
            assert(RAH2_STD::distance(in2.begin(), removed2.begin()) == 2);
            assert(RAH2_STD::distance(removed2.begin(), removed2.end()) == 3);

            testSuite.test_case("empty");
            RAH2_STD::vector<int> in3_;
            auto in3 = make_test_view_adapter<CS, Tag, Sized>(in3_);
            auto const removed3 =
                RAH2_NS::ranges::remove_if(in3.begin(), in3.end(), [](auto v) { return v == 1; });
            assert(RAH2_STD::distance(in3.begin(), removed3.begin()) == 0);
            assert(RAH2_STD::distance(removed3.begin(), removed3.end()) == 0);
        }

        testSuite.test_case("proj");
        {
            RAH2_STD::vector<Coord> in_{{1, 0}, {2, 0}, {1, 0}, {3, 0}, {1, 0}};
            auto in = make_test_view_adapter<CS, Tag, Sized>(in_);
            auto const removed =
                RAH2_NS::ranges::remove_if(in, [](auto v) { return v == 1; }, &Coord::x);
            assert(RAH2_STD::distance(in.begin(), removed.begin()) == 2);
            assert(RAH2_STD::distance(removed.begin(), removed.end()) == 3);

            RAH2_STD::vector<Coord> in2_{{1, 0}, {2, 0}, {1, 0}, {3, 0}, {1, 0}};
            auto in2 = make_test_view_adapter<CS, Tag, Sized>(in2_);
            auto const removed2 = RAH2_NS::ranges::remove_if(
                in2.begin(), in2.end(), [](auto v) { return v == 1; }, &Coord::x);
            assert(RAH2_STD::distance(in2.begin(), removed2.begin()) == 2);
            assert(RAH2_STD::distance(removed2.begin(), removed2.end()) == 3);
        }
    }

    template <bool = true>
    void test_perf(char const* range_type)
    {
        {
            RAH2_STD::vector<int> in2_{1, 2, 1, 3, 1};
            in2_.insert(in2_.end(), 1000000 * RELEASE_MULTIPLIER, 18);
            COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(
                CS == Common,
                "remove_if",
                range_type,
                (
                    [&]
                    {
                        auto in2_copy = in2_;
                        auto in2 = make_test_view_adapter<CS, Tag, Sized>(in2_copy);
                        auto const removed = STD::remove_if(
                            fwd(in2.begin()), in2.end(), [](auto v) { return v == 1; });
                        DONT_OPTIM(removed);
                    }));
        }
        {
            RAH2_STD::vector<Coord> in_{{1, 0}, {2, 0}, {1, 0}, {3, 0}, {1, 0}};
            in_.insert(in_.end(), 1000000 * RELEASE_MULTIPLIER, Coord{18, 0});
            COMPARE_DURATION_TO_STD_RANGES(
                "remove_if_proj",
                range_type,
                (
                    [&]
                    {
                        auto in_copy = in_;
                        auto in = make_test_view_adapter<CS, Tag, Sized>(in_copy);
                        auto const removed =
                            STD::remove_if(in, [](auto v) { return v == 1; }, &Coord::x);
                        assert(
                            RAH2_STD::distance(in.begin(), removed.begin())
                            == 2 + 1000000 * RELEASE_MULTIPLIER);
                        assert(RAH2_STD::distance(removed.begin(), removed.end()) == 3);
                    }));
        }
    }
    static constexpr bool do_test = RAH2_NS::derived_from<Tag, RAH2_NS::forward_iterator_tag>;
};
void test_remove_if()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::remove_if]
    RAH2_STD::vector<int> in{1, 2, 3, 4, 5};
    auto const to_erase = RAH2_NS::ranges::remove_if(in, [](auto a) { return a < 4; });
    in.erase(to_erase.begin(), to_erase.end());
    RAH2_STD::sort(in.begin(), in.end());
    assert(in == RAH2_STD::vector<int>({4, 5}));
    /// [rah2::ranges::remove_if]

    foreach_range_combination<test_algo<test_remove_if_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_remove_copy_
{
    template <bool = true>
    void test()
    {
        {
            RAH2_STD::vector<int> const in_{1, 2, 1, 3, 1};
            auto in = make_test_view_adapter<CS, Tag, Sized>(in_);

            {
                testSuite.test_case("range");
                RAH2_STD::vector<int> out(3, 42);
                auto const removed = RAH2_NS::ranges::remove_copy(in, out.begin(), 1);
                CHECK(removed.in == in.end());
                CHECK(RAH2_NS::ranges::distance(out.begin(), removed.out) == 2);
                CHECK(out == (RAH2_STD::vector<int>{2, 3, 42}));
            }

            {
                testSuite.test_case("iter");
                RAH2_STD::vector<int> out2(3, 42);
                auto const removed2 =
                    RAH2_NS::ranges::remove_copy(in.begin(), in.end(), out2.begin(), 1);
                CHECK(removed2.in == in.end());
                CHECK(RAH2_NS::ranges::distance(out2.begin(), removed2.out) == 2);
                CHECK(out2 == (RAH2_STD::vector<int>{2, 3, 42}));
            }
            {
                testSuite.test_case("empty");
                auto empty = make_test_view_adapter<CS, Tag, Sized>(RAH2_STD::vector<int>());
                RAH2_STD::vector<int> out2(3, 42);
                auto const removed2 =
                    RAH2_NS::ranges::remove_copy(empty.begin(), empty.end(), out2.begin(), 1);
                CHECK(removed2.in == empty.end());
                CHECK(RAH2_NS::ranges::distance(out2.begin(), removed2.out) == 0);
                CHECK(out2 == (RAH2_STD::vector<int>{42, 42, 42}));
            }
            CHECK(in_ == (RAH2_STD::vector<int>{1, 2, 1, 3, 1}));
        }
        testSuite.test_case("proj");
        {
            RAH2_STD::vector<Coord> in_{{1, 0}, {2, 0}, {1, 0}, {3, 0}, {1, 0}};
            auto in = make_test_view_adapter<CS, Tag, Sized>(in_);

            {
                RAH2_STD::vector<Coord> out(3, Coord{42, 42});
                auto const removed = RAH2_NS::ranges::remove_copy(in, out.begin(), 1, &Coord::x);
                auto ok = in_ == (RAH2_STD::vector<Coord>{{1, 0}, {2, 0}, {1, 0}, {3, 0}, {1, 0}});
                CHECK(removed.in == in.end());
                CHECK(RAH2_NS::ranges::distance(out.begin(), removed.out) == 2);
                CHECK(ok);
                CHECK(out == (RAH2_STD::vector<Coord>{{2, 0}, {3, 0}, {42, 42}}));
            }

            {
                RAH2_STD::vector<Coord> out2(3, Coord{42, 42});
                auto const removed2 =
                    RAH2_NS::ranges::remove_copy(in.begin(), in.end(), out2.begin(), 1, &Coord::x);
                CHECK(removed2.in == in.end());
                CHECK(RAH2_NS::ranges::distance(out2.begin(), removed2.out) == 2);
                CHECK(in_ == (RAH2_STD::vector<Coord>{{1, 0}, {2, 0}, {1, 0}, {3, 0}, {1, 0}}));
                CHECK(out2 == (RAH2_STD::vector<Coord>{{2, 0}, {3, 0}, {42, 42}}));
            }
        }
    }

    template <bool = true>
    void test_perf(char const* range_type)
    {
        {
            RAH2_STD::vector<Coord> in_{{1, 0}, {2, 0}, {1, 0}, {3, 0}, {1, 0}};
            in_.insert(in_.end(), 1000000 * RELEASE_MULTIPLIER, {42, 0});
            auto in = make_test_view_adapter<CS, Tag, Sized>(in_);
            RAH2_STD::vector<Coord> out(1000000 * RELEASE_MULTIPLIER + 3, Coord{0, 0});

            COMPARE_DURATION_TO_STD_RANGES(
                "remove_copy_range",
                range_type,
                (
                    [&]
                    {
                        auto const removed = STD::remove_copy(in, out.begin(), 1, &Coord::x);
                        DONT_OPTIM(removed);
                        CHECK(out[1] == (Coord{3, 0}));
                        CHECK(out[2] == (Coord{42, 0}));
                    }));
        }
        {
            RAH2_STD::vector<int> in_{1, 2, 1, 3, 1};
            in_.insert(in_.end(), 1000000 * RELEASE_MULTIPLIER, 42);
            auto in = make_test_view_adapter<CS, Tag, Sized>(in_);
            RAH2_STD::vector<int> out2(1000000 * RELEASE_MULTIPLIER + 3, 0);

            COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(
                CS == Common,
                "remove_copy_iter",
                range_type,
                (
                    [&]
                    {
                        auto const removed2 =
                            STD::remove_copy(fwd(in.begin()), in.end(), out2.begin(), 1);
                        DONT_OPTIM(removed2);
                        CHECK(out2[1] == 3);
                        CHECK(out2[2] == 42);
                    }));
        }
    }
    static constexpr bool do_test = true;
};
void test_remove_copy()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::remove_copy]
    // Filter out the hash symbol from the given string.
    RAH2_STD::string const str{"#Small #Buffer #Optimization"};

    RAH2_STD::string out;
    RAH2_NS::ranges::remove_copy(str.begin(), str.end(), RAH2_NS::back_inserter(out), '#');
    assert(out == "Small Buffer Optimization");
    /// [rah2::ranges::remove_copy]

    foreach_range_combination<test_algo<test_remove_copy_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_remove_copy_if_
{
    template <bool = true>
    void test()
    {
        {
            RAH2_STD::vector<int> in_{1, 2, 1, 3, 1};
            auto in = make_test_view_adapter<CS, Tag, Sized>(in_);

            {
                testSuite.test_case("range");
                RAH2_STD::vector<int> out(3, 42);
                auto const removed =
                    RAH2_NS::ranges::remove_copy_if(in, out.begin(), [](auto v) { return v == 1; });
                CHECK(removed.in == in.end());
                CHECK(RAH2_NS::ranges::distance(out.begin(), removed.out) == 2);
                CHECK(in_ == (RAH2_STD::vector<int>{1, 2, 1, 3, 1}));
                CHECK(out == (RAH2_STD::vector<int>{2, 3, 42}));
            }

            {
                testSuite.test_case("iter");
                RAH2_STD::vector<int> out2(3, 42);
                auto const removed2 = RAH2_NS::ranges::remove_copy_if(
                    in.begin(), in.end(), out2.begin(), [](auto v) { return v == 1; });
                CHECK(removed2.in == in.end());
                CHECK(RAH2_NS::ranges::distance(out2.begin(), removed2.out) == 2);
                CHECK(in_ == (RAH2_STD::vector<int>{1, 2, 1, 3, 1}));
                CHECK(out2 == (RAH2_STD::vector<int>{2, 3, 42}));
            }
        }
        testSuite.test_case("proj");
        {
            RAH2_STD::vector<Coord> in_{{1, 0}, {2, 0}, {1, 0}, {3, 0}, {1, 0}};
            auto in = make_test_view_adapter<CS, Tag, Sized>(in_);

            {
                RAH2_STD::vector<Coord> out(3, Coord{42, 42});
                auto const removed = RAH2_NS::ranges::remove_copy_if(
                    in, out.begin(), [](auto v) { return v == 1; }, &Coord::x);
                auto ok = in_ == (RAH2_STD::vector<Coord>{{1, 0}, {2, 0}, {1, 0}, {3, 0}, {1, 0}});
                CHECK(removed.in == in.end());
                CHECK(RAH2_NS::ranges::distance(out.begin(), removed.out) == 2);
                CHECK(ok);
                CHECK(out == (RAH2_STD::vector<Coord>{{2, 0}, {3, 0}, {42, 42}}));
            }

            {
                RAH2_STD::vector<Coord> out2(3, Coord{42, 42});
                auto const removed2 = RAH2_NS::ranges::remove_copy_if(
                    in.begin(), in.end(), out2.begin(), [](auto v) { return v == 1; }, &Coord::x);
                CHECK(removed2.in == in.end());
                CHECK(RAH2_NS::ranges::distance(out2.begin(), removed2.out) == 2);
                CHECK(in_ == (RAH2_STD::vector<Coord>{{1, 0}, {2, 0}, {1, 0}, {3, 0}, {1, 0}}));
                CHECK(out2 == (RAH2_STD::vector<Coord>{{2, 0}, {3, 0}, {42, 42}}));
            }
        }
    }

    template <bool = true>
    void test_perf(char const* range_type)
    {
        {
            RAH2_STD::vector<Coord> in_{{1, 0}, {2, 0}, {1, 0}, {3, 0}, {1, 0}};
            in_.insert(in_.end(), 1000000 * RELEASE_MULTIPLIER, {42, 0});
            auto in = make_test_view_adapter<CS, Tag, Sized>(in_);
            RAH2_STD::vector<Coord> out(1000000 * RELEASE_MULTIPLIER + 3, Coord{0, 0});

            COMPARE_DURATION_TO_STD_RANGES(
                "remove_copy_if_range",
                range_type,
                (
                    [&]
                    {
                        auto const removed = STD::remove_copy_if(
                            in, out.begin(), [](auto v) { return v == 1; }, &Coord::x);
                        DONT_OPTIM(removed);
                        CHECK(out[1] == (Coord{3, 0}));
                        CHECK(out[2] == (Coord{42, 0}));
                    }));
        }
        {
            RAH2_STD::vector<int> in_{1, 2, 1, 3, 1};
            in_.insert(in_.end(), 1000000 * RELEASE_MULTIPLIER, 42);
            auto in = make_test_view_adapter<CS, Tag, Sized>(in_);
            RAH2_STD::vector<int> out2(1000000 * RELEASE_MULTIPLIER + 3, 0);

            COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(
                CS == Common,
                "remove_copy_if_iter",
                range_type,
                (
                    [&]
                    {
                        auto const removed2 = STD::remove_copy_if(
                            fwd(in.begin()), in.end(), out2.begin(), [](auto v) { return v == 1; });
                        DONT_OPTIM(removed2);
                        CHECK(out2[1] == 3);
                        CHECK(out2[2] == 42);
                    }));
        }
    }
    static constexpr bool do_test = true;
};
void test_remove_copy_if()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::remove_copy_if]
    // Copy only the complex numbers with positive imaginary part.
    using Ci = std::complex<double>;
    constexpr RAH2_STD::array<Ci, 5> source{
        Ci{1., 0.}, Ci{0., 1.}, Ci{2., -1.}, Ci{3., 2.}, Ci{4., -3.}};
    RAH2_STD::vector<std::complex<double>> target;

    RAH2_NS::ranges::remove_copy_if(
        source, RAH2_NS::back_inserter(target), [](Ci z) { return z.imag() <= 0; });
    assert(target == (RAH2_STD::vector<std::complex<double>>{{0., 1.}, {3., 2.}}));
    /// [rah2::ranges::remove_copy_if]

    foreach_range_combination<test_algo<test_remove_copy_if_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_replace_
{
    template <bool = true>
    void test()
    {
        {
            RAH2_STD::vector<int> out_{0, 4, 0, 4, 5};
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            testSuite.test_case("iter");
            auto result = RAH2_NS::ranges::replace(out.begin(), out.end(), 4, 15);
            CHECK(result == out.end());
            CHECK(out_ == (RAH2_STD::vector<int>{0, 15, 0, 15, 5}));

            testSuite.test_case("range");
            auto result2 = RAH2_NS::ranges::replace(out, 15, 72);
            CHECK(result2 == out.end());
            CHECK(out_ == (RAH2_STD::vector<int>{0, 72, 0, 72, 5}));
        }

        {
            testSuite.test_case("proj");
            {
                RAH2_STD::vector<Coord> out_{{0, 0}, {4, 0}, {0, 0}, {4, 0}, {5, 0}};
                auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
                testSuite.test_case("iter");
                auto result =
                    RAH2_NS::ranges::replace(out.begin(), out.end(), 4, Coord{15, 0}, &Coord::x);
                CHECK(result == out.end());
                CHECK(out_ == (RAH2_STD::vector<Coord>{{0, 0}, {15, 0}, {0, 0}, {15, 0}, {5, 0}}));
            }

            testSuite.test_case("range");
            {
                RAH2_STD::vector<Coord> out_{{0, 0}, {15, 0}, {0, 0}, {15, 0}, {5, 0}};
                auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
                auto result2 = RAH2_NS::ranges::replace(out, 15, Coord{72, 0}, &Coord::x);
                CHECK(result2 == out.end());
                CHECK(out_ == (RAH2_STD::vector<Coord>{{0, 0}, {72, 0}, {0, 0}, {72, 0}, {5, 0}}));
            }
        }

        testSuite.test_case("empty");
        {
            RAH2_STD::vector<int> empty_out_;
            auto empty_out = make_test_view_adapter<CS, Tag, Sized>(empty_out_);

            auto result3 = RAH2_NS::ranges::replace(empty_out.begin(), empty_out.end(), 169, 0);
            CHECK(result3 == empty_out.end());
            CHECK(empty_out_.empty());
        }
    }

    template <bool = true>
    void test_perf(char const* range_type)
    {
        testSuite.test_case("perf");
        RAH2_STD::vector<int> out_;
        out_.resize(100000 * RELEASE_MULTIPLIER);
        out_.emplace_back(10);
        out_.emplace_back(10);
        auto out = make_test_view_adapter<CS, Tag, Sized>(out_);

        RAH2_STD::vector<Coord> out2_;
        out2_.resize(100000 * RELEASE_MULTIPLIER);
        out2_.emplace_back(Coord{10, 11});
        out2_.emplace_back(Coord{10, 11});
        auto out2 = make_test_view_adapter<CS, Tag, Sized>(out2_);

        {
            COMPARE_DURATION_TO_STD_RANGES(
                "replace_iter",
                range_type,
                [&]
                {
                    auto result =
                        STD::replace(RAH2_NS::ranges::begin(out), RAH2_NS::ranges::end(out), 10, 11);
                    CHECK(result == out.end());
                });
        }

        {
            COMPARE_DURATION_TO_STD_RANGES(
                "replace_ranges",
                range_type,
                (
                    [&]
                    {
                        auto result2 = STD::replace(out2, 10, Coord{11, 11}, &Coord::x);
                        CHECK(result2 == out2.end());
                    }));
        }
    }
    static constexpr bool do_test = true;
};
void test_replace()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::replace]
    RAH2_STD::array<int, 6> p{1, 6, 1, 6, 1, 6};
    RAH2_NS::ranges::replace(p, 6, 9);
    assert(p == (RAH2_STD::array<int, 6>{1, 9, 1, 9, 1, 9}));
    /// [rah2::ranges::replace]

    foreach_range_combination<test_algo<test_replace_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_replace_if_
{
    template <bool = true>
    void test()
    {
        {
            RAH2_STD::vector<int> out_{0, 4, 0, 4, 5};
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            testSuite.test_case("iter");
            auto result = RAH2_NS::ranges::replace_if(
                out.begin(), out.end(), [](auto v) { return v == 4; }, 15);
            CHECK(result == out.end());
            CHECK(out_ == (RAH2_STD::vector<int>{0, 15, 0, 15, 5}));

            testSuite.test_case("range");
            auto result2 = RAH2_NS::ranges::replace_if(out, [](auto v) { return v == 15; }, 72);
            CHECK(result2 == out.end());
            CHECK(out_ == (RAH2_STD::vector<int>{0, 72, 0, 72, 5}));
        }

        {
            testSuite.test_case("proj");
            {
                RAH2_STD::vector<Coord> out_{{0, 0}, {4, 0}, {0, 0}, {4, 0}, {5, 0}};
                auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
                testSuite.test_case("iter");
                auto result = RAH2_NS::ranges::replace_if(
                    out.begin(), out.end(), [](auto v) { return v == 4; }, Coord{15, 0}, &Coord::x);
                CHECK(result == out.end());
                CHECK(out_ == (RAH2_STD::vector<Coord>{{0, 0}, {15, 0}, {0, 0}, {15, 0}, {5, 0}}));
            }

            testSuite.test_case("range");
            {
                RAH2_STD::vector<Coord> out_{{0, 0}, {15, 0}, {0, 0}, {15, 0}, {5, 0}};
                auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
                auto result2 = RAH2_NS::ranges::replace_if(
                    out, [](auto v) { return v == 15; }, Coord{72, 0}, &Coord::x);
                CHECK(result2 == out.end());
                CHECK(out_ == (RAH2_STD::vector<Coord>{{0, 0}, {72, 0}, {0, 0}, {72, 0}, {5, 0}}));
            }
        }

        testSuite.test_case("empty");
        {
            RAH2_STD::vector<int> empty_out_;
            auto empty_out = make_test_view_adapter<CS, Tag, Sized>(empty_out_);
            CHECK(empty_out.begin() == empty_out.end());
            CHECK(empty_out.empty());

            auto result3 = RAH2_NS::ranges::replace_if(
                empty_out.begin(), empty_out.end(), [](auto v) { return v == 169; }, 0);
            CHECK(result3 == empty_out.end());
            CHECK(empty_out.empty());
        }
    }

    template <bool = true>
    void test_perf(char const* range_type)
    {
        testSuite.test_case("perf");
        RAH2_STD::vector<int> out_;
        out_.resize(100000 * RELEASE_MULTIPLIER);
        out_.emplace_back(10);
        out_.emplace_back(10);
        auto out = make_test_view_adapter<CS, Tag, Sized>(out_);

        RAH2_STD::vector<Coord> out2_;
        out2_.resize(100000 * RELEASE_MULTIPLIER);
        out2_.emplace_back(Coord{10, 11});
        out2_.emplace_back(Coord{10, 11});
        auto out2 = make_test_view_adapter<CS, Tag, Sized>(out2_);

        {
            COMPARE_DURATION_TO_STD_RANGES(
                "replace_if_iter",
                range_type,
                [&]
                {
                    auto result = STD::replace_if(
                        RAH2_NS::ranges::begin(out),
                        RAH2_NS::ranges::end(out),
                        [](auto v) { return v == 10; },
                        11);
                    CHECK(result == out.end());
                });
        }

        {
            COMPARE_DURATION_TO_STD_RANGES(
                "replace_if_ranges",
                range_type,
                (
                    [&]
                    {
                        auto result2 = STD::replace_if(
                            out2, [](auto v) { return v == 10; }, Coord{11, 11}, &Coord::x);
                        CHECK(result2 == out2.end());
                    }));
        }
    }
    static constexpr bool do_test = true;
};
void test_replace_if()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::replace_if]
    RAH2_STD::array<int, 8> q{1, 2, 3, 6, 7, 8, 4, 5};
    RAH2_NS::ranges::replace_if(q, [](int x) { return 5 < x; }, 5);
    assert(q == (RAH2_STD::array<int, 8>{1, 2, 3, 5, 5, 5, 4, 5}));
    /// [rah2::ranges::replace_if]

    foreach_range_combination<test_algo<test_replace_if_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_replace_copy_
{
    template <bool = true>
    void test()
    {
        {
            testSuite.test_case("iter");
            RAH2_STD::vector<int> in_{0, 4, 0, 4, 5};
            auto in = make_test_view_adapter<CS, Tag, Sized>(in_);
            RAH2_STD::vector<int> out(5, 1);
            auto result = RAH2_NS::ranges::replace_copy(in.begin(), in.end(), out.begin(), 4, 15);
            CHECK(result.in == in.end());
            CHECK(result.out == out.end());
            CHECK(out == (RAH2_STD::vector<int>{0, 15, 0, 15, 5}));
        }

        {
            testSuite.test_case("range");
            RAH2_STD::vector<int> in_{0, 4, 0, 4, 5};
            auto in = make_test_view_adapter<CS, Tag, Sized>(in_);
            RAH2_STD::vector<int> out(5, 1);
            auto result = RAH2_NS::ranges::replace_copy(in, out.begin(), 4, 72);
            CHECK(result.in == in.end());
            CHECK(result.out == out.end());
            CHECK(out == (RAH2_STD::vector<int>{0, 72, 0, 72, 5}));
        }

        testSuite.test_case("proj");
        {
            RAH2_STD::vector<Coord> in_{{0, 0}, {4, 0}, {0, 0}, {4, 0}, {5, 0}};
            auto in = make_test_view_adapter<CS, Tag, Sized>(in_);
            RAH2_STD::vector<Coord> out(5, Coord{-1, -1});
            auto result = RAH2_NS::ranges::replace_copy(
                in.begin(), in.end(), out.begin(), 4, Coord{15, 0}, &Coord::x);
            CHECK(result.in == in.end());
            CHECK(result.out == out.end());
            CHECK(out == (RAH2_STD::vector<Coord>{{0, 0}, {15, 0}, {0, 0}, {15, 0}, {5, 0}}));
        }

        {
            RAH2_STD::vector<Coord> in_{{0, 0}, {15, 0}, {0, 0}, {15, 0}, {5, 0}};
            auto in = make_test_view_adapter<CS, Tag, Sized>(in_);
            RAH2_STD::vector<Coord> out(5, Coord{-1, -1});
            auto result = RAH2_NS::ranges::replace_copy(in, out.begin(), 15, Coord{72, 0}, &Coord::x);
            CHECK(result.in == in.end());
            CHECK(result.out == out.end());
            CHECK(out == (RAH2_STD::vector<Coord>{{0, 0}, {72, 0}, {0, 0}, {72, 0}, {5, 0}}));
        }

        testSuite.test_case("empty");
        {
            RAH2_STD::vector<int> in_;
            auto in = make_test_view_adapter<CS, Tag, Sized>(in_);
            RAH2_STD::vector<int> out;
            auto result = RAH2_NS::ranges::replace_copy(in.begin(), in.end(), out.begin(), 169, 0);
            CHECK(result.in == in.end());
            CHECK(result.out == out.end());
        }
    }

    template <bool = true>
    void test_perf(char const* range_type)
    {
        testSuite.test_case("perf");
        RAH2_STD::vector<int> in_;
        in_.resize(100000 * RELEASE_MULTIPLIER);
        in_.emplace_back(10);
        in_.emplace_back(10);
        auto in = make_test_view_adapter<CS, Tag, Sized>(in_);
        RAH2_STD::vector<int> out(100000 * RELEASE_MULTIPLIER + 2);

        RAH2_STD::vector<Coord> in2_;
        in2_.resize(100000 * RELEASE_MULTIPLIER);
        in2_.emplace_back(Coord{10, 11});
        in2_.emplace_back(Coord{10, 11});
        auto in2 = make_test_view_adapter<CS, Tag, Sized>(in2_);
        RAH2_STD::vector<Coord> out2(100000 * RELEASE_MULTIPLIER + 2);

        {
            COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(
                CS == Common,
                "replace_copy_iter",
                range_type,
                [&]
                {
                    auto result = STD::replace_copy(
                        RAH2_NS::ranges::begin(fwd(in)), RAH2_NS::ranges::end(in), out.begin(), 10, 11);
                    DONT_OPTIM(result);
                });
        }

        {
            COMPARE_DURATION_TO_STD_RANGES(
                "replace_copy_ranges",
                range_type,
                (
                    [&]
                    {
                        auto result =
                            STD::replace_copy(in2, out2.begin(), 10, Coord{11, 11}, &Coord::x);
                        DONT_OPTIM(result);
                    }));
        }
    }
    static constexpr bool do_test = true;
};
void test_replace_copy()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::replace_copy]
    RAH2_STD::vector<int> o;
    RAH2_STD::array<int, 6> p{1, 6, 1, 6, 1, 6};
    o.resize(p.size());
    RAH2_NS::ranges::replace_copy(p, o.begin(), 6, 9);
    assert(o == (RAH2_STD::vector<int>{1, 9, 1, 9, 1, 9}));
    /// [rah2::ranges::replace_copy]

    foreach_range_combination<test_algo<test_replace_copy_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_replace_copy_if_
{
    template <bool = true>
    void test()
    {
        {
            testSuite.test_case("iter");
            RAH2_STD::vector<int> in_{0, 4, 0, 4, 5};
            auto in = make_test_view_adapter<CS, Tag, Sized>(in_);
            RAH2_STD::vector<int> out(5, 1);
            auto result = RAH2_NS::ranges::replace_copy_if(
                in.begin(), in.end(), out.begin(), [](auto v) { return v == 4; }, 15);
            CHECK(result.in == in.end());
            CHECK(result.out == out.end());
            CHECK(out == (RAH2_STD::vector<int>{0, 15, 0, 15, 5}));
        }

        {
            testSuite.test_case("range");
            RAH2_STD::vector<int> in_{0, 4, 0, 4, 5};
            auto in = make_test_view_adapter<CS, Tag, Sized>(in_);
            RAH2_STD::vector<int> out(5, 1);
            auto result =
                RAH2_NS::ranges::replace_copy_if(in, out.begin(), [](auto v) { return v == 4; }, 72);
            CHECK(result.in == in.end());
            CHECK(result.out == out.end());
            CHECK(out == (RAH2_STD::vector<int>{0, 72, 0, 72, 5}));
        }

        testSuite.test_case("proj");
        {
            RAH2_STD::vector<Coord> in_{{0, 0}, {4, 0}, {0, 0}, {4, 0}, {5, 0}};
            auto in = make_test_view_adapter<CS, Tag, Sized>(in_);
            RAH2_STD::vector<Coord> out(5, Coord{-1, -1});
            auto result = RAH2_NS::ranges::replace_copy_if(
                in.begin(),
                in.end(),
                out.begin(),
                [](auto v) { return v == 4; },
                Coord{15, 0},
                &Coord::x);
            CHECK(result.in == in.end());
            CHECK(result.out == out.end());
            CHECK(out == (RAH2_STD::vector<Coord>{{0, 0}, {15, 0}, {0, 0}, {15, 0}, {5, 0}}));
        }

        {
            RAH2_STD::vector<Coord> in_{{0, 0}, {15, 0}, {0, 0}, {15, 0}, {5, 0}};
            auto in = make_test_view_adapter<CS, Tag, Sized>(in_);
            RAH2_STD::vector<Coord> out(5, Coord{-1, -1});
            auto result = RAH2_NS::ranges::replace_copy_if(
                in, out.begin(), [](auto v) { return v == 15; }, Coord{72, 0}, &Coord::x);
            CHECK(result.in == in.end());
            CHECK(result.out == out.end());
            CHECK(out == (RAH2_STD::vector<Coord>{{0, 0}, {72, 0}, {0, 0}, {72, 0}, {5, 0}}));
        }

        testSuite.test_case("empty");
        {
            RAH2_STD::vector<int> in_;
            auto in = make_test_view_adapter<CS, Tag, Sized>(in_);
            RAH2_STD::vector<int> out;
            auto result = RAH2_NS::ranges::replace_copy_if(
                in.begin(), in.end(), out.begin(), [](auto v) { return v == 169; }, 0);
            CHECK(result.in == in.end());
            CHECK(result.out == out.end());
        }
    }

    template <bool = true>
    void test_perf(char const* range_type)
    {
        testSuite.test_case("perf");
        RAH2_STD::vector<int> in_;
        in_.resize(100000 * RELEASE_MULTIPLIER);
        in_.emplace_back(10);
        in_.emplace_back(10);
        auto in = make_test_view_adapter<CS, Tag, Sized>(in_);
        RAH2_STD::vector<int> out(100000 * RELEASE_MULTIPLIER + 2);

        RAH2_STD::vector<Coord> in2_;
        in2_.resize(100000 * RELEASE_MULTIPLIER);
        in2_.emplace_back(Coord{10, 11});
        in2_.emplace_back(Coord{10, 11});
        auto in2 = make_test_view_adapter<CS, Tag, Sized>(in2_);
        RAH2_STD::vector<Coord> out2(100000 * RELEASE_MULTIPLIER + 2);

        {
            COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(
                CS == Common,
                "replace_copy_if_iter",
                range_type,
                [&]
                {
                    auto result = STD::replace_copy_if(
                        RAH2_NS::ranges::begin(fwd(in)),
                        RAH2_NS::ranges::end(in),
                        out.begin(),
                        [](auto v) { return v == 10; },
                        11);
                    DONT_OPTIM(result);
                });
        }

        {
            COMPARE_DURATION_TO_STD_RANGES(
                "replace_copy_if_ranges",
                range_type,
                (
                    [&]
                    {
                        auto result = STD::replace_copy_if(
                            in2, out2.begin(), [](auto v) { return v == 10; }, Coord{11, 11}, &Coord::x);
                        DONT_OPTIM(result);
                    }));
        }
    }
    static constexpr bool do_test = true;
};
void test_replace_copy_if()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::replace_copy_if]
    RAH2_STD::vector<int> o;
    RAH2_STD::array<int, 8> q{1, 2, 3, 6, 7, 8, 4, 5};
    o.resize(q.size());
    RAH2_NS::ranges::replace_copy_if(q, o.begin(), [](int x) { return 5 < x; }, 5);
    assert(o == (RAH2_STD::vector<int>{1, 2, 3, 5, 5, 5, 4, 5}));
    /// [rah2::ranges::replace_copy_if]

    foreach_range_combination<test_algo<test_replace_copy_if_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_swap_
{
    template <bool = true>
    void test()
    {
        {
            testSuite.test_case("iter");
            RAH2_STD::vector<int> in_{1, 2, 3};
            auto in = make_test_view_adapter<CS, Tag, Sized>(in_);
            RAH2_STD::vector<int> out{10, 11, 12, 4, 5};
            auto result = RAH2_NS::ranges::swap_ranges(
                RAH2_NS::ranges::begin(in), RAH2_NS::ranges::end(in), out.begin(), out.end());
            CHECK(result.in2 == out.begin() + in_.size());
            CHECK(result.in1 == in.end());
            CHECK(out == (RAH2_STD::vector<int>{1, 2, 3, 4, 5}));
            CHECK(in_ == (RAH2_STD::vector<int>{10, 11, 12}));
        }

        {
            testSuite.test_case("range");
            RAH2_STD::vector<int> in_{1, 2, 3};
            auto in = make_test_view_adapter<CS, Tag, Sized>(in_);
            RAH2_STD::vector<int> out{10, 11, 12, 4, 5};
            auto result2 = RAH2_NS::ranges::swap_ranges(in, out);
            CHECK(result2.in2 == out.begin() + in_.size());
            CHECK(result2.in1 == in.end());
            CHECK(out == (RAH2_STD::vector<int>{1, 2, 3, 4, 5}));
            CHECK(in_ == (RAH2_STD::vector<int>{10, 11, 12}));
        }

        {
            testSuite.test_case("empty");
            RAH2_STD::vector<int> empty_in_;
            auto empty_in = make_test_view_adapter<CS, Tag, Sized>(empty_in_);
            RAH2_STD::vector<int> empty_out;

            auto result3 = RAH2_NS::ranges::swap_ranges(empty_in, empty_out);
            CHECK(result3.in2 == empty_out.begin() + empty_in_.size());
            CHECK(result3.in1 == empty_in.end());
            CHECK(empty_out.empty());
            CHECK(empty_in_.empty());
        }
    }

    template <bool = true>
    void test_perf(char const* range_type)
    {
        testSuite.test_case("perf");
        RAH2_STD::vector<int> in_;
        for (size_t i = 0; i < 1000000 * RELEASE_MULTIPLIER; ++i)
        {
            in_.push_back(i % 15);
        }
        auto in = make_test_view_adapter<CS, Tag, Sized>(in_);
        RAH2_STD::vector<int> out;
        out.resize(1000000 * RELEASE_MULTIPLIER);
        out.emplace_back();
        out.emplace_back();

        {
            COMPARE_DURATION_TO_STD_RANGES(
                "swap_ranges_iter",
                range_type,
                [&]
                {
                    auto result = STD::swap_ranges(
                        RAH2_NS::ranges::begin(in), RAH2_NS::ranges::end(in), out.begin(), out.end());
                    CHECK(result.in2 == out.begin() + in_.size());
                    CHECK(result.in1 == in.end());
                });
        }

        {
            COMPARE_DURATION_TO_STD_RANGES(
                "swap_ranges_ranges",
                range_type,
                (
                    [&]
                    {
                        auto result2 = STD::swap_ranges(in, out);
                        CHECK(result2.in2 == out.begin() + in_.size());
                        CHECK(result2.in1 == in.end());
                    }));
        }
    }
    static constexpr bool do_test = true;
};
void test_swap_ranges()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::swap_ranges]
    RAH2_STD::vector<char> p{'A', 'B', 'C', 'D', 'E'};
    RAH2_STD::list<char> q{'1', '2', '3', '4', '5', '6'};

    // swap p[0, 2) and q[1, 3):
    RAH2_NS::ranges::swap_ranges(
        p.begin(),
        p.begin() + 4,
        RAH2_NS::ranges::next(q.begin(), 1),
        RAH2_NS::ranges::next(q.begin(), 3));
    assert(p == (RAH2_STD::vector<char>{'2', '3', 'C', 'D', 'E'}));
    assert(q == (RAH2_STD::list<char>{'1', 'A', 'B', '4', '5', '6'}));

    // swap p[0, 5) and q[0, 5):
    RAH2_NS::ranges::swap_ranges(p, q);
    assert(q == (RAH2_STD::list<char>{'2', '3', 'C', 'D', 'E', '6'}));
    assert(p == (RAH2_STD::vector<char>{'1', 'A', 'B', '4', '5'}));
    /// [rah2::ranges::swap_ranges]

    foreach_range_combination<test_algo<test_swap_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_reverse_
{
    template <bool = true>
    void test()
    {
        {
            testSuite.test_case("iter");
            RAH2_STD::vector<int> out_{1, 2, 3, 4, 5};
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto result = RAH2_NS::ranges::reverse(out.begin(), out.end());
            CHECK(result == out.end());
            CHECK(out_ == (RAH2_STD::vector<int>{5, 4, 3, 2, 1}));
        }

        {
            testSuite.test_case("range");
            RAH2_STD::vector<int> out_{1, 2, 3, 4, 5, 6};
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto result2 = RAH2_NS::ranges::reverse(out);
            CHECK(result2 == out.end());
            CHECK(out_ == (RAH2_STD::vector<int>{6, 5, 4, 3, 2, 1}));
        }

        {
            testSuite.test_case("empty");
            RAH2_STD::vector<int> out_{};
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto result3 = RAH2_NS::ranges::reverse(out.begin(), out.end());
            CHECK(result3 == out.end());
            CHECK(out_.empty());
        }
    }

    template <bool = true>
    void test_perf(char const* range_type)
    {
        testSuite.test_case("perf");
        RAH2_STD::vector<int> out_;
        out_.resize(1000000 * RELEASE_MULTIPLIER);
        out_.emplace_back();
        out_.emplace_back();
        auto out = make_test_view_adapter<CS, Tag, Sized>(out_);

        {
            COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(
                CS == CommonOrSent::Common,
                "reverse_iter",
                range_type,
                [&]
                {
                    STD::reverse(RAH2_NS::ranges::begin(fwd(out)), RAH2_NS::ranges::end(out));
                    CHECK(out.front() == 0);
                });
        }

        {
            COMPARE_DURATION_TO_STD_RANGES(
                "reverse_ranges",
                range_type,
                (
                    [&]
                    {
                        auto result2 = STD::reverse(out);
                        DONT_OPTIM(result2);
                    }));
        }
    }
    static constexpr bool do_test = RAH2_NS::derived_from<Tag, RAH2_NS::bidirectional_iterator_tag>;
};
void test_reverse()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::reverse]
    RAH2_STD::string s{"ABCDEF"};
    RAH2_NS::ranges::reverse(s.begin(), s.end());
    assert(s == RAH2_STD::string{"FEDCBA"});
    RAH2_NS::ranges::reverse(s);
    assert(s == RAH2_STD::string{"ABCDEF"});

    RAH2_STD::array<int, 5> a{1, 2, 3, 4, 5};
    RAH2_NS::ranges::reverse(a);
    assert(a == (RAH2_STD::array<int, 5>{5, 4, 3, 2, 1}));
    /// [rah2::ranges::reverse]

    foreach_range_combination<test_algo<test_reverse_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_reverse_copy_
{
    template <bool = true>
    void test()
    {
        RAH2_STD::vector<int> in_{1, 2, 3};
        auto in = make_test_view_adapter<CS, Tag, Sized>(in_);
        {
            RAH2_STD::vector<int> out{3, 2, 1, 4, 5};
            testSuite.test_case("iter");
            auto result = RAH2_NS::ranges::reverse_copy(
                RAH2_NS::ranges::begin(in), RAH2_NS::ranges::end(in), out.begin());
            CHECK(result.out == out.begin() + in_.size());
            CHECK(result.in == in.end());
            CHECK(out == (RAH2_STD::vector<int>{3, 2, 1, 4, 5}));
        }

        {
            RAH2_STD::vector<int> out{0, 0, 0, 4, 5};
            testSuite.test_case("range");
            auto result2 = RAH2_NS::ranges::reverse_copy(in, out.begin());
            CHECK(result2.out == out.begin() + in_.size());
            CHECK(result2.in == in.end());
            CHECK(out == (RAH2_STD::vector<int>{3, 2, 1, 4, 5}));
        }

        testSuite.test_case("empty");
        {
            RAH2_STD::vector<int> empty_in_;
            auto empty_in = make_test_view_adapter<CS, Tag, Sized>(empty_in_);
            RAH2_STD::vector<int> empty_out;

            auto result3 = RAH2_NS::ranges::reverse_copy(empty_in, empty_out.begin());
            CHECK(result3.out == empty_out.begin() + empty_in_.size());
            CHECK(result3.in == empty_in.end());
            CHECK(empty_out.empty());
        }
    }

    template <bool = true>
    void test_perf(char const* range_type)
    {
        testSuite.test_case("perf");
        RAH2_STD::vector<int> in_;
        for (size_t i = 0; i < 1000000 * RELEASE_MULTIPLIER; ++i)
        {
            in_.push_back(i % 15);
        }
        auto in = make_test_view_adapter<CS, Tag, Sized>(in_);
        RAH2_STD::vector<int> out;
        out.resize(1000000 * RELEASE_MULTIPLIER);
        out.emplace_back();
        out.emplace_back();
        {
            COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(
                CS == CommonOrSent::Common,
                "reverse_copy_iter",
                range_type,
                [&]
                {
                    auto result = STD::reverse_copy(
                        RAH2_NS::ranges::begin(fwd(in)), RAH2_NS::ranges::end(in), out.begin());
                    DONT_OPTIM(result);
                });
        }

        {
            COMPARE_DURATION_TO_STD_RANGES(
                "reverse_copy_ranges",
                range_type,
                (
                    [&]
                    {
                        auto result2 = STD::reverse_copy(in, out.begin());
                        CHECK(result2.out == out.begin() + in_.size());
                        CHECK(result2.in == in.end());
                    }));
        }
    }
    static constexpr bool do_test = RAH2_NS::derived_from<Tag, RAH2_NS::bidirectional_iterator_tag>;
};
void test_reverse_copy()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::reverse_copy]
    RAH2_STD::string x{"12345"};
    RAH2_STD::string y(x.size(), ' ');
    RAH2_NS::ranges::reverse_copy(x.begin(), x.end(), y.begin());
    assert(x == (RAH2_STD::string{"12345"}));
    assert(y == (RAH2_STD::string{"54321"}));
    RAH2_NS::ranges::reverse_copy(y, x.begin());
    assert(x == (RAH2_STD::string{"12345"}));
    /// [rah2::ranges::reverse_copy]

    foreach_range_combination<test_algo<test_reverse_copy_>>();
}
