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

    template <typename I, std::enable_if_t<RAH2_NS::input_iterator<RAH2_NS::remove_cvref_t<I>>>* = nullptr>
    static auto get_begin(I&& i) -> decltype(std::forward<I>(i))
    {
        return std::forward<I>(i);
    }

    template <typename I, std::enable_if_t<!RAH2_NS::input_iterator<RAH2_NS::remove_cvref_t<I>>>* = nullptr>
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
                    auto result1 =
                        RAH2_NS::ranges::search_n(fwd(nums.begin()), nums.end(), count, value);
                    CHECK((RAH2_NS::ranges::distance(result1.begin(), result1.end())) == (count));
                    CHECK_EQUAL(
                        RAH2_STD::distance(nums.begin(), result1.begin()),
                        1000000 * RELEASE_MULTIPLIER + 6);
                    CHECK_EQUAL(
                        RAH2_STD::distance(nums.begin(), result1.end()),
                        1000000 * RELEASE_MULTIPLIER + 9);
                });
        }
        {
            COMPARE_DURATION_TO_STD_RANGES(
                "search_n_proj",
                range_type,
                (
                    [&]
                    {
                        auto result2 = RAH2_NS::ranges::search_n(nums, count, value);
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
            COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(
                CS == Common,
                "contains",
                range_type,
                [&]
                {
                    auto result1 = RAH2_NS::ranges::contains(fwd(nums.begin()), nums.end(), value);
                    CHECK(result1);
                });
        }
        {
            COMPARE_DURATION_TO_STD_RANGES(
                "contains_proj",
                range_type,
                (
                    [&]
                    {
                        auto result2 = RAH2_NS::ranges::contains(nums, value);
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
                        auto haystack_proj = make_test_view_adapter<CS, Tag, Sized>(haystack_proj_);
                        RAH2_STD::string bodkin_{"2345678"};
                        auto bodkin = make_test_view_adapter<CS, Tag, Sized>(bodkin_);
                        auto const found5 = STD::starts_with(
                            haystack_proj,
                            bodkin,
                            [](int const x, int const y) { return x == y; },
                            [](int const x) { return std::toupper(x); },
                            [](int const y) { return (y - '1') + 'A'; });
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
            COMPARE_DURATION_TO_STD_RANGES(
                "copy_iter",
                range_type,
                [&]
                {
                    auto result = RAH2_NS::ranges::copy(
                        RAH2_NS::ranges::begin(in), RAH2_NS::ranges::end(in), out.begin());
                    CHECK(result.out == out.begin() + in_.size());
                    CHECK(result.in == in.end());
                });
        }

        {
            COMPARE_DURATION_TO_STD_RANGES(
                "copy_ranges",
                range_type,
                (
                    [&]
                    {
                        auto result2 = RAH2_NS::ranges::copy(in, out.begin());
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
                    auto result = RAH2_NS::ranges::copy_if(
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
                        auto result2 = RAH2_NS::ranges::copy_if(
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
            COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(
                true,
                "copy_n",
                range_type,
                [&]
                {
                    auto const res = RAH2_NS::ranges::copy_n(
                        fwd(in.begin()), 1000000 * RELEASE_MULTIPLIER - 5, out.begin());
                    DONT_OPTIM(res);
                });
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
        CHECK_EQUAL(res2.out, out.end());
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
                "copy_backward",
                range_type,
                (
                    [&]
                    {
                        auto const res = RAH2_NS::ranges::copy_backward(in, out.end());
                        DONT_OPTIM(res);
                    }));
        }
        {
            COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(
                true,
                "copy_backward",
                range_type,
                [&]
                {
                    auto const res =
                        RAH2_NS::ranges::copy_backward(fwd(in.begin()), fwd(in.end()), out.end());
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
            COMPARE_DURATION_TO_STD_RANGES(
                "copy_iter",
                range_type,
                [&]
                {
                    auto result = RAH2_NS::ranges::move(
                        RAH2_NS::ranges::begin(in2), RAH2_NS::ranges::end(in2), out.begin());
                    CHECK(result.out == out.begin() + in_.size());
                    CHECK(result.in == in2.end());
                });
        }

        {
            COMPARE_DURATION_TO_STD_RANGES(
                "copy_ranges",
                range_type,
                (
                    [&]
                    {
                        auto result2 = RAH2_NS::ranges::move(in, out.begin());
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
        CHECK_EQUAL(res2.out, out.end());
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
                "copy_backward",
                range_type,
                (
                    [&]
                    {
                        auto const res = RAH2_NS::ranges::move_backward(in, out.end());
                        DONT_OPTIM(res);
                    }));
        }
        {
            COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(
                true,
                "copy_backward",
                range_type,
                [&]
                {
                    auto const res =
                        RAH2_NS::ranges::move_backward(fwd(in.begin()), fwd(in.end()), out.end());
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
    assert(a == (Vec{"", "", "", "▄", "▅", "▁", "▂", "▃"}));
    /// [rah2::ranges::move_backward]

    foreach_range_combination<test_algo<test_move_backward_>>();
}
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
}
void test_fill_n()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::fill_n]
    RAH2_STD::vector<int> out(5);
    RAH2_NS::ranges::fill_n(out.begin(), 4, 42);
    assert(out == (RAH2_STD::vector<int>{42, 42, 42, 42, 0}));
    /// [rah2::ranges::fill_n]
}
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
}
void test_generate()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::generate]
    RAH2_STD::array<int, 8> v = {};
    RAH2_NS::ranges::generate(v, [n = 1]() mutable { return n++; });
    assert(v == (RAH2_STD::array<int, 8>{1, 2, 3, 4, 5, 6, 7, 8}));
    /// [rah2::ranges::generate]
}
void test_generate_n()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::generate_n]
    RAH2_STD::array<int, 8> v = {};
    RAH2_NS::ranges::generate_n(
        v.begin(), static_cast<intptr_t>(v.size()), [n{0}]() mutable { return n++; });
    assert(v == (RAH2_STD::array<int, 8>{0, 1, 2, 3, 4, 5, 6, 7}));
    /// [rah2::ranges::generate_n]
}
void test_remove()
{
    testSuite.test_case("sample");
    testSuite.test_case("return");
    /// [rah2::ranges::remove]
    RAH2_STD::vector<int> in{1, 2, 1, 3, 1};
    auto const to_erase = RAH2_NS::ranges::remove(in, 1);
    in.erase(to_erase.begin(), to_erase.end());
    RAH2_STD::sort(in.begin(), in.end());
    assert(in == RAH2_STD::vector<int>({2, 3}));
    /// [rah2::ranges::remove]
}
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
}
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
}
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
}
void test_replace()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::replace]
    RAH2_STD::array<int, 6> p{1, 6, 1, 6, 1, 6};
    RAH2_NS::ranges::replace(p, 6, 9);
    assert(p == (RAH2_STD::array<int, 6>{1, 9, 1, 9, 1, 9}));
    /// [rah2::ranges::replace]
}
void test_replace_if()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::replace_if]
    RAH2_STD::array<int, 8> q{1, 2, 3, 6, 7, 8, 4, 5};
    RAH2_NS::ranges::replace_if(q, [](int x) { return 5 < x; }, 5);
    assert(q == (RAH2_STD::array<int, 8>{1, 2, 3, 5, 5, 5, 4, 5}));
    /// [rah2::ranges::replace_if]
}
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
}
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
}
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
}
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
}
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
}
void test_rotate()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::rotate]
    RAH2_STD::string s(16, ' ');

    RAH2_STD::iota(s.begin(), s.end(), 'A');
    RAH2_NS::ranges::rotate(s, s.begin());
    assert(s == (RAH2_STD::string{"ABCDEFGHIJKLMNOP"}));
    RAH2_NS::ranges::rotate(s, s.begin() + 1);
    assert(s == (RAH2_STD::string{"BCDEFGHIJKLMNOPA"}));
    RAH2_STD::iota(s.begin(), s.end(), 'A');
    RAH2_NS::ranges::rotate(s, s.begin() + 3);
    assert(s == (RAH2_STD::string{"DEFGHIJKLMNOPABC"}));

    RAH2_STD::iota(s.begin(), s.end(), 'A');
    RAH2_NS::ranges::rotate(s, s.end());
    assert(s == (RAH2_STD::string{"ABCDEFGHIJKLMNOP"}));
    RAH2_NS::ranges::rotate(s, s.end() - 1);
    assert(s == (RAH2_STD::string{"PABCDEFGHIJKLMNO"}));
    RAH2_STD::iota(s.begin(), s.end(), 'A');
    RAH2_NS::ranges::rotate(s, s.end() - 3);
    assert(s == (RAH2_STD::string{"NOPABCDEFGHIJKLM"}));
    /// [rah2::ranges::rotate]
}
void test_rotate_copy()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::rotate_copy]
    RAH2_STD::vector<int> src{1, 2, 3, 4, 5};
    RAH2_STD::vector<int> dest(src.size());
    auto pivot = RAH2_NS::ranges::find(src, 3);

    RAH2_NS::ranges::rotate_copy(src, pivot, dest.begin());
    assert(dest == (RAH2_STD::vector<int>{3, 4, 5, 1, 2}));

    pivot = RAH2_NS::ranges::find(src, 3);
    RAH2_NS::ranges::rotate_copy(src.begin(), pivot, src.end(), dest.begin());
    assert(dest == (RAH2_STD::vector<int>{3, 4, 5, 1, 2}));
    /// [rah2::ranges::rotate_copy]
}
void test_shuffle()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::shuffle]
    std::random_device rd;
    std::mt19937 g(rd());
    RAH2_STD::vector<int> in{1, 2, 3, 4, 5, 6};
    RAH2_NS::ranges::shuffle(in, g);
    /// [rah2::ranges::shuffle]
}
void test_shift_left()
{
    testSuite.test_case("sample");
    testSuite.test_case("return");
    /// [rah2::ranges::shift_left]
    RAH2_STD::vector<int> b{1, 2, 3, 4, 5, 6, 7};

    auto b8 = RAH2_NS::ranges::shift_left(b, 8); // has no effect: n >= last - first
    assert(RAH2_NS::ranges::equal(b8, RAH2_STD::vector<int>{1, 2, 3, 4, 5, 6, 7}));
    assert(b == (RAH2_STD::vector<int>{1, 2, 3, 4, 5, 6, 7}));

    auto b0 = RAH2_NS::ranges::shift_left(b, 0); // has no effect: n == 0
    assert(RAH2_NS::ranges::equal(b0, RAH2_STD::vector<int>{1, 2, 3, 4, 5, 6, 7}));
    assert(b == (RAH2_STD::vector<int>{1, 2, 3, 4, 5, 6, 7}));

    RAH2_STD::vector<int> ref{4, 5, 6, 7};
    auto b3 = RAH2_NS::ranges::shift_left(b, 3);
    assert(RAH2_NS::ranges::equal(b3, ref));
    assert(RAH2_NS::ranges::equal(b.begin(), b.begin() + 4, ref.begin(), ref.end()));
    /// [rah2::ranges::shift_left]
}
void test_shift_right()
{
    // TODO : Test perf with all iterator/range type. Take care of random_access+sized_range
    testSuite.test_case("sample");
    testSuite.test_case("return");
    /// [rah2::ranges::shift_right]
    RAH2_STD::vector<int> b{1, 2, 3, 4, 5, 6, 7};

    auto b8 = RAH2_NS::ranges::shift_right(b, 8); // has no effect: n >= last - first
    assert(RAH2_NS::ranges::equal(b8, RAH2_STD::vector<int>{1, 2, 3, 4, 5, 6, 7}));
    assert(b == (RAH2_STD::vector<int>{1, 2, 3, 4, 5, 6, 7}));

    auto b0 = RAH2_NS::ranges::shift_right(b, 0); // has no effect: n == 0
    assert(RAH2_NS::ranges::equal(b0, RAH2_STD::vector<int>{1, 2, 3, 4, 5, 6, 7}));
    assert(b == (RAH2_STD::vector<int>{1, 2, 3, 4, 5, 6, 7}));

    RAH2_STD::vector<int> ref{1, 2, 3, 4};
    auto b3 = RAH2_NS::ranges::shift_right(b, 3);
    assert(RAH2_NS::ranges::equal(b3, ref));
    assert(RAH2_NS::ranges::equal(b.begin() + 3, b.end(), ref.begin(), ref.end()));
    /// [rah2::ranges::shift_right]
}
void test_sample()
{
    testSuite.test_case("sample");
    testSuite.test_case("return");
    /// [rah2::ranges::sample]
    auto const in = {1, 2, 3, 4, 5, 6};

    RAH2_STD::vector<int> out(in.size() + 2);
    auto const max = static_cast<intptr_t>(in.size() + 2);
    auto gen = std::mt19937{std::random_device{}()};

    for (intptr_t n{}; n != max; ++n)
    {
        auto o = RAH2_NS::ranges::sample(in, out.begin(), n, gen);
        assert((o - out.begin()) == RAH2_STD::min(n, static_cast<intptr_t>(in.size())));
    }

    auto const o = RAH2_NS::ranges::sample(in, out.begin(), static_cast<intptr_t>(in.size()), gen);
    assert(RAH2_NS::ranges::equal(in.begin(), in.end(), out.begin(), o));

    /// [rah2::ranges::sample]
}
void test_unique()
{
    testSuite.test_case("sample");
    {
        /// [rah2::ranges::unique]
        RAH2_STD::vector<int> in{2, 1, 1, 1, 5, 3, 3, 4};
        in.erase(RAH2_NS::ranges::unique(in).begin(), end(in));
        assert(in == RAH2_STD::vector<int>({2, 1, 5, 3, 4}));
        /// [rah2::ranges::unique]
    }
    {
        /// [rah2::ranges::unique_pred]
        RAH2_STD::vector<int> in{2, 1, 1, 1, 5, 3, 3, 4};
        in.erase(RAH2_NS::ranges::unique(in, [](auto a, auto b) { return a == b; }).begin(), end(in));
        assert(in == RAH2_STD::vector<int>({2, 1, 5, 3, 4}));
        /// [rah2::ranges::unique_pred]
    }
}
void test_unique_copy()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::unique_copy]
    RAH2_STD::string s1{"The      string    with many       spaces!"};

    RAH2_STD::string s2;
    RAH2_NS::ranges::unique_copy(
        s1.begin(),
        s1.end(),
        RAH2_NS::back_inserter(s2),
        [](char c1, char c2) { return c1 == ' ' && c2 == ' '; });
    assert(s2 == (RAH2_STD::string{"The string with many spaces!"}));
    /// [rah2::ranges::unique_copy]
}
void test_is_partitioned()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::is_partitioned]
    RAH2_STD::array<int, 9> v = {1, 2, 3, 4, 5, 6, 7, 8, 9};

    auto is_even = [](int i)
    {
        return i % 2 == 0;
    };

    assert(RAH2_NS::ranges::is_partitioned(v, is_even) == false);

    RAH2_NS::ranges::partition(v, is_even);
    assert(RAH2_NS::ranges::is_partitioned(v, is_even));

    RAH2_NS::ranges::reverse(v);
    assert(RAH2_NS::ranges::is_partitioned(v.cbegin(), v.cend(), is_even) == false);
    assert(RAH2_NS::ranges::is_partitioned(v.crbegin(), v.crend(), is_even));
    /// [rah2::ranges::is_partitioned]
}
void test_partition()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::partition]
    RAH2_STD::vector<int> in{1, 2, 3, 4, 5};
    auto const boundary = RAH2_NS::ranges::partition(in, [](auto a) { return a >= 4; });
    assert(boundary.begin() == in.begin() + 2);
    RAH2_STD::sort(in.begin(), boundary.begin());
    RAH2_STD::sort(boundary.begin(), in.end());
    assert(in == RAH2_STD::vector<int>({4, 5, 1, 2, 3}));
    /// [rah2::ranges::partition]
}
void test_partition_copy()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::partition_copy]
    RAH2_STD::vector<char> const in = {'N', '3', 'U', 'M', '1', 'B', '4', 'E', '1', '5', 'R', '9'};

    RAH2_STD::vector<int> o1(in.size());
    RAH2_STD::vector<int> o2(in.size());

    auto pred = [](char c)
    {
        return std::isalpha(c);
    };

    auto const ret = RAH2_NS::ranges::partition_copy(in, o1.begin(), o2.begin(), pred);

    assert(in == (RAH2_STD::vector<char>{'N', '3', 'U', 'M', '1', 'B', '4', 'E', '1', '5', 'R', '9'}));
    RAH2_STD::vector<int> o1_expected{'N', 'U', 'M', 'B', 'E', 'R'};
    RAH2_STD::vector<int> o2_expected{'3', '1', '4', '1', '5', '9'};
    assert(RAH2_NS::ranges::equal(o1.begin(), ret.out1, o1_expected.begin(), o1_expected.end()));
    assert(RAH2_NS::ranges::equal(o2.begin(), ret.out2, o2_expected.begin(), o2_expected.end()));
    /// [rah2::ranges::partition_copy]
}
void test_stable_partition()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::stable_partition]
    RAH2_STD::vector<int> in{1, 2, 3, 4, 5};
    auto const boundary = RAH2_NS::ranges::stable_partition(in, [](auto a) { return a >= 4; });
    assert(boundary.begin() == in.begin() + 2);
    assert(in == RAH2_STD::vector<int>({4, 5, 1, 2, 3}));
    /// [rah2::ranges::stable_partition]
}
void test_partition_point()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::partition_point]
    RAH2_STD::array<int, 9> v{1, 2, 3, 4, 5, 6, 7, 8, 9};

    auto is_even = [](int i)
    {
        return i % 2 == 0;
    };

    RAH2_NS::ranges::partition(v, is_even);

    auto const pp = RAH2_NS::ranges::partition_point(v, is_even);
    auto const i = RAH2_NS::ranges::distance(v.cbegin(), pp);
    assert(i == 4); // 4 even number in v
    /// [rah2::ranges::partition_point]
}
void test_is_sorted()
{
    testSuite.test_case("sample");
    testSuite.test_case("return");
    /// [rah2::ranges::is_sorted]

    RAH2_STD::array<int, 5> digits{3, 1, 4, 1, 5};
    assert(not RAH2_NS::ranges::is_sorted(digits));

    RAH2_NS::ranges::sort(digits);
    assert(RAH2_NS::ranges::is_sorted(digits));

    RAH2_NS::ranges::reverse(digits);
    assert(not RAH2_NS::ranges::is_sorted(digits));
    assert(RAH2_NS::ranges::is_sorted(digits, RAH2_NS::ranges::greater{}));
    /// [rah2::ranges::is_sorted]
}
void test_is_sorted_until()
{
    testSuite.test_case("sample");
    testSuite.test_case("return");
    testSuite.test_case("empty");
    /// [rah2::ranges::is_sorted_until]
    RAH2_STD::array<int, 0> a0 = {};
    auto const sorted_end0 = RAH2_NS::ranges::is_sorted_until(a0);
    assert(RAH2_NS::ranges::distance(a0.begin(), sorted_end0) == 0);

    RAH2_STD::array<int, 6> a1{3, 1, 4, 1, 5, 9};
    auto const sorted_end = RAH2_NS::ranges::is_sorted_until(a1);
    assert(RAH2_NS::ranges::distance(a1.begin(), sorted_end) == 1);

    RAH2_STD::array<int, 6> a3{3, 6, 18, 1, 5, 9};
    auto const sorted_end3 = RAH2_NS::ranges::is_sorted_until(a3);
    assert(RAH2_NS::ranges::distance(a3.begin(), sorted_end3) == 3);

    RAH2_STD::array<int, 6> a6{3, 6, 18, 19, 20, 78};
    auto const sorted_end6 = RAH2_NS::ranges::is_sorted_until(a6);
    assert(RAH2_NS::ranges::distance(a6.begin(), sorted_end6) == 6);
    /// [rah2::ranges::is_sorted_until]
}
void test_sort()
{
    testSuite.test_case("sample");
    {
        /// [rah2::ranges::sort]
        RAH2_STD::vector<int> in{2, 1, 5, 3, 4};
        RAH2_NS::ranges::sort(in);
        assert(in == RAH2_STD::vector<int>({1, 2, 3, 4, 5}));
        /// [rah2::ranges::sort]
    }
    {
        /// [rah2::ranges::sort_pred]
        RAH2_STD::vector<int> in{2, 1, 5, 3, 4};
        RAH2_NS::ranges::sort(in, [](auto a, auto b) { return a < b; });
        assert(in == RAH2_STD::vector<int>({1, 2, 3, 4, 5}));
        /// [rah2::ranges::sort_pred]
    }
}
void test_partial_sort()
{
    testSuite.test_case("sample");
    testSuite.test_case("return");
    /// [rah2::ranges::partial_sort]
    RAH2_STD::vector<char> v{'x', 'P', 'y', 'C', 'z', 'w', 'P', 'o'};

    int const m{3};
    RAH2_NS::ranges::partial_sort(v, v.begin() + m);
    assert((RAH2_NS::ranges::equal(v | RAH2_NS::ranges::views::take(3), RAH2_STD::string("CPP"))));

    RAH2_STD::string s{"3a1b41c5"};
    RAH2_NS::ranges::partial_sort(s.begin(), s.begin() + m, s.end(), RAH2_NS::ranges::greater{});
    assert((RAH2_NS::ranges::equal(s | RAH2_NS::ranges::views::take(3), RAH2_STD::string("cba"))));
    /// [rah2::ranges::partial_sort]
}
void test_partial_sort_copy()
{
    testSuite.test_case("sample");
    testSuite.test_case("return");
    /// [rah2::ranges::partial_sort_copy]
    std::forward_list<int> const source{4, 2, 5, 1, 3};

    RAH2_STD::vector<int> dest1{10, 11, 12};
    auto lastI_lastO = RAH2_NS::ranges::partial_sort_copy(source, dest1);
    assert(dest1 == (RAH2_STD::vector<int>{1, 2, 3}));
    assert(lastI_lastO.in == source.end());
    assert(lastI_lastO.out == dest1.end());

    RAH2_STD::vector<int> dest2{10, 11, 12, 13, 14, 15, 16};
    lastI_lastO = RAH2_NS::ranges::partial_sort_copy(source, dest2, RAH2_NS::ranges::greater{});
    assert(dest2 == (RAH2_STD::vector<int>{5, 4, 3, 2, 1, 15, 16}));
    assert(lastI_lastO.in == source.end());
    assert(lastI_lastO.out == dest2.begin() + 5);
    /// [rah2::ranges::partial_sort_copy]
}
void test_stable_sort()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::stable_sort]
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
        RAH2_STD::vector<CmpA> in{{4, 1}, {2, 1}, {4, 2}, {1, 1}, {4, 3}, {2, 2}, {4, 4}};
        RAH2_NS::ranges::stable_sort(in);
        assert(in == RAH2_STD::vector<CmpA>({{1, 1}, {2, 1}, {2, 2}, {4, 1}, {4, 2}, {4, 3}, {4, 4}}));
    }
    /// [rah2::ranges::stable_sort]
    {
        /// [rah2::ranges::stable_sort_pred]
        RAH2_STD::vector<RAH2_STD::pair<int, int>> in{
            {4, 1}, {2, 1}, {4, 2}, {1, 1}, {4, 3}, {2, 2}, {4, 4}};
        RAH2_NS::ranges::stable_sort(in, [](auto l, auto r) { return l.second < r.second; });
        assert(
            in
            == (RAH2_STD::vector<RAH2_STD::pair<int, int>>(
                {{4, 1}, {2, 1}, {1, 1}, {4, 2}, {2, 2}, {4, 3}, {4, 4}})));
        /// [rah2::ranges::stable_sort_pred]
    }
}
void test_nth_element()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::nth_element]
    RAH2_STD::array<int, 9> v{5, 6, 4, 3, 2, 6, 7, 9, 3};

    auto out_last = RAH2_NS::ranges::nth_element(v, v.begin() + 4);
    assert(v[4] == 5);
    assert(out_last == v.end());

    out_last = RAH2_NS::ranges::nth_element(v, v.begin() + 1, RAH2_NS::ranges::greater());
    assert(v[1] == 7);
    assert(out_last == v.end());

    using namespace RAH2_STD::literals;
    RAH2_STD::array<RAH2_STD::string, 7> names{
        "Diva",
        "Cornelius",
        "Munro",
        "Rhod"
        "Zorg",
        "Korben",
        "Bender",
        "Leeloo"};
    auto const out_last2 = RAH2_NS::ranges::nth_element(names, names.begin() + 4);
    assert(names[4] == "Leeloo");
    assert(out_last2 == names.end());
    /// [rah2::ranges::nth_element]
}
void test_lower_bound()
{
    testSuite.test_case("sample");
    testSuite.test_case("return");
    /// [rah2::ranges::lower_bound]
    RAH2_STD::vector<int> data{1, 2, 2, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 5};
    auto const lower = RAH2_NS::ranges::lower_bound(data, 4);
    assert(lower == data.begin() + 6);
    /// [rah2::ranges::lower_bound]
}
void test_upper_bound()
{
    testSuite.test_case("sample");
    testSuite.test_case("return");
    /// [rah2::ranges::upper_bound]
    RAH2_STD::vector<int> data{1, 2, 2, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 5};
    auto const upper = RAH2_NS::ranges::upper_bound(data, 4);
    assert(upper == data.begin() + 10);
    /// [rah2::ranges::upper_bound]
}
void test_binary_search()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::binary_search]
    RAH2_STD::vector<int> vecIn1{1, 2, 2, 3, 4};
    assert(not RAH2_NS::ranges::binary_search(vecIn1, 0));
    assert(RAH2_NS::ranges::binary_search(vecIn1, 1));
    assert(RAH2_NS::ranges::binary_search(vecIn1, 2));
    /// [rah2::ranges::binary_search]
}
void test_equal_range()
{
    testSuite.test_case("sample");
    {
        /// [rah2::ranges::equal_range]
        RAH2_STD::vector<int> vecIn1{1, 2, 2, 3, 4};
        {
            RAH2_STD::vector<int> out;
            for (int const i : RAH2_NS::ranges::equal_range(vecIn1, 0))
                out.push_back(i);
            assert(out == RAH2_STD::vector<int>({}));
        }
        {
            RAH2_STD::vector<int> out;
            for (int const i : RAH2_NS::ranges::equal_range(vecIn1, 1))
                out.push_back(i);
            assert(out == RAH2_STD::vector<int>({1}));
        }
        {
            RAH2_STD::vector<int> out;
            for (int const i : RAH2_NS::ranges::equal_range(vecIn1, 2))
                out.push_back(i);
            assert(out == RAH2_STD::vector<int>({2, 2}));
        }
        /// [rah2::ranges::equal_range]
    }
    {
        /// [rah2::ranges::equal_range_pred_0]
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
        /// [rah2::ranges::equal_range_pred_0]
        {
            /// [rah2::ranges::equal_range_pred]
            RAH2_STD::vector<S> vecIn1{{1, 'a'}, {2, 'b'}, {2, 'c'}, {3, 'd'}, {4, 'd'}};
            {
                RAH2_STD::vector<S> out;
                for (S const i : RAH2_NS::ranges::equal_range(vecIn1, 0, FindS{}))
                    out.push_back(i);
                assert(out == RAH2_STD::vector<S>({}));
            }
            {
                RAH2_STD::vector<S> out;
                for (S const i : RAH2_NS::ranges::equal_range(vecIn1, 1, FindS{}))
                    out.push_back(i);
                assert(out == RAH2_STD::vector<S>({{1, 'a'}}));
            }
            {
                RAH2_STD::vector<S> out;
                for (S const i : RAH2_NS::ranges::equal_range(vecIn1, 2, FindS{}))
                    out.push_back(i);
                assert(out == RAH2_STD::vector<S>({{2, 'b'}, {2, 'c'}}));
            }
            /// [rah2::ranges::equal_range_pred]
        }
    }
}
void test_merge()
{
    testSuite.test_case("sample");
    testSuite.test_case("return");
    /// [rah2::ranges::merge]
    RAH2_STD::vector<int> in1 = {1, 2, 3, 4, 5};
    RAH2_STD::vector<int> in2 = {3, 4, 5, 6, 7};
    RAH2_STD::vector<int> out(in1.size() + in2.size());

    auto const ret = RAH2_NS::ranges::merge(in1, in2, out.begin());
    assert((RAH2_NS::ranges::equal(
        RAH2_NS::ranges::make_subrange(out.begin(), ret.out),
        RAH2_STD::vector<int>{1, 2, 3, 3, 4, 4, 5, 5, 6, 7})));

    in1 = {1, 2, 3, 4, 5, 5, 5};
    in2 = {3, 4, 5, 6, 7};
    out.clear();
    RAH2_NS::ranges::merge(in1, in2, RAH2_NS::back_inserter(out));
    assert(out == (RAH2_STD::vector<int>{1, 2, 3, 3, 4, 4, 5, 5, 5, 5, 6, 7}));
    /// [rah2::ranges::merge]
}
void test_inplace_merge()
{
    testSuite.test_case("sample");
    testSuite.test_case("return");
    /// [rah2::ranges::inplace_merge]
    RAH2_STD::vector<int> v{1, 4, 8, 9, 10, 45, 2, 3, 4, 9, 11};
    auto const last = RAH2_NS::ranges::inplace_merge(v, v.begin() + 6);
    assert(last == v.end());
    assert(RAH2_NS::ranges::is_sorted(v));
    /// [rah2::ranges::inplace_merge]
}
void test_includes()
{
    testSuite.test_case("sample");
    testSuite.test_case("return");
    testSuite.test_case("pred");
    /// [rah2::ranges::includes]
    auto ignore_case = [](char a, char b)
    {
        return std::tolower(a) < std::tolower(b);
    };

    auto const a = {'a', 'b', 'c'}, b = {'a', 'c'}, c = {'a', 'a', 'b'}, d = {'g'},
               e = {'a', 'c', 'g'}, f = {'A', 'B', 'C'}, g = {'e'},
               z = {'a', 'b', 'c', 'f', 'h', 'x'};

    assert(RAH2_NS::ranges::includes(z.begin(), z.end(), a.begin(), a.end()));
    assert(RAH2_NS::ranges::includes(z, b));
    assert(not RAH2_NS::ranges::includes(z, g));
    assert(not RAH2_NS::ranges::includes(z, c));
    assert(not RAH2_NS::ranges::includes(z, d));
    assert(not RAH2_NS::ranges::includes(z, e));
    assert(RAH2_NS::ranges::includes(z, f, ignore_case));
    /// [rah2::ranges::includes]
}
void test_set_difference()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::set_difference]
    RAH2_STD::vector<int> in1{1, 3, 4};
    RAH2_STD::vector<int> in2{1, 2, 3};
    RAH2_STD::vector<int> out{0, 0, 0, 0};
    RAH2_NS::ranges::set_difference(in1, in2, out.begin());
    assert(out == RAH2_STD::vector<int>({4, 0, 0, 0}));
    /// [rah2::ranges::set_difference]
}
void test_set_intersection()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::set_intersection]
    RAH2_STD::vector<int> in1{1, 3, 4};
    RAH2_STD::vector<int> in2{1, 2, 3};
    RAH2_STD::vector<int> out{0, 0, 0, 0};
    RAH2_NS::ranges::set_intersection(in1, in2, out.begin());
    assert(out == RAH2_STD::vector<int>({1, 3, 0, 0}));
    /// [rah2::ranges::set_intersection]
}
void test_set_symmetric_difference()
{
    testSuite.test_case("sample");
    testSuite.test_case("return");
    /// [rah2::ranges::set_symmetric_difference]
    auto const in1 = {1, 3, 4, 6, 7, 9};
    auto const in2 = {1, 4, 5, 6, 9};

    RAH2_STD::vector<int> out(5);

    auto const res = RAH2_NS::ranges::set_symmetric_difference(in1, in2, out.begin());
    assert(out == (RAH2_STD::vector<int>{3, 5, 7, 0, 0}));
    assert(res.in1 == in1.end());
    assert(res.in2 == in2.end());
    assert(res.out == out.begin() + 3);
    /// [rah2::ranges::set_symmetric_difference]
}
void test_set_union()
{
    testSuite.test_case("sample");
    testSuite.test_case("return");
    /// [rah2::ranges::set_union]
    RAH2_STD::vector<int> in1 = {1, 2, 3, 4, 5};
    RAH2_STD::vector<int> in2 = {3, 4, 5, 6, 7};
    RAH2_STD::vector<int> out(in1.size() + in2.size());
    auto const ret = RAH2_NS::ranges::set_union(in1, in2, out.begin());
    assert(out == (RAH2_STD::vector<int>{1, 2, 3, 4, 5, 6, 7, 0, 0, 0}));
    assert(ret.in1 == in1.end());
    assert(ret.in2 == in2.end());
    assert(ret.out == out.begin() + 7);

    in1 = {1, 2, 3, 4, 5, 5, 5};
    in2 = {3, 4, 5, 6, 7};
    out.clear();
    out.reserve(in1.size() + in2.size());
    auto const ret2 = RAH2_NS::ranges::set_union(in1, in2, RAH2_NS::back_inserter(out));
    assert(out == (RAH2_STD::vector<int>{1, 2, 3, 4, 5, 5, 5, 6, 7}));
    assert(ret2.in1 == in1.end());
    assert(ret2.in2 == in2.end());
    /// [rah2::ranges::set_union]
}
void test_is_heap()
{
    testSuite.test_case("sample");
    testSuite.test_case("return");
    /// [rah2::ranges::is_heap]
    RAH2_STD::vector<int> v{3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5, 8, 9, 7, 9, 3, 2, 3, 8};
    assert(!RAH2_NS::ranges::is_heap(v));
    RAH2_STD::make_heap(v.begin(), v.end());
    assert(RAH2_NS::ranges::is_heap(v));
    /// [rah2::ranges::is_heap]
}
void test_is_heap_until()
{
    testSuite.test_case("sample");
    testSuite.test_case("return");
    /// [rah2::ranges::is_heap_until]
    RAH2_STD::vector<int> v{3, 1, 4, 1, 5, 9};
    RAH2_STD::make_heap(v.begin(), v.end());
    assert(RAH2_NS::ranges::is_heap_until(v) == v.end());

    // mess up the heap
    v.push_back(10);
    v.push_back(20);

    auto const heap_end = RAH2_NS::ranges::is_heap_until(v);
    assert(v.begin() + 6 == heap_end);
    /// [rah2::ranges::is_heap_until]
}
void test_make_heap()
{
    testSuite.test_case("sample");
    testSuite.test_case("return");
    /// [rah2::ranges::make_heap]
    RAH2_STD::vector<int> h{1, 6, 1, 8, 0, 3, 3, 9, 8, 8, 7, 4, 9, 8, 9};
    assert(!RAH2_STD::is_heap(h.begin(), h.end()));
    auto const last = RAH2_NS::ranges::make_heap(h);
    assert(last == h.end());
    assert(RAH2_STD::is_heap(h.begin(), h.end()));

    assert(!RAH2_STD::is_heap(h.begin(), h.end(), RAH2_NS::ranges::greater{}));
    auto const last2 = RAH2_NS::ranges::make_heap(h, RAH2_NS::ranges::greater{});
    assert(last2 == h.end());
    assert(RAH2_STD::is_heap(h.begin(), h.end(), RAH2_NS::ranges::greater{}));
    /// [rah2::ranges::make_heap]
}
void test_push_heap()
{
    testSuite.test_case("sample");
    testSuite.test_case("return");
    /// [rah2::ranges::push_heap]

    RAH2_STD::vector<int> v{1, 6, 1, 8, 0, 3};
    RAH2_NS::ranges::make_heap(v);

    v.push_back(9);
    auto const last = RAH2_NS::ranges::push_heap(v);
    assert(last == v.end());

    assert(RAH2_STD::is_heap(v.begin(), v.end()));
    assert(RAH2_STD::count(v.begin(), v.end(), 9) != 0);
    /// [rah2::ranges::push_heap]
}
void test_pop_heap()
{
    testSuite.test_case("sample");
    testSuite.test_case("return");
    /// [rah2::ranges::pop_heap]
    RAH2_STD::vector<int> v{3, 1, 4, 1, 5, 9, 2, 6, 5, 3};

    RAH2_STD::make_heap(v.begin(), v.end());
    auto const last = RAH2_NS::ranges::pop_heap(v);
    assert(last == v.end());
    assert(v.back() == 9);
    v.pop_back();
    assert(RAH2_STD::is_heap(v.begin(), v.end()));

    RAH2_NS::ranges::pop_heap(v);
    assert(v.back() == 6);
    v.pop_back();
    assert(RAH2_STD::is_heap(v.begin(), v.end()));
    /// [rah2::ranges::pop_heap]
}
void test_sort_heap()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::sort_heap]
    RAH2_STD::array<int, 6> v{3, 1, 4, 1, 5, 9};
    RAH2_STD::make_heap(v.begin(), v.end());
    RAH2_NS::ranges::sort_heap(v);
    assert(RAH2_STD::is_sorted(v.begin(), v.end()));
    /// [rah2::ranges::sort_heap]
}
void test_max()
{
    testSuite.test_case("sample");
    testSuite.test_case("return");
    /// [rah2::ranges::max]
    assert(RAH2_NS::ranges::max(1, 3) == 3);
    assert(RAH2_NS::ranges::max(1, 3, RAH2_NS::ranges::greater{}) == 1);

    assert(RAH2_NS::ranges::max({1, 7, 5}) == 7);
    assert(RAH2_NS::ranges::max({1, 7, 5}, RAH2_NS::ranges::greater{}) == 1);

    RAH2_STD::vector<int> v{1, 7, 5};
    assert(RAH2_NS::ranges::max(v) == 7);
    assert(RAH2_NS::ranges::max(v, RAH2_NS::ranges::greater{}) == 1);
    /// [rah2::ranges::max]
}
void test_max_element()
{
    testSuite.test_case("sample");
    {
        /// [rah2::ranges::max_element]
        RAH2_STD::vector<int> in{1, 5, 3, 4};
        auto const iter = RAH2_NS::ranges::max_element(in);
        assert(*iter == 5);
        /// [rah2::ranges::max_element]
    }
    {
        /// [rah2::ranges::max_element_pred]
        RAH2_STD::vector<RAH2_STD::pair<int, int>> in{{100, 3}, {0, 5}, {0, 1}, {0, 4}};
        auto const iter =
            RAH2_NS::ranges::max_element(in, [](auto&& a, auto& b) { return a.second < b.second; });
        assert(*iter == (RAH2_STD::pair<int, int>{0, 5}));
        /// [rah2::ranges::max_element_pred]
    }
}
void test_min()
{
    testSuite.test_case("sample");
    testSuite.test_case("return");
    /// [rah2::ranges::min]
    assert(RAH2_NS::ranges::min(1, 3) == 1);
    assert(RAH2_NS::ranges::min(1, 3, RAH2_NS::ranges::greater{}) == 3);

    assert(RAH2_NS::ranges::min({1, 7, 5}) == 1);
    assert(RAH2_NS::ranges::min({1, 7, 5}, RAH2_NS::ranges::greater{}) == 7);

    RAH2_STD::vector<int> v{1, 7, 5};
    assert(RAH2_NS::ranges::min(v) == 1);
    assert(RAH2_NS::ranges::min(v, RAH2_NS::ranges::greater{}) == 7);
    /// [rah2::ranges::min]
}
void test_min_element()
{
    testSuite.test_case("sample");
    testSuite.test_case("range");
    testSuite.test_case("nopred");
    {
        /// [rah2::ranges::min_element]
        RAH2_STD::vector<int> in{1, -5, 3, 4};
        auto iter = RAH2_NS::ranges::min_element(in);
        assert(*iter == -5);
        /// [rah2::ranges::min_element]

        testSuite.test_case("iter");
        iter = RAH2_NS::ranges::min_element(in.begin(), in.end());
        assert(*iter == -5);
    }

    {
        testSuite.test_case("pred");
        /// [rah2::ranges::min_element_pred]
        RAH2_STD::vector<RAH2_STD::pair<int, int>> in{{-100, 3}, {0, -5}, {0, 1}, {0, 4}};
        auto const iter =
            RAH2_NS::ranges::min_element(in, [](auto&& a, auto& b) { return a.second < b.second; });
        assert(*iter == (RAH2_STD::pair<int, int>{0, -5}));
        /// [rah2::ranges::min_element_pred]
    }
}
void test_minmax()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::minmax]

    constexpr int a = 1;
    constexpr int b = 3;
    auto const res1 = RAH2_NS::ranges::minmax(a, b);
    assert(res1.min == 1);
    assert(res1.max == 3);
    auto const res2 = RAH2_NS::ranges::minmax(a, b, RAH2_NS::ranges::greater{});
    assert(res2.min == 3);
    assert(res2.max == 1);

    auto const res3 = RAH2_NS::ranges::minmax({1, 7, 5});
    assert(res3.min == 1);
    assert(res3.max == 7);
    auto const res4 = RAH2_NS::ranges::minmax({1, 7, 5}, RAH2_NS::ranges::greater{});
    assert(res4.min == 7);
    assert(res4.max == 1);

    RAH2_STD::vector<int> v{1, 7, 5};
    auto const res5 = RAH2_NS::ranges::minmax(v);
    assert(res5.min == 1);
    assert(res5.max == 7);
    auto const res6 = RAH2_NS::ranges::minmax(v, RAH2_NS::ranges::greater{});
    assert(res6.min == 7);
    assert(res6.max == 1);
    /// [rah2::ranges::minmax]
}
void test_minmax_element()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::minmax_element]
    RAH2_STD::vector<int> v{1, 7, 5};
    auto const res5 = RAH2_NS::ranges::minmax_element(v);
    assert(*res5.min == 1);
    assert(*res5.max == 7);
    auto const res6 = RAH2_NS::ranges::minmax_element(v, RAH2_NS::ranges::greater{});
    assert(*res6.min == 7);
    assert(*res6.max == 1);
    /// [rah2::ranges::minmax_element]
}
void test_clamp()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::clamp]
    assert(RAH2_NS::ranges::clamp(0, 4, 8) == 4);
    assert(RAH2_NS::ranges::clamp(4, 4, 8) == 4);
    assert(RAH2_NS::ranges::clamp(6, 4, 8) == 6);
    assert(RAH2_NS::ranges::clamp(8, 4, 8) == 8);
    assert(RAH2_NS::ranges::clamp(10, 4, 8) == 8);
    /// [rah2::ranges::clamp]

    testSuite.test_case("comp");
    assert(RAH2_NS::ranges::clamp(0, 8, 4, RAH2_STD::greater<>()) == 4);
    assert(RAH2_NS::ranges::clamp(4, 8, 4, RAH2_STD::greater<>()) == 4);
    assert(RAH2_NS::ranges::clamp(6, 8, 4, RAH2_STD::greater<>()) == 6);
    assert(RAH2_NS::ranges::clamp(8, 8, 4, RAH2_STD::greater<>()) == 8);
    assert(RAH2_NS::ranges::clamp(10, 8, 4, RAH2_STD::greater<>()) == 8);
}
void test_is_permutation()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::is_permutation]
    static constexpr auto r1 = {1, 2, 3, 4, 5};
    static constexpr auto r2 = {3, 5, 4, 1, 2};
    assert(RAH2_NS::ranges::is_permutation(r1, r1));
    assert(RAH2_NS::ranges::is_permutation(r1, r2));
    assert(RAH2_NS::ranges::is_permutation(r2, r1));
    assert(RAH2_NS::ranges::is_permutation(r1.begin(), r1.end(), r2.begin(), r2.end()));
    /// [rah2::ranges::is_permutation]
}

size_t factorial(size_t n)
{
    size_t f = 1;
    for (size_t i = 1; i <= n; ++i)
        f *= i;
    return f;
}

void test_next_permutation()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::next_permutation]

    // Generate all permutations (iterators case)
    RAH2_STD::string s{"abc"};
    RAH2_STD::set<RAH2_STD::string> allPermutation;
    do
    {
        if (not allPermutation.empty())
            assert(s > *(--allPermutation.end()));
        allPermutation.emplace(s);
    } while (RAH2_NS::ranges::next_permutation(s.begin(), s.end()).found);
    assert(allPermutation.size() == factorial(s.size()));

    RAH2_STD::set<RAH2_STD::array<int, 3>> allPermutation2;

    // Generate all permutations (range case)
    RAH2_STD::array<int, 3> a{'a', 'b', 'c'};
    do
    {
        assert(allPermutation2.count(a) == 0);
        if (not allPermutation2.empty())
            assert(a > *(--allPermutation2.end()));
        allPermutation2.emplace(a);
    } while (RAH2_NS::ranges::next_permutation(a).found);
    assert(allPermutation2.size() == factorial(s.size()));

    RAH2_STD::set<RAH2_STD::array<RAH2_STD::string, 3>> allPermutation3;

    // Generate all permutations using comparator
    RAH2_STD::array<RAH2_STD::string, 3> z{"C", "B", "A"};
    do
    {
        if (not allPermutation3.empty())
            assert(z < *(allPermutation3.begin()));
        allPermutation3.emplace(z);
    } while (RAH2_NS::ranges::next_permutation(z, RAH2_NS::ranges::greater{}).found);
    assert(allPermutation3.size() == factorial(s.size()));
    /// [rah2::ranges::next_permutation]
}
void test_prev_permutation()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::prev_permutation]

    // Generate all permutations (iterators case)
    RAH2_STD::string s{"cba"};
    RAH2_STD::set<RAH2_STD::string> allPermutation;
    do
    {
        if (not allPermutation.empty())
            assert(s < *(allPermutation.begin()));
        allPermutation.emplace(s);
    } while (RAH2_NS::ranges::prev_permutation(s.begin(), s.end()).found);
    assert(allPermutation.size() == factorial(s.size()));

    RAH2_STD::set<RAH2_STD::array<int, 3>> allPermutation2;

    // Generate all permutations (range case)
    RAH2_STD::array<int, 3> a{'c', 'b', 'a'};
    do
    {
        if (not allPermutation2.empty())
            assert(a < *(allPermutation2.begin()));
        allPermutation2.emplace(a);
    } while (RAH2_NS::ranges::prev_permutation(a).found);
    assert(allPermutation2.size() == factorial(s.size()));

    RAH2_STD::set<RAH2_STD::array<RAH2_STD::string, 3>> allPermutation3;

    // Generate all permutations using comparator
    RAH2_STD::array<RAH2_STD::string, 3> z{"A", "B", "C"};
    do
    {
        if (not allPermutation3.empty())
            assert(z > *(--allPermutation3.end()));
        allPermutation3.emplace(z);
    } while (RAH2_NS::ranges::prev_permutation(z, RAH2_NS::ranges::greater{}).found);
    assert(allPermutation3.size() == factorial(s.size()));
    /// [rah2::ranges::prev_permutation]
}
void test_iota()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::iota]
    RAH2_STD::list<int> list(8);

    // Fill the list with ascending values: 0, 1, 2, ..., 7
    RAH2_NS::ranges::iota(list, 0);
    assert(list == (RAH2_STD::list<int>{0, 1, 2, 3, 4, 5, 6, 7}));
    /// [rah2::ranges::iota]
}
void test_fold_left()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::fold_left]
    RAH2_STD::vector<int> vecIn1{1, 2, 3, 4};
    assert(RAH2_NS::ranges::fold_left(vecIn1, 0, [](auto a, auto b) { return a + b; }) == 10);
    /// [rah2::ranges::fold_left]
}
void test_fold_left_first()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::fold_left_first]
    RAH2_STD::vector<int> v{1, 2, 3, 4, 5, 6, 7, 8};

    auto sum = RAH2_NS::ranges::fold_left_first(v.begin(), v.end(), RAH2_STD::plus<>()); // (1)
    assert(sum.value() == 36);

    auto mul = RAH2_NS::ranges::fold_left_first(v, RAH2_STD::multiplies<>()); // (2)
    assert(mul.value() == 40320);

    // get the product of the RAH2_STD::pair::second of all pairs in the vector:
    RAH2_STD::vector<RAH2_STD::pair<char, float>> data{{'A', 3.f}, {'B', 3.5f}, {'C', 4.f}};
    auto sec = RAH2_NS::ranges::fold_left_first(
        data | RAH2_NS::ranges::views::values, RAH2_STD::multiplies<>());
    assert(*sec == 42);

    // use a program defined function object (lambda-expression):
    auto val = RAH2_NS::ranges::fold_left_first(v, [](int x, int y) { return x + y + 13; });
    assert(*val == 127);
    /// [rah2::ranges::fold_left_first]
}
void test_fold_right()
{
    testSuite.test_case("sample");
    testSuite.test_case("return");
    /// [rah2::ranges::fold_right]
    auto v = {1, 2, 3, 4, 5, 6, 7, 8};
    RAH2_STD::vector<RAH2_STD::string> vs{"A", "B", "C", "D"};

    auto const r1 = RAH2_NS::ranges::fold_right(v.begin(), v.end(), 6, RAH2_STD::plus<>()); // (1)
    assert(r1 == 42);

    auto const r2 = RAH2_NS::ranges::fold_right(vs, RAH2_STD::string("!"), RAH2_STD::plus<>()); // (2)
    assert(r2 == RAH2_STD::string("ABCD!"));

    // Use a program defined function object (lambda-expression):
    RAH2_STD::string const r3 = RAH2_NS::ranges::fold_right(
        v, "A", [](int x, RAH2_STD::string const& s) { return s + ':' + RAH2_STD::to_string(x); });
    assert(r3 == RAH2_STD::string("A:8:7:6:5:4:3:2:1"));

    // Get the product of the RAH2_STD::pair::second of all pairs in the vector:
    RAH2_STD::vector<RAH2_STD::pair<char, float>> data{{'A', 2.f}, {'B', 3.f}, {'C', 3.5f}};
    float const r4 = RAH2_NS::ranges::fold_right(
        data | RAH2_NS::ranges::views::values, 2.0f, RAH2_STD::multiplies<>());
    assert(r4 == 42);
    /// [rah2::ranges::fold_right]
}
void test_fold_right_last()
{
    testSuite.test_case("sample");
    testSuite.test_case("return");
    /// [rah2::ranges::fold_right_last]
    auto v = {1, 2, 3, 4, 5, 6, 7, 8};
    RAH2_STD::vector<RAH2_STD::string> vs{"A", "B", "C", "D"};

    auto r1 = RAH2_NS::ranges::fold_right_last(v.begin(), v.end(), RAH2_STD::plus<>()); // (1)
    assert(*r1 == 36);

    auto r2 = RAH2_NS::ranges::fold_right_last(vs, RAH2_STD::plus<>()); // (2)
    assert(*r2 == "ABCD");

    // Use a program defined function object (lambda-expression):
    auto r3 = RAH2_NS::ranges::fold_right_last(v, [](int x, int y) { return x + y + 99; });
    assert(*r3 == 729);

    // Get the product of the RAH2_STD::pair::second of all pairs in the vector:
    RAH2_STD::vector<RAH2_STD::pair<char, float>> data{{'A', 3.f}, {'B', 3.5f}, {'C', 4.f}};
    auto r4 = RAH2_NS::ranges::fold_right_last(
        data | RAH2_NS::ranges::views::values, RAH2_STD::multiplies<>());
    assert(*r4 == 42);
    /// [rah2::ranges::fold_right_last]
}
void test_fold_left_with_iter()
{
    testSuite.test_case("sample");
    testSuite.test_case("return");
    /// [rah2::ranges::fold_left_with_iter]
    RAH2_STD::vector<int> v{1, 2, 3, 4, 5, 6, 7, 8};

    auto const sum = RAH2_NS::ranges::fold_left_with_iter(v.begin(), v.end(), 6, RAH2_STD::plus<>());
    assert(sum.value == 42);
    assert(sum.in == v.end());

    auto const mul = RAH2_NS::ranges::fold_left_with_iter(v, 0X69, RAH2_STD::multiplies<>());
    assert(mul.value == 4233600);
    assert(mul.in == v.end());

    // get the product of the RAH2_STD::pair::second of all pairs in the vector:
    RAH2_STD::vector<RAH2_STD::pair<char, float>> data{{'A', 2.f}, {'B', 3.f}, {'C', 3.5f}};
    auto const sec = RAH2_NS::ranges::fold_left_with_iter(
        data | RAH2_NS::ranges::views::values, 2.0f, RAH2_STD::multiplies<>());
    assert(sec.value == 42);

    // use a program defined function object (lambda-expression):
    auto lambda = [](int x, int y)
    {
        return x + 0B110 + y;
    };
    auto const val = RAH2_NS::ranges::fold_left_with_iter(v, -42, lambda);
    assert(val.value == 42);
    assert(val.in == v.end());
    /// [rah2::ranges::fold_left_with_iter]
}
void test_fold_left_first_with_iter()
{
    testSuite.test_case("sample");
    testSuite.test_case("return");
    /// [rah2::ranges::fold_left_first_with_iter]

    RAH2_STD::vector<int> v{1, 2, 3, 4, 5, 6, 7, 8};

    auto sum = RAH2_NS::ranges::fold_left_first_with_iter(v.begin(), v.end(), RAH2_STD::plus<>());
    assert(sum.value.value() == 36);
    assert(sum.in == v.end());

    auto mul = RAH2_NS::ranges::fold_left_first_with_iter(v, RAH2_STD::multiplies<>());
    assert(mul.value.value() == 40320);
    assert(mul.in == v.end());

    // get the product of the RAH2_STD::pair::second of all pairs in the vector:
    RAH2_STD::vector<RAH2_STD::pair<char, float>> data{{'A', 2.f}, {'B', 3.f}, {'C', 7.f}};
    auto sec = RAH2_NS::ranges::fold_left_first_with_iter(
        data | RAH2_NS::ranges::views::values, RAH2_STD::multiplies<>());
    assert(sec.value.value() == 42);

    // use a program defined function object (lambda-expression):
    auto lambda = [](int x, int y)
    {
        return x + y + 2;
    };
    auto val = RAH2_NS::ranges::fold_left_first_with_iter(v, lambda);
    assert(val.value.value() == 50);
    assert(val.in == v.end());

    /// [rah2::ranges::fold_left_first_with_iter]
}
void test_uninitialized_copy()
{
    testSuite.test_case("sample");
    // testSuite.test_case("return");
    /// [rah2::ranges::uninitialized_copy]

    char const* v[]{
        "This",
        "is",
        "an",
        "example",
    };

    auto const sz{RAH2_NS::ranges::size(v)};
    alignas(alignof(RAH2_STD::string)) char pbuf[sz * sizeof(RAH2_STD::string)];
    auto const first{reinterpret_cast<RAH2_STD::string*>(pbuf)};
    auto const last{first + sz};
    RAH2_NS::ranges::uninitialized_copy(RAH2_STD::begin(v), RAH2_STD::end(v), first, last);

    for (size_t i = 0; i < 4; ++i)
        assert(v[i] == first[i]);

    RAH2_NS::ranges::destroy(first, last); // NOLINT(cppcoreguidelines-no-malloc)

    /// [rah2::ranges::uninitialized_copy]
}
void test_uninitialized_copy_n()
{
    testSuite.test_case("sample");
    // testSuite.test_case("return");
    /// [rah2::ranges::uninitialized_copy_n]
    char const* stars[] = {"Procyon", "Spica", "Pollux", "Deneb", "Polaris"};

    constexpr int n{4};
    alignas(alignof(RAH2_STD::string)) char out[n * sizeof(RAH2_STD::string)];

    auto const first{reinterpret_cast<RAH2_STD::string*>(out)};
    auto const last{first + n};
    auto const ret = RAH2_NS::ranges::uninitialized_copy_n(RAH2_STD::begin(stars), n, first, last);
    assert(ret.in == stars + n);
    assert(ret.out == last);

    for (size_t i = 0; i < n; ++i)
        assert(stars[i] == first[i]);

    RAH2_NS::ranges::destroy(first, last);
    /// [rah2::ranges::uninitialized_copy_n]
}
void test_uninitialized_fill()
{
    testSuite.test_case("sample");
    // testSuite.test_case("return");
    /// [rah2::ranges::uninitialized_fill]

    constexpr int n{4};
    alignas(alignof(RAH2_STD::string)) char out[n * sizeof(RAH2_STD::string)];

    auto const first{reinterpret_cast<RAH2_STD::string*>(out)};
    auto const last{first + n};
    RAH2_NS::ranges::uninitialized_fill(first, last, "▄▀▄▀▄▀▄▀");

    assert(RAH2_NS::ranges::all_of(first, last, ([](auto& x) { return x == "▄▀▄▀▄▀▄▀"; })));

    RAH2_NS::ranges::destroy(first, last);

    /// [rah2::ranges::uninitialized_fill]
}
void test_uninitialized_fill_n()
{
    testSuite.test_case("sample");
    testSuite.test_case("return");
    /// [rah2::ranges::uninitialized_fill_n]

    constexpr int n{3};
    alignas(alignof(RAH2_STD::string)) char out[n * sizeof(RAH2_STD::string)];

    auto const first{reinterpret_cast<RAH2_STD::string*>(out)};
    auto const last = RAH2_NS::ranges::uninitialized_fill_n(first, n, "cppreference");

    assert(RAH2_NS::ranges::all_of(first, last, ([](auto& x) { return x == "cppreference"; })));

    RAH2_NS::ranges::destroy(first, last);

    /// [rah2::ranges::uninitialized_fill_n]
}
void test_uninitialized_move()
{
    testSuite.test_case("sample");
    // testSuite.test_case("return");
    /// [rah2::ranges::uninitialized_move]

    RAH2_STD::string in[]{"Home", "World"};

    constexpr auto sz = RAH2_NS::ranges::size(in);
    alignas(alignof(RAH2_STD::string)) char out[sz * sizeof(RAH2_STD::string)];
    auto const first{reinterpret_cast<RAH2_STD::string*>(out)};
    auto const last{first + sz};
    RAH2_NS::ranges::uninitialized_move(RAH2_STD::begin(in), RAH2_STD::end(in), first, last);
    assert(*first == "Home");
    assert(*RAH2_NS::ranges::next(first) == "World");
    RAH2_NS::ranges::destroy(first, last);
    /// [rah2::ranges::uninitialized_move]
}
void test_uninitialized_move_n()
{
    testSuite.test_case("sample");
    // testSuite.test_case("return");
    /// [rah2::ranges::uninitialized_move_n]

    RAH2_STD::string in[] = {"No", "Diagnostic", "Required"};

    constexpr auto sz = RAH2_NS::ranges::size(in);
    alignas(alignof(RAH2_STD::string)) char out[sz * sizeof(RAH2_STD::string)];
    auto const first{reinterpret_cast<RAH2_STD::string*>(out)};
    auto const last{first + sz};
    RAH2_NS::ranges::uninitialized_move_n(RAH2_STD::begin(in), sz, first, last);
    RAH2_NS::ranges::equal(
        RAH2_NS::ranges::make_subrange(first, last),
        std::initializer_list<RAH2_STD::string>{"No", "Diagnostic", "Required"});

    RAH2_NS::ranges::destroy(first, last);

    /// [rah2::ranges::uninitialized_move_n]
}
void test_uninitialized_default_construct()
{
    testSuite.test_case("sample");
    // testSuite.test_case("return");
    /// [rah2::ranges::uninitialized_default_construct]

    struct S
    {
        RAH2_STD::string m{"▄▀▄▀▄▀▄▀"};
    };

    constexpr int n{4};
    alignas(alignof(S)) char out[n * sizeof(S)];

    auto const first{reinterpret_cast<S*>(out)};
    auto const last{first + n};

    RAH2_NS::ranges::uninitialized_default_construct(first, last);

    assert(RAH2_NS::ranges::all_of(first, last, [](S& s) { return s.m == "▄▀▄▀▄▀▄▀"; }));

    RAH2_NS::ranges::destroy(first, last); // NOLINT(cppcoreguidelines-no-malloc)

    // Notice that for "trivial types" the uninitialized_default_construct
    // generally does not zero-fill the given uninitialized memory area.
    constexpr char etalon[]{'A', 'B', 'C', 'D', '\n'};
    char v[]{'A', 'B', 'C', 'D', '\n'};
    RAH2_NS::ranges::uninitialized_default_construct(RAH2_STD::begin(v), RAH2_STD::end(v));
    assert(std::memcmp(v, etalon, sizeof(v)) == 0);

    /// [rah2::ranges::uninitialized_default_construct]
}
void test_uninitialized_default_construct_n()
{
    testSuite.test_case("sample");
    testSuite.test_case("return");
    /// [rah2::ranges::uninitialized_default_construct_n]
    struct S
    {
        RAH2_STD::string m{"█▓▒░ █▓▒░ "};
    };

    constexpr int n{4};
    alignas(alignof(S)) char out[n * sizeof(S)];

    auto const first{reinterpret_cast<S*>(out)};
    auto const last = RAH2_NS::ranges::uninitialized_default_construct_n(first, n);
    assert(RAH2_NS::ranges::all_of(first, last, [](S& s) { return s.m == "█▓▒░ █▓▒░ "; }));

    RAH2_NS::ranges::destroy(first, last);

    // Notice that for "trivial types" the uninitialized_default_construct_n
    // generally does not zero-fill the given uninitialized memory area.
    constexpr int etalon[]{1, 2, 3, 4, 5, 6};
    int v[]{1, 2, 3, 4, 5, 6};
    RAH2_NS::ranges::uninitialized_default_construct_n(
        RAH2_STD::begin(v), static_cast<intptr_t>(RAH2_NS::ranges::size(v)));
    assert(std::memcmp(v, etalon, sizeof(v)) == 0);
    /// [rah2::ranges::uninitialized_default_construct_n]
}
void test_uninitialized_value_construct()
{
    testSuite.test_case("sample");
    // testSuite.test_case("return");
    // ReSharper disable once CppInconsistentNaming
    /// [rah2::ranges::uninitialized_value_construct]
    struct S
    {
        RAH2_STD::string m{"▄▀▄▀▄▀▄▀"};
    };

    constexpr int n{4};
    alignas(alignof(S)) char out[n * sizeof(S)];

    auto const first{reinterpret_cast<S*>(out)};
    auto const last{first + n};

    RAH2_NS::ranges::uninitialized_value_construct(first, last);
    assert(RAH2_NS::ranges::all_of(first, last, [](S& s) { return s.m == "▄▀▄▀▄▀▄▀"; }));

    RAH2_NS::ranges::destroy(first, last);
    // Notice that for "trivial types" the uninitialized_value_construct
    // zero-fills the given uninitialized memory area.
    int v[]{0, 1, 2, 3};
    RAH2_NS::ranges::uninitialized_value_construct(RAH2_STD::begin(v), RAH2_STD::end(v));
    assert(RAH2_NS::ranges::all_of(v, [](int i) { return i == 0; }));
    /// [rah2::ranges::uninitialized_value_construct]
}
void test_uninitialized_value_construct_n()
{
    testSuite.test_case("sample");
    testSuite.test_case("return");
    // ReSharper disable once CppInconsistentNaming
    /// [rah2::ranges::uninitialized_value_construct_n]
    struct S
    {
        RAH2_STD::string m{"█▓▒░ █▓▒░ █▓▒░ "};
    };

    constexpr int n{4};
    alignas(alignof(S)) char out[n * sizeof(S)];

    auto const first{reinterpret_cast<S*>(out)};
    auto const last = RAH2_NS::ranges::uninitialized_value_construct_n(first, n);
    assert(RAH2_NS::ranges::all_of(first, last, [](S& s) { return s.m == "█▓▒░ █▓▒░ █▓▒░ "; }));

    RAH2_NS::ranges::destroy(first, last);

    // Notice that for "trivial types" the uninitialized_value_construct_n
    // zero-initializes the given uninitialized memory area.
    int v[]{1, 2, 3, 4, 5, 6, 7, 8};
    RAH2_NS::ranges::uninitialized_value_construct_n(
        RAH2_STD::begin(v), static_cast<intptr_t>(RAH2_NS::ranges::size(v)));
    assert(RAH2_NS::ranges::all_of(v, [](int i) { return i == 0; }));

    /// [rah2::ranges::uninitialized_value_construct_n]
}
void test_destroy()
{
    testSuite.test_case("sample");
    // testSuite.test_case("return");
    /// [rah2::ranges::destroy]
    struct tracer // NOLINT(cppcoreguidelines-special-member-functions)
    {
        volatile int value;
        ~tracer()
        {
            value = 42;
        }
    };
    alignas(tracer) unsigned char buffer[sizeof(tracer) * 8];

    for (int i = 0; i < 8; ++i)
        new (buffer + sizeof(tracer) * i) tracer{i}; //manually construct objects

    auto const ptr = reinterpret_cast<tracer*>(buffer);

    RAH2_NS::ranges::destroy(ptr, ptr + 8);
#if defined(__GNUC__) && !defined(__clang__)
    RAH2_EXT_WARNING_PUSH
#pragma GCC diagnostic ignored "-Wmaybe-uninitialized"
#endif
    assert(RAH2_NS::ranges::all_of(ptr, ptr + 8, [](tracer const& t) { return t.value == 42; }));
#if defined(__GNUC__) && !defined(__clang__)
    RAH2_EXT_WARNING_POP
#endif
    /// [rah2::ranges::destroy]
}
void test_destroy_n()
{
    testSuite.test_case("sample");
    // testSuite.test_case("return");
    /// [rah2::ranges::destroy_n]
    struct tracer // NOLINT(cppcoreguidelines-special-member-functions)
    {
        volatile int value;
        ~tracer()
        {
            value = 42;
        }
    };

    alignas(tracer) unsigned char buffer[sizeof(tracer) * 8];

    for (int i = 0; i < 8; ++i)
        new (buffer + sizeof(tracer) * i) tracer{i}; //manually construct objects

    auto const ptr = reinterpret_cast<tracer*>(buffer);

    RAH2_NS::ranges::destroy_n(ptr, 8);
#if defined(__GNUC__) && !defined(__clang__)
    RAH2_EXT_WARNING_PUSH
#pragma GCC diagnostic ignored "-Wmaybe-uninitialized"
#endif
    assert(RAH2_NS::ranges::all_of(ptr, ptr + 8, [](tracer& t) { return t.value == 42; }));
#if defined(__GNUC__) && !defined(__clang__)
    RAH2_EXT_WARNING_POP
#endif
    /// [rah2::ranges::destroy_n]
}
void test_destroy_at()
{
    testSuite.test_case("sample");
    // testSuite.test_case("return");
    /// [rah2::ranges::destroy_at]
    struct tracer // NOLINT(cppcoreguidelines-special-member-functions)
    {
        volatile int value;
        ~tracer()
        {
            value = 42;
        }
    };

    alignas(tracer) unsigned char buffer[sizeof(tracer) * 8];

    for (int i = 0; i < 8; ++i)
        new (buffer + sizeof(tracer) * i) tracer{i}; //manually construct objects

    auto const ptr = reinterpret_cast<tracer*>(buffer);

    for (int i = 0; i < 8; ++i)
        RAH2_NS::ranges::destroy_at(ptr + i);
#if defined(__GNUC__) && !defined(__clang__)
    RAH2_EXT_WARNING_PUSH
#pragma GCC diagnostic ignored "-Wmaybe-uninitialized"
#endif
    assert(RAH2_NS::ranges::all_of(ptr, ptr + 8, [](tracer& t) { return t.value == 42; }));
#if defined(__GNUC__) && !defined(__clang__)
    RAH2_EXT_WARNING_POP
#endif
    /// [rah2::ranges::destroy_at]
}
void test_construct_at()
{
    testSuite.test_case("sample");
    testSuite.test_case("return");
    /// [rah2::ranges::construct_at]
    // ReSharper disable once CppInconsistentNaming
    struct S
    {
        int x;
        float y;
        double z;

        S(int const x, float const y, double const z)
            : x{x}
            , y{y}
            , z{z}
        {
        }
    };

    alignas(S) unsigned char buf[sizeof(S)];

    S* ptr = RAH2_NS::ranges::construct_at(reinterpret_cast<S*>(buf), 42, 2.71828f, 3.1415);
    assert(ptr->x == 42);

    RAH2_NS::ranges::destroy_at(ptr);
    /// [rah2::ranges::construct_at]
}
