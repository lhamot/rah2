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
struct test_all_of_
{
    template <bool = true>
    void test()
    {
        std::vector<int> raw_yes = {4, 4, 4, 4};
        assert(RAH2_NS::ranges::all_of(
            make_test_view_adapter<CS, Tag, Sized>(raw_yes), [](auto a) { return a == 4; }));
        std::vector<int> raw_no = {4, 4, 3, 4};
        assert(!RAH2_NS::ranges::all_of(
            make_test_view_adapter<CS, Tag, Sized>(raw_no), [](auto a) { return a == 4; }));
        std::vector<int> raw_empty = {};
        assert(RAH2_NS::ranges::all_of(
            make_test_view_adapter<CS, Tag, Sized>(raw_empty), [](auto a) { return a == 4; }));
    }
    template <bool = true>
    void test_perf(char const* range_type)
    {
        std::vector<Coord> perf_no_vec(1000000 * RELEASE_MULTIPLIER, {0, 2});
        auto perf_no = make_test_view_adapter<CS, Tag, Sized>(perf_no_vec);
        auto is_zero = [](Coord const& value)
        {
            return value.x == 0;
        };
        COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(
            CS == Common,
            "all_of",
            range_type,
            [&]
            {
                auto result = STD::all_of(fwd(perf_no.begin()), perf_no.end(), is_zero);
                assert(result);
            });
        COMPARE_DURATION_TO_STD_RANGES(
            "all_of_proj",
            range_type,
            [&]
            {
                auto result = STD::all_of(
                    perf_no, [](intptr_t x) { return x == 0; }, &Coord::x);
                assert(result);
            });
    }
    static constexpr bool do_test = true;
};
void test_all_of()
{
    testSuite.test_case("sample");
    testSuite.test_case("range");
    testSuite.test_case("yes");
    /// [rah2::ranges::all_of]
    assert(RAH2_NS::ranges::all_of(
        std::initializer_list<int>{4, 4, 4, 4}, [](auto a) { return a == 4; }));
    /// [rah2::ranges::all_of]
    testSuite.test_case("no");
    RAH2_STD::vector<int> vec = {4, 4, 3, 4};
    testSuite.test_case("iter");
    assert(RAH2_NS::ranges::all_of(vec.begin(), vec.end(), [](auto a) { return a == 4; }) == false);

    testSuite.test_case("all_input_types");
    testSuite.test_case("empty");
    foreach_range_combination<test_algo<test_all_of_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_any_of_
{
    template <bool = true>
    void test()
    {
        std::vector<int> raw_yes = {3, 3, 4, 3};
        assert(RAH2_NS::ranges::any_of(
            make_test_view_adapter<CS, Tag, Sized>(raw_yes), [](auto a) { return a == 4; }));
        std::vector<int> raw_no = {3, 2, 3, 5};
        assert(!RAH2_NS::ranges::any_of(
            make_test_view_adapter<CS, Tag, Sized>(raw_no), [](auto a) { return a == 4; }));
        std::vector<int> raw_empty = {};
        assert(!RAH2_NS::ranges::any_of(
            make_test_view_adapter<CS, Tag, Sized>(raw_empty), [](auto a) { return a == 4; }));
    }
    template <bool = true>
    void test_perf(char const* range_type)
    {
        std::vector<Coord> perf_no_vec(1000000 * RELEASE_MULTIPLIER);
        auto perf_no = make_test_view_adapter<CS, Tag, Sized>(perf_no_vec);
        auto is_one = [](Coord const& value)
        {
            return value.x == 1;
        };
        COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(
            CS == Common,
            "any_of",
            range_type,
            [&]
            {
                auto result = STD::any_of(fwd(perf_no.begin()), perf_no.end(), is_one);
                assert(!result);
            });
        COMPARE_DURATION_TO_STD_RANGES(
            "any_of_proj",
            range_type,
            [&]
            {
                auto result = STD::any_of(
                    perf_no, [](intptr_t v) { return v == 1; }, &Coord::x);
                assert(!result);
            });
    }
    static constexpr bool do_test = true;
};
void test_any_of()
{
    testSuite.test_case("sample");
    testSuite.test_case("yes");
    testSuite.test_case("range");
    /// [rah2::ranges::any_of]
    assert(RAH2_NS::ranges::any_of(
        std::initializer_list<int>{3, 0, 1, 3, 4, 6}, [](auto a) { return a == 3; }));
    /// [rah2::ranges::any_of]
    testSuite.test_case("no");
    testSuite.test_case("iter");
    RAH2_STD::vector<int> vec = {3, 0, 1, 3, 4, 6};
    assert(RAH2_NS::ranges::any_of(vec.begin(), vec.end(), [](auto a) { return a == 5; }) == false);

    testSuite.test_case("all_input_types");
    testSuite.test_case("empty");
    foreach_range_combination<test_algo<test_any_of_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_none_of_
{
    template <bool = true>
    void test()
    {
        std::vector<int> raw_yes = {3, 2, 3, 5};
        assert(RAH2_NS::ranges::none_of(
            make_test_view_adapter<CS, Tag, Sized>(raw_yes), [](auto a) { return a == 4; }));
        std::vector<int> raw_no = {3, 2, 4, 5};
        assert(!RAH2_NS::ranges::none_of(
            make_test_view_adapter<CS, Tag, Sized>(raw_no), [](auto a) { return a == 4; }));
        std::vector<int> raw_empty = {};
        assert(RAH2_NS::ranges::none_of(
            make_test_view_adapter<CS, Tag, Sized>(raw_empty), [](auto a) { return a == 4; }));
    }
    template <bool = true>
    void test_perf(char const* range_type)
    {

        std::vector<Coord> perf_no_vec(1000000 * RELEASE_MULTIPLIER);
        auto perf_no = make_test_view_adapter<CS, Tag, Sized>(perf_no_vec);
        auto is_one = [](Coord const& value)
        {
            return value.x == 1;
        };
        COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(
            CS == Common,
            "none_of",
            range_type,
            [&]
            {
                auto result = STD::none_of(fwd(perf_no.begin()), perf_no.end(), is_one);
                assert(result);
            });
        COMPARE_DURATION_TO_STD_RANGES(
            "none_of_proj",
            range_type,
            [&]
            {
                auto result = STD::none_of(
                    perf_no, [](intptr_t x) { return x == 1; }, &Coord::x);
                assert(result);
            });
    }
    static constexpr bool do_test = true;
};
void test_none_of()
{
    testSuite.test_case("sample");
    testSuite.test_case("yes");
    testSuite.test_case("range");
    /// [rah2::ranges::none_of]
    assert((RAH2_NS::ranges::none_of(
        std::initializer_list<int>{7, 8, 9, 10}, [](auto a) { return a == 11; })));
    /// [rah2::ranges::none_of]
    testSuite.test_case("no");
    testSuite.test_case("iter");
    RAH2_STD::vector<int> vec = {7, 8, 9, 10};
    assert(RAH2_NS::ranges::none_of(vec.begin(), vec.end(), [](auto a) { return a == 9; }) == false);

    testSuite.test_case("all_input_types");
    testSuite.test_case("empty");
    foreach_range_combination<test_algo<test_none_of_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_for_each_
{
    template <bool = true>
    void test()
    {
#if defined(TEST_DISPLAY_ALL)
        std::cout << "range_type : " << range_type << std::endl;
#endif
        size_t sum = 0;
        std::vector<int> vec(1000000 * RELEASE_MULTIPLIER, 1);
        auto func = [&sum](auto v)
        {
            sum += v;
        };
        auto r = make_test_view_adapter<CS, Tag, Sized>(vec);
        auto result = RAH2_NS::ranges::for_each(r, func);
        assert(sum == vec.size());
        assert(result.in == r.end());
        result.fun(1);
        assert(sum == vec.size() + 1); // result.fun

        std::vector<int> vec_empty;
        sum = 0;
        auto r2 = make_test_view_adapter<CS, Tag, Sized>(vec_empty);
        auto result2 = RAH2_NS::ranges::for_each(r2, func);
        assert(sum == 0);
        assert(result2.in == r2.end());
        result.fun(1);
        assert(sum == vec_empty.size() + 1); // result.fun
    }
    template <bool = true>
    void test_perf(char const* range_type)
    {
        std::vector<Coord> coord_vec(1000000 * RELEASE_MULTIPLIER, {1, 2});
        COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(
            CS == Common,
            "for_each",
            range_type,
            (
                [&]
                {
                    intptr_t sum = 0;
                    auto r3 = make_test_view_adapter<CS, Tag, Sized>(coord_vec);
                    STD::for_each(fwd(r3.begin()), r3.end(), [&sum](Coord const& c) { sum += c.x; });
                    assert(sum == 1000000 * RELEASE_MULTIPLIER);
                }));
        COMPARE_DURATION_TO_STD_RANGES(
            "for_each_proj",
            range_type,
            (
                [&coord_vec]
                {
                    intptr_t sum = 0;
                    auto r3 = make_test_view_adapter<CS, Tag, Sized>(coord_vec);
                    STD::for_each(
                        r3, [&sum](intptr_t x) { sum += x; }, &Coord::x);
                    assert(sum == 1000000 * RELEASE_MULTIPLIER);
                }));
    }
    static constexpr bool do_test = true;
};
void test_for_each()
{
    testSuite.test_case("sample");
    testSuite.test_case("range");
    /// [rah2::ranges::for_each]
    RAH2_STD::vector<int> testFE{4, 4, 4, 4};
    RAH2_NS::ranges::for_each(testFE, [](auto& value) { return ++value; });
    assert(RAH2_NS::ranges::equal(testFE, std::initializer_list<int>({5, 5, 5, 5})));
    /// [rah2::ranges::for_each]
    STATIC_ASSERT((RAH2_NS::ranges::details::input_range_impl<decltype(testFE), true>::value));
    testSuite.test_case("iter");
    RAH2_NS::ranges::for_each(testFE.begin(), testFE.end(), [](auto& value) { return ++value; });
    assert(RAH2_NS::ranges::equal(testFE, std::initializer_list<int>({6, 6, 6, 6})));

    testSuite.test_case("all_input_types");
    testSuite.test_case("empty");
    testSuite.test_case("proj");
    testSuite.test_case("noproj");
    foreach_range_combination<test_algo<test_for_each_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_for_each_n_
{
    template <bool = true>
    void test()
    {
        size_t sum = 0;
        static constexpr auto vec_size = 1000 * RELEASE_MULTIPLIER;
        std::vector<int> vec(vec_size, 1);
        auto func = [&sum](auto v)
        {
            sum += v;
        };
        auto r = make_test_view_adapter<CS, Tag, Sized>(vec);
        auto result = RAH2_NS::ranges::for_each_n(r.begin(), vec_size, func);
        assert(sum == vec.size());
        assert(result.in == r.end());
        result.fun(1);
        assert(sum == vec.size() + 1); // result.fun

        std::vector<int> vec_empty;
        sum = 0;
        auto r2 = make_test_view_adapter<CS, Tag, Sized>(vec_empty);
        auto result2 = RAH2_NS::ranges::for_each_n(r2.begin(), 0, func);
        assert(sum == 0);
        assert(result2.in == r2.end());
        result.fun(1);
        assert(sum == vec_empty.size() + 1); // result.fun
    }
    template <bool = true>
    void test_perf(char const* range_type)
    {
        static constexpr auto vec_size = 1000000 * RELEASE_MULTIPLIER;
        std::vector<Coord> coord_vec(vec_size, {1, 2});
        COMPARE_DURATION_TO_STD_ALGO_17_AND_RANGES(
            CS == Common,
            "for_each_n",
            range_type,
            (
                [&]
                {
                    intptr_t sum = 0;
                    auto r3 = make_test_view_adapter<CS, Tag, Sized>(coord_vec);
                    fwd(STD::for_each_n(
                        fwd(r3.begin()), vec_size, [&sum](Coord const& c) { sum += c.x; }));
                    assert(sum == vec_size);
                }));
        COMPARE_DURATION_TO_STD_RANGES(
            "for_each_n_proj",
            range_type,
            (
                [&coord_vec]
                {
                    intptr_t sum = 0;
                    auto r3 = make_test_view_adapter<CS, Tag, Sized>(coord_vec);
                    STD::for_each_n(
                        r3.begin(), vec_size, [&sum](intptr_t x) { sum += x; }, &Coord::x);
                    assert(sum == vec_size);
                }));
    }
    static constexpr bool do_test = true;
};
void test_for_each_n()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::for_each_n]
    RAH2_STD::vector<int> testFE{4, 4, 4, 4};
    auto const res = RAH2_NS::ranges::for_each_n(
        testFE.begin(), static_cast<intptr_t>(testFE.size()), [](auto& value) { return ++value; });
    assert(RAH2_NS::ranges::equal(testFE, std::initializer_list<int>({5, 5, 5, 5})));
    assert(res.in == testFE.end());
    /// [rah2::ranges::for_each_n]

    testSuite.test_case("all_input_types");
    testSuite.test_case("empty");
    testSuite.test_case("proj");
    testSuite.test_case("noproj");
    foreach_range_combination<test_algo<test_for_each_n_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_count_
{
    template <bool = true>
    void test()
    {
        testSuite.test_case("range");
        testSuite.test_case("noproj");
        RAH2_STD::vector<int> intsVec({4, 4, 4, 3});
        auto intsRange = make_test_view_adapter<CS, Tag, Sized>(intsVec);
        assert(RAH2_NS::ranges::count(intsRange, 3) == 1);

        testSuite.test_case("proj");
        RAH2_STD::vector<Coord> coordsVec({Coord{1, 1}, {1, 2}, {2, 1}, {2, 3}, {3, 1}});
        static auto coords = make_test_view_adapter<CS, Tag, Sized>(coordsVec);
        assert(RAH2_NS::ranges::count(coords, 1, RAH2_STD::mem_fn(&Coord::x)) == 2);

        testSuite.test_case("iter");
        assert(
            RAH2_NS::ranges::count(coords.begin(), coords.end(), 1, RAH2_STD::mem_fn(&Coord::x)) == 2);
    }
    template <bool = true>
    void test_perf(char const* range_type)
    {
        RAH2_STD::vector<Coord> coordsVec;
        coordsVec.insert(coordsVec.end(), 1000000, {1, 47});
        coordsVec.insert(coordsVec.end(), 79, {2, 47});
        coordsVec.insert(coordsVec.end(), 1000000, {3, 47});
        RAH2_STD::vector<int> intsVec;
        intsVec.insert(intsVec.end(), 1000000, 1);
        intsVec.insert(intsVec.end(), 79, 2);
        intsVec.insert(intsVec.end(), 1000000, 3);

        auto intsRange = make_test_view_adapter<CS, Tag, Sized>(intsVec);
        auto coordsRange = make_test_view_adapter<CS, Tag, Sized>(coordsVec);
        (void)coordsRange;

        auto getter_lbd = [](Coord const& c)
        {
            return c.x;
        };
        (void)getter_lbd;
        testSuite.test_case("perf");
        COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(
            CS == Common,
            "count_noproj",
            range_type,
            [&]
            {
                const auto count = STD::count(fwd(intsRange.begin()), intsRange.end(), 2);
                assert(count == 79);
            });
        COMPARE_DURATION_TO_STD_RANGES(
            "count_proj",
            range_type,
            [&]
            {
                const auto count = STD::count(coordsRange, 2, getter_lbd);
                assert(count == 79);
            });
    }
    static constexpr bool do_test = true;
};
void test_algo_count()
{
    testSuite.test_case("sample");
    {
        /// [rah2::ranges::count]
        assert(RAH2_NS::ranges::count(std::initializer_list<int>{4, 4, 4, 3}, 3) == 1);
        struct Coord
        {
            int x;
            int y;
        };
        static auto coords = {Coord{1, 1}, {1, 2}, {2, 1}, {2, 3}, {3, 1}};
        assert(RAH2_NS::ranges::count(coords, 1, RAH2_STD::mem_fn(&Coord::x)) == 2);
        /// [rah2::ranges::count]
    }

    foreach_range_combination<test_algo<test_count_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_count_if_
{
    template <bool = true>
    void test()
    {
        testSuite.test_case("range");
        testSuite.test_case("noproj");
        RAH2_STD::vector<int> vec = {4, 4, 4, 3};
        auto intsRange = make_test_view_adapter<CS, Tag, Sized>(vec);
        assert(RAH2_NS::ranges::count_if(intsRange, [](auto a) { return a == 4; }) == 3);

        testSuite.test_case("iter");
        testSuite.test_case("proj");
        RAH2_STD::vector<Coord> coords = {{4, 1}, {4, 2}, {4, 3}, {3, 4}};
        auto coordsRange = make_test_view_adapter<CS, Tag, Sized>(coords);
        assert(
            RAH2_NS::ranges::count_if(
                coordsRange.begin(), coordsRange.end(), [](auto a) { return a == 4; }, &Coord::x)
            == 3);
    }
    template <bool = true>
    void test_perf(char const* range_type)
    {
        testSuite.test_case("perf");
        RAH2_STD::vector<Coord> coords_vec;
        coords_vec.insert(coords_vec.end(), 1000000, {1, 47});
        coords_vec.insert(coords_vec.end(), 79, {2, 47});
        coords_vec.insert(coords_vec.end(), 1000000, {3, 47});
        auto coordRange = make_test_view_adapter<CS, Tag, Sized>(coords_vec);
        auto pred = [](Coord const& c)
        {
            return c.x == 2;
        };
        COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(
            CS == Common,
            "count_if",
            range_type,
            [&]
            {
                const auto count = STD::count_if(fwd(coordRange.begin()), coordRange.end(), pred);
                assert(count == 79);
            });
        COMPARE_DURATION_TO_STD_RANGES(
            "count_if_proj",
            range_type,
            [&]
            {
                auto proj = [](Coord const& c)
                {
                    return c;
                };
                const auto count = STD::count_if(coordRange, pred, proj);
                assert(count == 79);
            });
    }
    static constexpr bool do_test = true;
};
void test_count_if()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::count_if]
    RAH2_STD::vector<int> vec = {4, 4, 4, 3};
    assert(RAH2_NS::ranges::count_if(vec, [](auto a) { return a == 4; }) == 3);
    /// [rah2::ranges::count_if]

    foreach_range_combination<test_algo<test_count_if_>>();
}
