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
#include <EASTL/utility.h>

#else
#include <array>
#include <list>
#include <algorithm>
#include <numeric>
#include <forward_list>
#include <set>
#include <cstring>
#include <utility>
#endif

#if RAH2_CPP20
#include <algorithm>
#endif

#include "test_helpers.hpp"

#ifdef RAH2_USE_EASTL
namespace eastl
{
    template <class InputIt1, class InputIt2>
    eastl::pair<InputIt1, InputIt2> mismatch(InputIt1 first1, InputIt1 last1, InputIt2 first2, InputIt2)
    {
        return ::eastl::mismatch(first1, last1, first2);
    }
} // namespace eastl
#endif

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_mismatch_
{
    template <bool = true>
    void test()
    {
        // TODO : Test all ranges combinations ?
        {
            testSuite.test_case("noproj");
            testSuite.test_case("nopred");
            testSuite.test_case("iter");
            RAH2_STD::vector<int> in1 = {1, 2, 3, 4};
            RAH2_STD::vector<int> in2 = {1, 2, 42, 43};
            auto rng1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto rng2 = make_test_view_adapter<CS, Tag, Sized>(in2);
            auto r1_r2 =
                RAH2_NS::ranges::mismatch(rng1.begin(), rng1.end(), rng2.begin(), rng2.end());
            assert(*r1_r2.in1 == 3);
            assert(*r1_r2.in2 == 42);
        }
        {
            testSuite.test_case("range");
            testSuite.test_case("proj");
            testSuite.test_case("pred");
            RAH2_STD::vector<Coord> in1 = {{1, 0}, {2, 0}, {3, 5}, {4, 2}};
            RAH2_STD::vector<Coord> in2 = {{6, 1}, {2, 2}, {2, 42}, {3, 43}};
            auto rng1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto rng2 = make_test_view_adapter<CS, Tag, Sized>(in2);
            auto r1_r2 = RAH2_NS::ranges::mismatch(
                rng1, rng2, [](auto a, auto b) { return a == b; }, &Coord::x, &Coord::y);
            assert(*r1_r2.in1 == (Coord{3, 5}));
            assert(*r1_r2.in2 == (Coord{2, 42}));
        }
    }
    template <bool = true>
    void test_perf(char const* range_type)
    {
        testSuite.test_case("perf");
        RAH2_STD::vector<Coord> coords_vec;
        coords_vec.insert(coords_vec.end(), 1000000 * RELEASE_MULTIPLIER, Coord{1, 47});
        coords_vec.insert(coords_vec.end(), 79 * RELEASE_MULTIPLIER, Coord{3, 47});
        coords_vec.insert(coords_vec.end(), 10000 * RELEASE_MULTIPLIER, Coord{3, 47});
        RAH2_STD::vector<Coord> coords_vec2;
        coords_vec2.insert(coords_vec2.end(), 1000000 * RELEASE_MULTIPLIER, Coord{1, 47});
        coords_vec2.insert(coords_vec2.end(), 79 * RELEASE_MULTIPLIER, Coord{2, 47});
        coords_vec2.insert(coords_vec2.end(), 10000 * RELEASE_MULTIPLIER, Coord{2, 48});

        auto coordRange1 = make_test_view_adapter<CS, Tag, Sized>(coords_vec);
        auto coordRange2 = make_test_view_adapter<CS, Tag, Sized>(coords_vec2);
        COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(
            CS == Common,
            "mismatch",
            range_type,
            [&]
            {
                volatile const auto v1_v2 = STD::mismatch(
                    fwd(coordRange1.begin()),
                    coordRange1.end(),
                    coordRange2.begin(),
                    coordRange2.end());
                DONT_OPTIM(v1_v2);
            });
        COMPARE_DURATION_TO_STD_RANGES(
            "mismatch_pred_proj",
            range_type,
            [&]
            {
                auto pred = [](intptr_t a, intptr_t b)
                {
                    return a == b;
                };
                volatile const auto v1_v2 =
                    STD::mismatch(coordRange1, coordRange2, pred, &Coord::x, &Coord::x);
                DONT_OPTIM(v1_v2);
            });
    }
    static constexpr bool do_test = true;
};
void test_mismatch()
{
    testSuite.test_case("sample");
    testSuite.test_case("range");
    /// [rah2::ranges::mismatch]
    RAH2_STD::vector<int> in1 = {1, 2, 3, 4};
    RAH2_STD::vector<int> in2 = {1, 2, 42, 43};
    auto r1_r2 = RAH2_NS::ranges::mismatch(in1, in2);
    assert(*r1_r2.in1 == 3);
    assert(*r1_r2.in2 == 42);
    /// [rah2::ranges::mismatch]

    foreach_range_combination<test_algo<test_mismatch_>>();
}

#ifdef RAH2_USE_EASTL

namespace RAH2_STD
{
    template <class InputIt1, class InputIt2>
    bool equal(InputIt1 first1, InputIt1 last1, InputIt2 first2, InputIt2 last)
    {
        return ::eastl::identical(first1, last1, first2, last);
    }
} // namespace RAH2_STD
#endif

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_equal_
{
    template <bool = true>
    void test()
    {
        // TODO : Test all ranges combinations ?
        {
            testSuite.test_case("noproj");
            testSuite.test_case("nopred");
            testSuite.test_case("iter");
            RAH2_STD::vector<int> in1 = {1, 2, 3, 4};
            RAH2_STD::vector<int> in2 = {1, 2, 42, 43};
            auto rng1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto rng2 = make_test_view_adapter<CS, Tag, Sized>(in2);
            assert(not RAH2_NS::ranges::equal(rng1.begin(), rng1.end(), rng2.begin(), rng2.end()));
            assert(RAH2_NS::ranges::equal(rng1.begin(), rng1.end(), rng1.begin(), rng1.end()));
        }
        {
            testSuite.test_case("range");
            testSuite.test_case("proj");
            testSuite.test_case("pred");
            RAH2_STD::vector<Coord> in1 = {{1, 0}, {2, 0}, {3, 5}, {4, 2}};
            RAH2_STD::vector<Coord> in2 = {{6, 1}, {2, 2}, {2, 3}, {3, 4}};
            auto rng1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto rng2 = make_test_view_adapter<CS, Tag, Sized>(in2);
            assert(RAH2_NS::ranges::equal(rng1, rng2, [](auto a, auto b) { return a.x == b.y; }));
        }
    }
    template <bool = true>
    void test_perf(char const* range_type)
    {
        testSuite.test_case("perf");
        RAH2_STD::vector<Coord> coords_vec;
        coords_vec.insert(coords_vec.end(), 1000000 * RELEASE_MULTIPLIER, Coord{1, 1});
        coords_vec.insert(coords_vec.end(), 79 * RELEASE_MULTIPLIER, Coord{3, 4});
        coords_vec.insert(coords_vec.end(), 10000 * RELEASE_MULTIPLIER, Coord{3, 3});
        RAH2_STD::vector<Coord> coords_vec2;
        coords_vec2.insert(coords_vec2.end(), 1000000 * RELEASE_MULTIPLIER, Coord{1, 1});
        coords_vec2.insert(coords_vec2.end(), 79 * RELEASE_MULTIPLIER, Coord{5, 3});
        coords_vec2.insert(coords_vec2.end(), 10000 * RELEASE_MULTIPLIER, Coord{3, 3});

        auto coordRange1 = make_test_view_adapter<CS, Tag, Sized>(coords_vec);
        auto coordRange2 = make_test_view_adapter<CS, Tag, Sized>(coords_vec2);
        COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(
            CS == Common,
            "equal",
            range_type,
            [&]
            {
                assert(not STD::equal(
                    fwd(coordRange1.begin()),
                    coordRange1.end(),
                    coordRange2.begin(),
                    coordRange2.end()));
            });
        COMPARE_DURATION_TO_STD_RANGES(
            "equal_pred",
            range_type,
            [&]
            {
                auto pred = [](Coord a, Coord b)
                {
                    return a.x == b.y;
                };
                assert(STD::equal(coordRange1, coordRange2, pred));
            });
    }
    static constexpr bool do_test = true;
};
void test_equal()
{
    testSuite.test_case("sample");
    testSuite.test_case("range");
    /// [rah2::ranges::equal]
    RAH2_STD::vector<int> in1{1, 2, 3};
    RAH2_STD::vector<int> in2{1, 2, 3};
    RAH2_STD::vector<int> in3{11, 12, 13};
    assert(RAH2_NS::ranges::equal(in1, in2));
    assert(RAH2_NS::ranges::equal(in1, in3) == false);
    /// [rah2::ranges::equal]

    foreach_range_combination<test_algo<test_equal_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_lexicographical_compare_
{
    template <bool = true>
    void test()
    {
        // TODO : Test all ranges combinations ?
        {
            testSuite.test_case("range");
            testSuite.test_case("nocomp");
            testSuite.test_case("noproj");
            RAH2_STD::vector<char> v1{'a', 'b', 'c', 'd'};
            RAH2_STD::vector<char> v2{'a', 'x', 'y', 'z'};
            auto r1 = make_test_view_adapter<CS, Tag, Sized>(v1);
            auto r2 = make_test_view_adapter<CS, Tag, Sized>(v2);
            assert(RAH2_NS::ranges::lexicographical_compare(r1, r1) == false);
            assert(RAH2_NS::ranges::lexicographical_compare(r1, r2) == true);
            assert(RAH2_NS::ranges::lexicographical_compare(r2, r1) == false);

            testSuite.test_case("iter");
            assert(
                RAH2_NS::ranges::lexicographical_compare(r1.begin(), r1.end(), r1.begin(), r1.end())
                == false);
            assert(
                RAH2_NS::ranges::lexicographical_compare(r1.begin(), r1.end(), r2.begin(), r2.end())
                == true);
            assert(
                RAH2_NS::ranges::lexicographical_compare(r2.begin(), r2.end(), r1.begin(), r1.end())
                == false);
        }
        {
            testSuite.test_case("range");
            testSuite.test_case("comp");
            testSuite.test_case("proj");
            RAH2_STD::vector<Coord> v1{{1, 0}, {2, 0}, {3, 0}, {4, 0}};
            RAH2_STD::vector<Coord> v2{{0, 1}, {0, 3}, {0, 4}, {0, 5}};
            auto r1 = make_test_view_adapter<CS, Tag, Sized>(v1);
            auto r2 = make_test_view_adapter<CS, Tag, Sized>(v2);
            assert(
                RAH2_NS::ranges::lexicographical_compare(
                    r1, r1, [](auto a, auto b) { return a > b; }, &Coord::x, &Coord::y)
                == true);
            assert(
                RAH2_NS::ranges::lexicographical_compare(
                    r1, r2, [](auto a, auto b) { return a > b; }, &Coord::x, &Coord::y)
                == false);
            assert(
                RAH2_NS::ranges::lexicographical_compare(
                    r2, r1, [](auto a, auto b) { return a > b; }, &Coord::x, &Coord::y)
                == false);
        }
    }
    template <bool = true>
    void test_perf(char const* range_type)
    {
        testSuite.test_case("perf");
        RAH2_STD::vector<Coord> coords_vec;
        RAH2_STD::vector<Coord> coords_vec2;
        for (intptr_t i = 0; i < 1000000; ++i)
        {
            coords_vec.push_back(Coord{i, i});
            coords_vec2.push_back(Coord{i, i});
        }
        coords_vec.push_back({0, 1});
        coords_vec2.push_back({1, 0});

        auto coordRange1 = make_test_view_adapter<CS, Tag, Sized>(coords_vec);
        auto coordRange2 = make_test_view_adapter<CS, Tag, Sized>(coords_vec2);
        COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(
            CS == Common,
            "lexicographical_compare",
            range_type,
            [&]
            {
                assert(STD::lexicographical_compare(
                    fwd(coordRange1.begin()),
                    fwd(coordRange1.end()),
                    coordRange2.begin(),
                    coordRange2.end()));
            });
        COMPARE_DURATION_TO_STD_RANGES(
            "lexicographical_compare_pred",
            range_type,
            [&]
            {
                auto pred = [](Coord a, Coord b)
                {
                    return a.x < b.y;
                };
                assert(not STD::lexicographical_compare(coordRange1, coordRange2, pred));
            });
    }
    static constexpr bool do_test = true;
};
void test_lexicographical_compare()
{
    testSuite.test_case("sample");
    testSuite.test_case("range");
    /// [rah2::ranges::lexicographical_compare]
    RAH2_STD::vector<char> v1{'a', 'b', 'c', 'd'};
    RAH2_STD::vector<char> v2{'a', 'x', 'y', 'z'};
    assert(RAH2_NS::ranges::lexicographical_compare(v1, v1) == false);
    assert(RAH2_NS::ranges::lexicographical_compare(v1, v2) == true);
    assert(RAH2_NS::ranges::lexicographical_compare(v2, v1) == false);
    /// [rah2::ranges::lexicographical_compare]

    foreach_range_combination<test_algo<test_lexicographical_compare_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_find_
{
    template <bool = true>
    void test()
    {
        {
            RAH2_STD::vector<Coord> in{{1, 0}, {2, 0}, {3, 0}, {4, 0}};
            auto r1 = make_test_view_adapter<CS, Tag, Sized>(in);
            testSuite.test_case("range");
            testSuite.test_case("noproj");
            auto const iter = RAH2_NS::ranges::find(r1, Coord{3, 0});
            testSuite.test_case("iter");
            testSuite.test_case("proj");
            assert((*iter == Coord{3, 0}));
            auto const iter2 = RAH2_NS::ranges::find(r1.begin(), r1.end(), 3, &Coord::x);
            assert((*iter2 == Coord{3, 0}));
        }
    }
    template <bool = true>
    void test_perf(char const* range_type)
    {
        RAH2_STD::vector<Coord> in(1000000, Coord{1, 2});
        in.push_back(Coord{3, 4});
        auto r1 = make_test_view_adapter<CS, Tag, Sized>(in);

        COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(
            CS == Common,
            "find",
            range_type,
            [&]
            {
                auto iter = STD::find(fwd(r1.begin()), r1.end(), Coord{3, 4});
                assert((*iter == Coord{3, 4}));
            });
        COMPARE_DURATION_TO_STD_RANGES(
            "find_proj",
            range_type,
            [&]
            {
                auto iter = STD::find(r1.begin(), r1.end(), 3, &Coord::x);
                assert((*iter == Coord{3, 4}));
            });
    }
    static constexpr bool do_test = true;
};
void test_find()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::find]
    RAH2_STD::vector<int> in{1, 2, 3, 4};
    auto const iter = RAH2_NS::ranges::find(in, 3);
    assert(RAH2_NS::ranges::equal(
        RAH2_NS::ranges::make_subrange(iter, end(in)), std::initializer_list<int>({3, 4})));
    /// [rah2::ranges::find]

    foreach_range_combination<test_algo<test_find_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_find_if_
{
    template <bool = true>
    void test()
    {
        {
            RAH2_STD::vector<Coord> in{{1, 0}, {2, 0}, {3, 0}, {4, 0}};
            auto r1 = make_test_view_adapter<CS, Tag, Sized>(in);
            testSuite.test_case("range");
            testSuite.test_case("noproj");
            auto const iter = RAH2_NS::ranges::find_if(r1, [](auto c) { return c == Coord{3, 0}; });
            testSuite.test_case("iter");
            testSuite.test_case("proj");
            assert((*iter == Coord{3, 0}));
            auto const iter2 = RAH2_NS::ranges::find_if(
                r1.begin(), r1.end(), [](auto c) { return c == 3; }, &Coord::x);
            assert((*iter2 == Coord{3, 0}));
        }
    }
    template <bool = true>
    void test_perf(char const* range_type)
    {
        testSuite.test_case("perf");
        RAH2_STD::vector<Coord> in(1000000, Coord{1, 2});
        in.push_back(Coord{3, 4});
        auto r1 = make_test_view_adapter<CS, Tag, Sized>(in);

        COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(
            CS == Common,
            "find_if",
            range_type,
            [&]
            {
                auto iter =
                    STD::find_if(fwd(r1.begin()), r1.end(), [](auto c) { return c == Coord{3, 4}; });
                assert((*iter == Coord{3, 4}));
            });
        COMPARE_DURATION_TO_STD_RANGES(
            "find_if_proj",
            range_type,
            [&]
            {
                auto iter =
                    STD::find_if(r1.begin(), r1.end(), [](auto c) { return c == 3; }, &Coord::x);
                assert((*iter == Coord{3, 4}));
            });
    }
    static constexpr bool do_test = true;
};
void test_find_if()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::find_if]
    RAH2_STD::vector<int> in{1, 2, 3, 4};
    auto const iter = RAH2_NS::ranges::find_if(in, [](int i) { return i == 3; });
    assert(RAH2_NS::ranges::equal(
        RAH2_NS::ranges::make_subrange(iter, end(in)), std::initializer_list<int>({3, 4})));
    /// [rah2::ranges::find_if]

    foreach_range_combination<test_algo<test_find_if_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_find_if_not_
{
    template <bool = true>
    void test()
    {
        {
            RAH2_STD::vector<Coord> in{{1, 0}, {2, 0}, {3, 0}, {4, 0}};
            auto r1 = make_test_view_adapter<CS, Tag, Sized>(in);
            testSuite.test_case("range");
            testSuite.test_case("noproj");
            auto const iter =
                RAH2_NS::ranges::find_if_not(r1, [](auto c) { return c != Coord{3, 0}; });
            testSuite.test_case("iter");
            testSuite.test_case("proj");
            assert((*iter == Coord{3, 0}));
            auto const iter2 = RAH2_NS::ranges::find_if_not(
                r1.begin(), r1.end(), [](auto c) { return c != 3; }, &Coord::x);
            assert((*iter2 == Coord{3, 0}));
        }
    }
    template <bool = true>
    void test_perf(char const* range_type)
    {
        testSuite.test_case("perf");
        RAH2_STD::vector<Coord> in(1000000, Coord{1, 2});
        in.push_back(Coord{3, 4});
        auto r1 = make_test_view_adapter<CS, Tag, Sized>(in);

        COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(
            CS == Common,
            "find_if",
            range_type,
            [&]
            {
                auto iter = STD::find_if_not(
                    fwd(r1.begin()), r1.end(), [](auto c) { return c != Coord{3, 4}; });
                assert((*iter == Coord{3, 4}));
            });
        COMPARE_DURATION_TO_STD_RANGES(
            "find_if_proj",
            range_type,
            [&]
            {
                auto iter =
                    STD::find_if_not(r1.begin(), r1.end(), [](auto c) { return c != 3; }, &Coord::x);
                assert((*iter == Coord{3, 4}));
            });
    }
    static constexpr bool do_test = true;
};
void test_find_if_not()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::find_if_not]
    RAH2_STD::vector<int> in{1, 2, 3, 4};
    auto const iter = RAH2_NS::ranges::find_if_not(in, [](int i) { return i < 3; });
    assert(RAH2_NS::ranges::equal(
        RAH2_NS::ranges::make_subrange(iter, end(in)), std::initializer_list<int>({3, 4})));
    /// [rah2::ranges::find_if_not]

    foreach_range_combination<test_algo<test_find_if_not_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_find_last_
{
    template <bool = true>
    void test()
    {
        RAH2_STD::vector<Coord> in{{1, 0}, {2, 0}, {3, 0}, {4, 0}, {3, 0}, {2, 0}};
        auto r1 = make_test_view_adapter<CS, Tag, Sized>(in);
        testSuite.test_case("range");
        testSuite.test_case("noproj");
        auto const iter = RAH2_NS::ranges::find_last(r1, Coord{3, 0});
        testSuite.test_case("iter");
        testSuite.test_case("proj");
        assert((*RAH2_NS::ranges::begin(iter) == Coord{3, 0}));
        assert((RAH2_NS::ranges::distance(iter.begin(), iter.end()) == 2));
        auto const iter2 = RAH2_NS::ranges::find_last(r1.begin(), r1.end(), 3, &Coord::x);
        assert((*RAH2_NS::ranges::begin(iter2) == Coord{3, 0}));
        assert((RAH2_NS::ranges::distance(iter2.begin(), iter2.end()) == 2));
        auto const iter3 = RAH2_NS::ranges::find_last(r1.begin(), r1.end(), 45, &Coord::x);
        assert(RAH2_NS::ranges::empty(iter3));
    }
    template <bool = true>
    void test_perf(char const* range_type)
    {
        RAH2_STD::vector<Coord> in(1000000, Coord{1, 2});
        in.push_back(Coord{3, 4});
        in.push_back(Coord{3, 4});
        in.push_back(Coord{18, 4});
        auto r1 = make_test_view_adapter<CS, Tag, Sized>(in);
        (void)r1;

        {
            COMPARE_DURATION_TO_STD_RANGES_23(
                "find_last",
                range_type,
                [&]
                {
                    auto iter = STD::find_last(r1.begin(), r1.end(), Coord{3, 4});
                    assert((*RAH2_NS::ranges::begin(iter) == Coord{3, 4}));
                    assert((RAH2_NS::ranges::distance(iter.begin(), iter.end()) == 2));
                });
        }
        {
            COMPARE_DURATION_TO_STD_RANGES_23(
                "find_last_proj",
                range_type,
                [&]
                {
                    auto iter = STD::find_last(r1.begin(), r1.end(), 3, &Coord::x);
                    assert((*RAH2_NS::ranges::begin(iter) == Coord{3, 4}));
                    assert((RAH2_NS::ranges::distance(iter.begin(), iter.end()) == 2));
                });
        }
    }
    static constexpr bool do_test = RAH2_NS::derived_from<Tag, RAH2_NS::forward_iterator_tag>;
};
void test_find_last()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::find_last]
    static auto v = {1, 2, 3, 1, 2, 3, 1, 2};
    {
        auto const i1 = RAH2_NS::ranges::find_last(v.begin(), v.end(), 3);
        auto const i2 = RAH2_NS::ranges::find_last(v, 3);
        assert(RAH2_NS::ranges::distance(v.begin(), i1.begin()) == 5);
        assert(RAH2_NS::ranges::distance(v.begin(), i2.begin()) == 5);
    }
    {
        auto const i1 = RAH2_NS::ranges::find_last(v.begin(), v.end(), -3);
        auto const i2 = RAH2_NS::ranges::find_last(v, -3);
        assert(i1.begin() == v.end());
        assert(i2.begin() == v.end());
    }
    /// [rah2::ranges::find_last]

    foreach_range_combination<test_algo<test_find_last_>>();
}
template <CommonOrSent CS, typename Tag, bool Sized>
struct test_find_last_if_
{
    template <bool = true>
    void test()
    {
        RAH2_STD::vector<Coord> in{{1, 0}, {2, 0}, {3, 0}, {4, 0}, {3, 0}, {2, 0}};
        auto r1 = make_test_view_adapter<CS, Tag, Sized>(in);
        testSuite.test_case("range");
        testSuite.test_case("noproj");
        auto const iter =
            RAH2_NS::ranges::find_last_if(r1, [](Coord const& c) { return c == Coord{3, 0}; });
        testSuite.test_case("iter");
        testSuite.test_case("proj");
        assert((*RAH2_NS::ranges::begin(iter) == Coord{3, 0}));
        assert((RAH2_NS::ranges::distance(iter.begin(), iter.end()) == 2));
        auto const iter2 = RAH2_NS::ranges::find_last_if(
            r1.begin(), r1.end(), [](intptr_t c) { return c == 3; }, &Coord::x);
        assert((*RAH2_NS::ranges::begin(iter2) == Coord{3, 0}));
        assert((RAH2_NS::ranges::distance(iter2.begin(), iter2.end()) == 2));
        auto const iter3 = RAH2_NS::ranges::find_last_if(
            r1.begin(), r1.end(), [](intptr_t c) { return c == 45; }, &Coord::x);
        assert(RAH2_NS::ranges::empty(iter3));
    }
    template <bool = true>
    void test_perf(char const* range_type)
    {
        RAH2_STD::vector<Coord> in(1000000, Coord{1, 2});
        in.push_back(Coord{3, 4});
        in.push_back(Coord{3, 4});
        in.push_back(Coord{18, 4});
        auto r1 = make_test_view_adapter<CS, Tag, Sized>(in);
        (void)r1;

        {
            COMPARE_DURATION_TO_STD_RANGES_23( // find_last_if does not exist in std
                "find_last_if",
                range_type,
                [&]
                {
                    auto iter = STD::find_last_if(
                        r1.begin(), r1.end(), [](Coord const& c) { return c == Coord{3, 4}; });
                    assert((*RAH2_NS::ranges::begin(iter) == Coord{3, 4}));
                    assert((RAH2_NS::ranges::distance(iter.begin(), iter.end()) == 2));
                });
        }
        {
            COMPARE_DURATION_TO_STD_RANGES_23(
                "find_last_if_proj",
                range_type,
                [&]
                {
                    auto iter = STD::find_last_if(
                        r1.begin(), r1.end(), [](intptr_t c) { return c == 3; }, &Coord::x);
                    assert((*RAH2_NS::ranges::begin(iter) == Coord{3, 4}));
                    assert((RAH2_NS::ranges::distance(iter.begin(), iter.end()) == 2));
                });
        }
    }
    static constexpr bool do_test = RAH2_NS::derived_from<Tag, RAH2_NS::forward_iterator_tag>;
};
void test_find_last_if()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::find_last_if]
    static auto v = {1, 2, 3, 1, 2, 3, 1, 2};
    auto abs = [](int x)
    {
        return x < 0 ? -x : x;
    };
    {
        auto pred = [](int x)
        {
            return x == 3;
        };
        auto const i1 = RAH2_NS::ranges::find_last_if(v.begin(), v.end(), pred, abs);
        auto const i2 = RAH2_NS::ranges::find_last_if(v, pred, abs);
        assert(RAH2_NS::ranges::distance(v.begin(), i1.begin()) == 5);
        assert(RAH2_NS::ranges::distance(v.begin(), i2.begin()) == 5);
    }
    {
        auto pred = [](int x)
        {
            return x == -3;
        };
        auto const i1 = RAH2_NS::ranges::find_last_if(v.begin(), v.end(), pred, abs);
        auto const i2 = RAH2_NS::ranges::find_last_if(v, pred, abs);
        assert(i1.begin() == v.end());
        assert(i2.begin() == v.end());
    }
    /// [rah2::ranges::find_last_if]

    foreach_range_combination<test_algo<test_find_last_if_>>();
}
template <CommonOrSent CS, typename Tag, bool Sized>
struct test_find_last_if_not_
{
    template <bool = true>
    void test()
    {
        RAH2_STD::vector<Coord> in{{1, 0}, {2, 0}, {3, 0}, {4, 0}, {3, 0}, {2, 0}};
        auto r1 = make_test_view_adapter<CS, Tag, Sized>(in);
        testSuite.test_case("range");
        testSuite.test_case("noproj");
        auto const iter =
            RAH2_NS::ranges::find_last_if_not(r1, [](Coord const& c) { return c != Coord{3, 0}; });
        testSuite.test_case("iter");
        testSuite.test_case("proj");
        assert((*RAH2_NS::ranges::begin(iter) == Coord{3, 0}));
        assert((RAH2_NS::ranges::distance(iter.begin(), iter.end()) == 2));
        auto const iter2 = RAH2_NS::ranges::find_last_if_not(
            r1.begin(), r1.end(), [](intptr_t c) { return c != 3; }, &Coord::x);
        assert((*RAH2_NS::ranges::begin(iter2) == Coord{3, 0}));
        assert((RAH2_NS::ranges::distance(iter2.begin(), iter2.end()) == 2));
        auto const iter3 = RAH2_NS::ranges::find_last_if_not(
            r1.begin(), r1.end(), [](intptr_t c) { return c != 45; }, &Coord::x);
        assert(RAH2_NS::ranges::empty(iter3));
    }
    template <bool = true>
    void test_perf(char const* range_type)
    {
        RAH2_STD::vector<Coord> in(1000000, Coord{1, 2});
        in.push_back(Coord{3, 4});
        in.push_back(Coord{3, 4});
        in.push_back(Coord{18, 4});
        auto r1 = make_test_view_adapter<CS, Tag, Sized>(in);
        (void)r1;

        {
            COMPARE_DURATION_TO_STD_RANGES_23( // find_last_if_not does not exist in std
                "find_last_if_not",
                range_type,
                [&]
                {
                    auto iter = STD::find_last_if_not(
                        r1.begin(), r1.end(), [](Coord const& c) { return c != Coord{3, 4}; });
                    assert((*RAH2_NS::ranges::begin(iter) == Coord{3, 4}));
                    assert((RAH2_NS::ranges::distance(iter.begin(), iter.end()) == 2));
                });
        }
        {
            COMPARE_DURATION_TO_STD_RANGES_23(
                "find_last_if_not_proj",
                range_type,
                [&]
                {
                    auto iter = STD::find_last_if_not(
                        r1.begin(), r1.end(), [](intptr_t c) { return c != 3; }, &Coord::x);
                    assert((*RAH2_NS::ranges::begin(iter) == Coord{3, 4}));
                    assert((RAH2_NS::ranges::distance(iter.begin(), iter.end()) == 2));
                });
        }
    }
    static constexpr bool do_test = RAH2_NS::derived_from<Tag, RAH2_NS::forward_iterator_tag>;
};
void test_find_last_if_not()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::find_last_if_not]
    static auto v = {1, 2, 3, 1, 2, 3, 1, 2};
    auto abs = [](int x)
    {
        return x < 0 ? -x : x;
    };
    {
        auto pred = [](int x)
        {
            return x == 1 or x == 2;
        };
        auto const i1 = RAH2_NS::ranges::find_last_if_not(v.begin(), v.end(), pred, abs);
        auto const i2 = RAH2_NS::ranges::find_last_if_not(v, pred, abs);
        assert(RAH2_NS::ranges::distance(v.begin(), i1.begin()) == 5);
        assert(RAH2_NS::ranges::distance(v.begin(), i2.begin()) == 5);
    }
    {
        auto pred = [](int x)
        {
            return x == 1 or x == 2 or x == 3;
        };
        auto const i1 = RAH2_NS::ranges::find_last_if_not(v.begin(), v.end(), pred, abs);
        auto const i2 = RAH2_NS::ranges::find_last_if_not(v, pred, abs);
        assert(i1.begin() == v.end());
        assert(i2.begin() == v.end());
    }
    /// [rah2::ranges::find_last_if_not]

    foreach_range_combination<test_algo<test_find_last_if_not_>>();
}
template <CommonOrSent CS, typename Tag, bool Sized>
struct test_find_end_
{
    template <bool = true>
    void test()
    {
        using namespace std::literals;
        RAH2_STD::string secret_str{"password password word..."};
        RAH2_STD::string wanted_str{"password"};
        auto secret = make_test_view_adapter<CS, Tag, Sized>(secret_str);
        auto wanted = make_test_view_adapter<CS, Tag, Sized>(wanted_str);

        testSuite.test_case("iter");
        testSuite.test_case("noproj");
        auto const found1 =
            RAH2_NS::ranges::find_end(secret.begin(), secret.end(), wanted.begin(), wanted.end());
        assert(
            found1.begin()
            == RAH2_NS::ranges::next(secret.begin(), static_cast<intptr_t>(strlen("password "))));
        assert(
            found1.end()
            == RAH2_NS::ranges::next(
                secret.begin(), static_cast<intptr_t>(strlen("password password"))));

        testSuite.test_case("range");
        auto word_str = RAH2_STD::string("word");
        auto word = make_test_view_adapter<CS, Tag, Sized>(word_str);
        auto const found2 = RAH2_NS::ranges::find_end(secret, word);
        assert(
            found2.begin()
            == RAH2_NS::ranges::next(
                secret.begin(), static_cast<intptr_t>(strlen("password password "))));
        assert(
            found2.end()
            == RAH2_NS::ranges::next(
                secret.begin(), static_cast<intptr_t>(strlen("password password word"))));

        auto ord_str = RAH2_STD::string("ORD");
        auto ord = make_test_view_adapter<CS, Tag, Sized>(ord_str);
        auto const found3 = RAH2_NS::ranges::find_end(
            secret, ord, [](char const x, char const y) { // uses a binary predicate
                return std::tolower(x) == std::tolower(y);
            });
        assert(
            found3.begin()
            == RAH2_NS::ranges::next(
                secret.begin(), static_cast<intptr_t>(strlen("password password w"))));
        assert(
            found3.end()
            == RAH2_NS::ranges::next(
                secret.begin(), static_cast<intptr_t>(strlen("password password word"))));

        testSuite.test_case("proj");
        auto sword_str = RAH2_STD::string("SWORD");
        auto sword = make_test_view_adapter<CS, Tag, Sized>(sword_str);
        auto const found4 = RAH2_NS::ranges::find_end(
            secret, sword, {}, {}, [](char c) { return std::tolower(c); }); // projects the 2nd range
        assert(
            found4.begin()
            == RAH2_NS::ranges::next(secret.begin(), static_cast<intptr_t>(strlen("password pas"))));
        assert(
            found4.end()
            == RAH2_NS::ranges::next(
                secret.begin(), static_cast<intptr_t>(strlen("password password"))));

        testSuite.test_case("empty");
        auto pass_str = RAH2_STD::string("PASS");
        auto pass = make_test_view_adapter<CS, Tag, Sized>(pass_str);
        assert(RAH2_NS::ranges::find_end(secret, pass).empty()); // => not found
    }

    template <bool = true>
    void test_perf(char const* range_type)
    {
        RAH2_STD::vector<Coord> in(1000000 * RELEASE_MULTIPLIER, Coord{1, 2});
        in.push_back(Coord{3, 4});
        in.push_back(Coord{3, 4});
        in.push_back(Coord{18, 4});
        in.insert(in.end(), 1000001 * RELEASE_MULTIPLIER, Coord{0, 0});
        RAH2_STD::vector<Coord> search1;
        search1.push_back(Coord{3, 4});
        search1.push_back(Coord{3, 4});
        search1.push_back(Coord{18, 4});
        RAH2_STD::vector<Coord> search2;
        search2.push_back(Coord{0, 3});
        search2.push_back(Coord{0, 3});
        search2.push_back(Coord{0, 18});
        auto r1 = make_test_view_adapter<CS, Tag, Sized>(in);
        auto s1 = make_test_view_adapter<CS, Tag, Sized>(search1);
        auto s2 = make_test_view_adapter<CS, Tag, Sized>(search2);
        (void)s1;
        (void)s2;
        (void)r1;
        {
            COMPARE_DURATION_TO_STD_ALGO_17_AND_RANGES(
                CS == Common,
                "find_end",
                range_type,
                [&]
                {
                    auto iter = fwd(STD::find_end(fwd(r1.begin()), r1.end(), s1.begin(), s1.end()));
                    DONT_OPTIM(iter);
                });
        }

        #if RAH2_CPP20
        auto a = RAH2_STD::ranges::borrowed_subrange_t<
            test_view_adapter<CS, Tag, Sized, RAH2_STD::vector<Coord>>>();
        DONT_OPTIM(a);
        auto b = RAH2_STD::ranges::subrange<
            test_view_adapter<CS, Tag, Sized, RAH2_STD::vector<Coord>>::iterator>();
        DONT_OPTIM(b);
#endif

        {
            COMPARE_DURATION_TO_STD_RANGES(
                "find_end_proj",
                range_type,
                [&]
                {
                    auto iter = STD::find_end(
                        r1, s2, [](intptr_t a, intptr_t b) { return a == b; }, &Coord::x, &Coord::y);
                    assert((*RAH2_NS::ranges::begin(iter) == Coord{3, 4}));
                    assert((RAH2_NS::ranges::distance(iter.begin(), iter.end()) == 3));
                });
        }
    }
    static constexpr bool do_test = RAH2_NS::derived_from<Tag, RAH2_NS::forward_iterator_tag>;
};
void test_find_end()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::find_end]
    using namespace std::literals;
    RAH2_STD::string secret{"password password word..."};
    RAH2_STD::string wanted{"password"};

    auto const found1 =
        RAH2_NS::ranges::find_end(secret.begin(), secret.end(), wanted.begin(), wanted.end());
    assert(found1.begin() == secret.begin() + static_cast<intptr_t>(strlen("password ")));
    assert(found1.end() == secret.begin() + static_cast<intptr_t>(strlen("password password")));

    auto const found2 = RAH2_NS::ranges::find_end(secret, RAH2_STD::string("word"));
    assert(found2.begin() == secret.begin() + static_cast<intptr_t>(strlen("password password ")));
    assert(found2.end() == secret.begin() + static_cast<intptr_t>(strlen("password password word")));

    auto const found3 = RAH2_NS::ranges::find_end(
        secret, RAH2_STD::string("ORD"), [](char const x, char const y) { // uses a binary predicate
            return std::tolower(x) == std::tolower(y);
        });
    assert(found3.begin() == secret.begin() + static_cast<intptr_t>(strlen("password password w")));
    assert(found3.end() == secret.begin() + static_cast<intptr_t>(strlen("password password word")));

    auto const found4 = RAH2_NS::ranges::find_end(
        secret, RAH2_STD::string("SWORD"), {}, {}, [](char c) { return std::tolower(c); }); // projects the 2nd range
    assert(found4.begin() == secret.begin() + static_cast<intptr_t>(strlen("password pas")));
    assert(found4.end() == secret.begin() + static_cast<intptr_t>(strlen("password password")));

    assert(RAH2_NS::ranges::find_end(secret, RAH2_STD::string("PASS")).empty()); // => not found
    /// [rah2::ranges::find_end]

    foreach_range_combination<test_algo<test_find_end_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_find_first_of_
{
    using Tag2 = RAH2_NS::ranges::details::max_iterator_tag<Tag, RAH2_NS::forward_iterator_tag>;
    template <bool = true>
    void test()
    {
        {
            std::vector<int> v1{0, 2, 3, 25, 5};
            std::vector<int> v2 = {19, 10, 3, 4};
            std::vector<int> v3 = {1, 6, 7, 9};
            auto r = make_test_view_adapter<CS, Tag, Sized>(v1);
            auto t1 = make_test_view_adapter<CS, Tag2, Sized>(v2);
            auto t2 = make_test_view_adapter<CS, Tag2, Sized>(v3);

            testSuite.test_case("iter");
            testSuite.test_case("noproj");
            auto const found1 =
                RAH2_NS::ranges::find_first_of(r.begin(), r.end(), t1.begin(), t1.end());
            assert(found1 != r.end());
            assert(*found1 == 3);
            auto const found2 =
                RAH2_NS::ranges::find_first_of(r.begin(), r.end(), t2.begin(), t2.end());
            assert(found2 == r.end());
        }

        {
            std::vector<Coord> v1 = {{0, 0}, {2, 0}, {3, 0}, {25, 0}, {5, 0}};
            std::vector<Coord> v2 = {{1, 19}, {1, 10}, {1, 3}, {1, 4}};
            auto r = make_test_view_adapter<CS, Tag, Sized>(v1);
            auto t1 = make_test_view_adapter<CS, Tag2, Sized>(v2);

            testSuite.test_case("range");
            testSuite.test_case("proj");
            auto const found1 = RAH2_NS::ranges::find_first_of(
                r, t1, [](auto a, auto b) { return a == b; }, &Coord::x, &Coord::y);
            assert(found1 != r.end());
            assert((*found1).x == 3);
            auto const found2 = RAH2_NS::ranges::find_first_of(
                r, t1, [](auto a, auto b) { return a == b; }, &Coord::x, &Coord::x);
            assert(found2 == r.end());
        }
    }

    template <bool = true>
    void test_perf(char const* range_type)
    {
        std::vector<Coord> v1 = {{0, 0}, {2, 0}, {3, 0}, {25, 0}, {5, 0}};
        v1.insert(v1.begin(), 1000000 * RELEASE_MULTIPLIER, {0, 0});
        std::vector<Coord> v2 = {{1, 19}, {1, 10}, {1, 3}, {1, 4}};
        std::vector<Coord> v3 = {{3, 19}, {25, 0}, {3, 0}, {3, 4}};
        auto r = make_test_view_adapter<CS, Tag, Sized>(v1);
        auto t1 = make_test_view_adapter<CS, Tag2, Sized>(v2);
        auto t2 = make_test_view_adapter<CS, Tag2, Sized>(v3);
        (void)r;
        (void)t1;
        (void)t2;

        {
            COMPARE_DURATION_TO_STD_ALGO_17_AND_RANGES(
                CS == Common,
                "find_first_of",
                range_type,
                [&]
                {
                    auto const found1 =
                        STD::find_first_of(fwd(r.begin()), r.end(), t2.begin(), t2.end());
                    assert(found1 != r.end());
                    assert(*found1 == (Coord{3, 0}));
                    auto const found2 =
                        RAH2_NS::ranges::find_first_of(r.begin(), r.end(), t1.begin(), t1.end());
                    assert(found2 == r.end());
                });
        }
        {
            COMPARE_DURATION_TO_STD_RANGES(
                "find_first_of_proj",
                range_type,
                [&]
                {
                    auto const found1 = STD::find_first_of(
                        r, t1, [](auto a, auto b) { return a == b; }, &Coord::x, &Coord::y);
                    assert(found1 != r.end());
                    assert((*found1).x == 3);
                    auto const found2 = STD::find_first_of(
                        r, t1, [](auto a, auto b) { return a == b; }, &Coord::x, &Coord::x);
                    assert(found2 == r.end());
                });
        }
    }
    static constexpr bool do_test = true;
};
void test_find_first_of()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::find_first_of]
    static auto haystack = {1, 2, 3, 4};
    static auto needles = {0, 3, 4, 3};

    auto const found1 = RAH2_NS::ranges::find_first_of(
        haystack.begin(), haystack.end(), needles.begin(), needles.end());
    assert(RAH2_STD::distance(haystack.begin(), found1) == 2);

    auto const found2 = RAH2_NS::ranges::find_first_of(haystack, needles);
    assert(RAH2_STD::distance(haystack.begin(), found2) == 2);

    static auto negatives = {-6, -3, -4, -3};
    auto const not_found = RAH2_NS::ranges::find_first_of(haystack, negatives);
    assert(not_found == haystack.end());

    auto const found3 = RAH2_NS::ranges::find_first_of(
        haystack, negatives, [](int x, int y) { return x == -y; }); // uses a binary comparator
    assert(RAH2_STD::distance(haystack.begin(), found3) == 2);

    struct P
    {
        int x, y;
    };
    static auto p1 = {P{1, -1}, P{2, -2}, P{3, -3}, P{4, -4}};
    static auto p2 = {P{5, -5}, P{6, -3}, P{7, -5}, P{8, -3}};

    // Compare only P::y data members by projecting them:
    auto const found4 =
        RAH2_NS::ranges::find_first_of(p1, p2, [](auto a, auto b) { return a.y == b.y; });
    assert(RAH2_STD::distance(p1.begin(), found4) == 2); // {3, -3}
    /// [rah2::ranges::find_first_of]

    foreach_range_combination<test_algo<test_find_first_of_>>();
}
