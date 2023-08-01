#include <rah2/ranges.hpp>
#include <rah2/algorithm.hpp>

#include "test_helpers.hpp"

#include <array>
#include <complex>
#include <list>
#include <algorithm>
#include <numeric>
#include <forward_list>
#include <set>
#include <cstring>
#if RAH2_CPP20
#include <ranges>
#endif

void test_all_of()
{
    testSuite.test_case("sample");
    testSuite.test_case("range");
    testSuite.test_case("yes");
    /// [rah2::ranges::all_of]
    assert(rah2::ranges::all_of(
        std::initializer_list<int>{4, 4, 4, 4}, [](auto a) { return a == 4; }));
    /// [rah2::ranges::all_of]
    testSuite.test_case("no");
    std::vector<int> vec = {4, 4, 3, 4};
    testSuite.test_case("iter");
    assert(rah2::ranges::all_of(vec.begin(), vec.end(), [](auto a) { return a == 4; }) == false);
}
void test_any_of()
{
    testSuite.test_case("sample");
    testSuite.test_case("yes");
    testSuite.test_case("range");
    /// [rah2::ranges::any_of]
    assert(rah2::ranges::any_of(
        std::initializer_list<int>{3, 0, 1, 3, 4, 6}, [](auto a) { return a == 3; }));
    /// [rah2::ranges::any_of]
    testSuite.test_case("no");
    testSuite.test_case("iter");
    std::vector<int> vec = {3, 0, 1, 3, 4, 6};
    assert(rah2::ranges::any_of(vec.begin(), vec.end(), [](auto a) { return a == 5; }) == false);
}
void test_none_of()
{
    testSuite.test_case("sample");
    testSuite.test_case("yes");
    testSuite.test_case("range");
    /// [rah2::ranges::none_of]
    assert((rah2::ranges::none_of(
        std::initializer_list<int>{7, 8, 9, 10}, [](auto a) { return a == 11; })));
    /// [rah2::ranges::none_of]
    testSuite.test_case("no");
    testSuite.test_case("iter");
    std::vector<int> vec = {7, 8, 9, 10};
    assert(rah2::ranges::none_of(vec.begin(), vec.end(), [](auto a) { return a == 9; }) == false);
}
void test_for_each()
{
    testSuite.test_case("sample");
    testSuite.test_case("range");
    /// [rah2::ranges::for_each]
    std::vector<int> testFE{4, 4, 4, 4};
    rah2::ranges::for_each(testFE, [](auto& value) { return ++value; });
    assert(rah2::ranges::equal(testFE, std::initializer_list<int>({5, 5, 5, 5})));
    /// [rah2::ranges::for_each]
    testSuite.test_case("iter");
    rah2::ranges::for_each(testFE.begin(), testFE.end(), [](auto& value) { return ++value; });
    assert(rah2::ranges::equal(testFE, std::initializer_list<int>({6, 6, 6, 6})));
}
void test_for_each_n()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::for_each_n]
    std::vector<int> testFE{4, 4, 4, 4};
    auto const res = rah2::ranges::for_each_n(
        testFE.begin(), static_cast<intptr_t>(testFE.size()), [](auto& value) { return ++value; });
    assert(rah2::ranges::equal(testFE, std::initializer_list<int>({5, 5, 5, 5})));
    assert(res.in == testFE.end());
    /// [rah2::ranges::for_each_n]
}

void test_algo_count()
{
    testSuite.test_case("sample");
    testSuite.test_case("noproj");
    testSuite.test_case("proj");
    testSuite.test_case("range");
    {
        /// [rah2::ranges::count]
        assert(rah2::ranges::count(std::initializer_list<int>{4, 4, 4, 3}, 3) == 1);
        struct Coord
        {
            int x;
            int y;
        };
        static auto coords = {Coord{1, 1}, {1, 2}, {2, 1}, {2, 3}, {3, 1}};
        assert(rah2::ranges::count(coords, 1, RAH2_STD::mem_fn(&Coord::x)) == 2);
        /// [rah2::ranges::count]

        testSuite.test_case("iter");
        assert(rah2::ranges::count(coords.begin(), coords.end(), 1, RAH2_STD::mem_fn(&Coord::x)) == 2);
    }

    {
        testSuite.test_case("perf");
        struct Coord
        {
            int x;
            int y;
        };
        std::vector<Coord> coordsVec;
        coordsVec.insert(coordsVec.end(), 10000000, {1, 47});
        coordsVec.insert(coordsVec.end(), 79, {2, 47});
        coordsVec.insert(coordsVec.end(), 10000000, {3, 47});
        std::vector<int> intsVec;
        intsVec.insert(intsVec.end(), 10000000, 1);
        intsVec.insert(intsVec.end(), 79, 2);
        intsVec.insert(intsVec.end(), 10000000, 3);

        auto const value = 2;
        auto getter_lbd = [](Coord const& c)
        {
            return c.x;
        };

        auto const count_rah_noproj = COMPUTE_DURATION(
            "count_rah_noproj",
            [&]
            {
                const auto count =
                    rah2::ranges::count(intsVec.data(), intsVec.data() + intsVec.size(), 2);
                assert(count == 79);
            });
        auto const count_raw_noproj = COMPUTE_DURATION(
            "count_raw_noproj",
            [&]
            {
                size_t count = 0;
                auto i = intsVec.data();
                auto e = intsVec.data() + intsVec.size();
                for (; i != e; ++i)
                {
                    if (*i == value)
                        ++count;
                }
                assert(count == 79);
            });
        assert(count_rah_noproj < count_raw_noproj * 1.2);
        auto const count_std_noproj = COMPUTE_DURATION(
            "count_std_noproj",
            [&]
            {
                const auto count = std::count(intsVec.data(), intsVec.data() + intsVec.size(), 2);
                assert(count == 79);
            });
        assert(count_rah_noproj < count_std_noproj * 1.2);
#if RAH2_CPP20
        const auto count_rng_noproj = COMPUTE_DURATION(
            "count_rng_noproj",
            [&]
            {
                const size_t count =
                    rah2::ranges::count(intsVec.data(), intsVec.data() + intsVec.size(), 2);
                assert(count == 79);
            });
        assert(count_rah_noproj < count_rng_noproj * 1.2);
#endif
        auto const count_rah_proj = COMPUTE_DURATION(
            "count_rah_proj",
            [&]
            {
                const auto count = rah2::ranges::count(
                    coordsVec.data(), coordsVec.data() + coordsVec.size(), 2, getter_lbd);
                assert(count == 79);
            });
        auto const count_raw_proj = COMPUTE_DURATION(
            "count_raw_proj",
            [&]
            {
                size_t count = 0;
                auto i = coordsVec.data();
                auto e = coordsVec.data() + coordsVec.size();
                for (; i != e; ++i)
                {
                    if (getter_lbd(*i) == value)
                        ++count;
                }
                assert(count == 79);
            });
        assert(count_rah_proj < count_raw_proj * 1.2);

        auto const count_rgn_proj = COMPUTE_DURATION(
            "count_rgn_proj",
            [&]
            {
                const auto count = rah2::ranges::count(
                    coordsVec.data(),
                    coordsVec.data() + coordsVec.size(),
                    2,
                    [](Coord const& c) { return c.x; });
                assert(count == 79);
            });
        assert(count_rah_proj < count_rgn_proj * 1.2);
    }
}
void test_count_if()
{
    testSuite.test_case("sample");
    testSuite.test_case("range");
    /// [rah2::ranges::count_if]
    std::vector<int> vec = {4, 4, 4, 3};
    assert(rah2::ranges::count_if(vec, [](auto a) { return a == 4; }) == 3);
    /// [rah2::ranges::count_if]

    testSuite.test_case("iter");
    assert(rah2::ranges::count_if(vec.begin(), vec.end(), [](auto a) { return a == 4; }) == 3);

    {
        testSuite.test_case("perf");
        struct coord
        {
            int x;
            int y;
        };
        std::vector<coord> coords_vec;
        coords_vec.insert(coords_vec.end(), 10000000, {1, 47});
        coords_vec.insert(coords_vec.end(), 79, {2, 47});
        coords_vec.insert(coords_vec.end(), 10000000, {3, 47});
        auto pred = [](coord const& c)
        {
            return c.x == 2;
        };

        auto const count_if_rah_pred = COMPUTE_DURATION(
            "count_if_rah_pred",
            [&]
            {
                const auto count = rah2::ranges::count_if(
                    coords_vec.data(), coords_vec.data() + coords_vec.size(), pred);
                assert(count == 79);
            });

        auto const count_if_raw_pred = COMPUTE_DURATION(
            "count_if_raw_pred",
            [&]
            {
                size_t count = 0;
                auto i = coords_vec.data();
                auto e = coords_vec.data() + coords_vec.size();
                for (; i != e; ++i)
                {
                    if (pred(*i))
                        ++count;
                }
                assert(count == 79);
            });
        assert(count_if_rah_pred < count_if_raw_pred * 1.2);

        auto const count_if_std_pred = COMPUTE_DURATION(
            "count_if_std_pred",
            [&]
            {
                const auto count =
                    std::count_if(coords_vec.data(), coords_vec.data() + coords_vec.size(), pred);
                assert(count == 79);
            });
        assert(count_if_rah_pred < count_if_std_pred * 1.2);
    }
}
void test_mismatch()
{
    testSuite.test_case("sample");
    testSuite.test_case("range");
    /// [rah2::ranges::mismatch]
    std::vector<int> in1 = {1, 2, 3, 4};
    std::vector<int> in2 = {1, 2, 42, 43};
    auto r1_r2 = rah2::ranges::mismatch(in1, in2);
    assert(*r1_r2.in1 == 3);
    assert(*r1_r2.in2 == 42);
    /// [rah2::ranges::mismatch]

    testSuite.test_case("iter");
    r1_r2 = rah2::ranges::mismatch(in1.begin(), in1.end(), in2.begin(), in2.end());
    assert(*r1_r2.in1 == 3);
    assert(*r1_r2.in2 == 42);
}
void test_equal()
{
    testSuite.test_case("sample");
    testSuite.test_case("range");
    /// [rah2::ranges::equal]
    std::vector<int> in1{1, 2, 3};
    std::vector<int> in2{1, 2, 3};
    std::vector<int> in3{11, 12, 13};
    assert(rah2::ranges::equal(in1, in2));
    assert(rah2::ranges::equal(in1, in3) == false);
    /// [rah2::ranges::equal]

    testSuite.test_case("iter");
    assert(rah2::ranges::equal(in1.begin(), in1.end(), in2.begin(), in2.end()));
    assert(rah2::ranges::equal(in1.begin(), in1.end(), in3.begin(), in3.end()) == false);
}
void test_lexicographical_compare()
{
    testSuite.test_case("sample");
    testSuite.test_case("range");
    /// [rah2::ranges::lexicographical_compare]
    std::vector<char> v1{'a', 'b', 'c', 'd'};
    std::vector<char> v2{'a', 'x', 'y', 'z'};
    assert(rah2::ranges::lexicographical_compare(v1, v1) == false);
    assert(rah2::ranges::lexicographical_compare(v1, v2) == true);
    assert(rah2::ranges::lexicographical_compare(v2, v1) == false);
    /// [rah2::ranges::lexicographical_compare]

    testSuite.test_case("iter");
    assert(rah2::ranges::lexicographical_compare(v1.begin(), v1.end(), v1.begin(), v1.end()) == false);
    assert(rah2::ranges::lexicographical_compare(v1.begin(), v1.end(), v2.begin(), v2.end()) == true);
    assert(rah2::ranges::lexicographical_compare(v2.begin(), v2.end(), v1.begin(), v1.end()) == false);
}
void test_find()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::find]
    std::vector<int> in{1, 2, 3, 4};
    auto const iter = rah2::ranges::find(in, 3);
    assert(rah2::ranges::equal(
        rah2::ranges::make_subrange(iter, end(in)), std::initializer_list<int>({3, 4})));
    /// [rah2::ranges::find]
}
void test_find_if()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::find_if]
    std::vector<int> in{1, 2, 3, 4};
    auto const iter = rah2::ranges::find_if(in, [](int i) { return i == 3; });
    assert(rah2::ranges::equal(
        rah2::ranges::make_subrange(iter, end(in)), std::initializer_list<int>({3, 4})));
    /// [rah2::ranges::find_if]
}
void test_find_if_not()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::find_if_not]
    std::vector<int> in{1, 2, 3, 4};
    auto const iter = rah2::ranges::find_if_not(in, [](int i) { return i < 3; });
    assert(rah2::ranges::equal(
        rah2::ranges::make_subrange(iter, end(in)), std::initializer_list<int>({3, 4})));
    /// [rah2::ranges::find_if_not]
}
void test_find_last()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::find_last]
    static auto v = {1, 2, 3, 1, 2, 3, 1, 2};
    {
        auto const i1 = rah2::ranges::find_last(v.begin(), v.end(), 3);
        auto const i2 = rah2::ranges::find_last(v, 3);
        assert(rah2::ranges::distance(v.begin(), i1.begin()) == 5);
        assert(rah2::ranges::distance(v.begin(), i2.begin()) == 5);
    }
    {
        auto const i1 = rah2::ranges::find_last(v.begin(), v.end(), -3);
        auto const i2 = rah2::ranges::find_last(v, -3);
        assert(i1.begin() == v.end());
        assert(i2.begin() == v.end());
    }
    /// [rah2::ranges::find_last]
}
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
        auto const i1 = rah2::ranges::find_last_if(v.begin(), v.end(), pred, abs);
        auto const i2 = rah2::ranges::find_last_if(v, pred, abs);
        assert(rah2::ranges::distance(v.begin(), i1.begin()) == 5);
        assert(rah2::ranges::distance(v.begin(), i2.begin()) == 5);
    }
    {
        auto pred = [](int x)
        {
            return x == -3;
        };
        auto const i1 = rah2::ranges::find_last_if(v.begin(), v.end(), pred, abs);
        auto const i2 = rah2::ranges::find_last_if(v, pred, abs);
        assert(i1.begin() == v.end());
        assert(i2.begin() == v.end());
    }
    /// [rah2::ranges::find_last_if]
}
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
        auto const i1 = rah2::ranges::find_last_if_not(v.begin(), v.end(), pred, abs);
        auto const i2 = rah2::ranges::find_last_if_not(v, pred, abs);
        assert(rah2::ranges::distance(v.begin(), i1.begin()) == 5);
        assert(rah2::ranges::distance(v.begin(), i2.begin()) == 5);
    }
    {
        auto pred = [](int x)
        {
            return x == 1 or x == 2 or x == 3;
        };
        auto const i1 = rah2::ranges::find_last_if_not(v.begin(), v.end(), pred, abs);
        auto const i2 = rah2::ranges::find_last_if_not(v, pred, abs);
        assert(i1.begin() == v.end());
        assert(i2.begin() == v.end());
    }
    /// [rah2::ranges::find_last_if_not]
}
void test_find_end()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::find_end]
    using namespace std::literals;
    std::string secret{"password password word..."};
    std::string wanted{"password"};

    auto const found1 =
        rah2::ranges::find_end(secret.begin(), secret.end(), wanted.begin(), wanted.end());
    assert(found1.begin() == secret.begin() + static_cast<intptr_t>(strlen("password ")));
    assert(found1.end() == secret.begin() + static_cast<intptr_t>(strlen("password password")));

    auto const found2 = rah2::ranges::find_end(secret, std::string("word"));
    assert(found2.begin() == secret.begin() + static_cast<intptr_t>(strlen("password password ")));
    assert(found2.end() == secret.begin() + static_cast<intptr_t>(strlen("password password word")));

    auto const found3 = rah2::ranges::find_end(
        secret, std::string("ORD"), [](char const x, char const y) { // uses a binary predicate
            return std::tolower(x) == std::tolower(y);
        });
    assert(found3.begin() == secret.begin() + static_cast<intptr_t>(strlen("password password w")));
    assert(found3.end() == secret.begin() + static_cast<intptr_t>(strlen("password password word")));

    auto const found4 = rah2::ranges::find_end(
        secret, std::string("SWORD"), {}, {}, [](char c) { return std::tolower(c); }); // projects the 2nd range
    assert(found4.begin() == secret.begin() + static_cast<intptr_t>(strlen("password pas")));
    assert(found4.end() == secret.begin() + static_cast<intptr_t>(strlen("password password")));

    assert(rah2::ranges::find_end(secret, std::string("PASS")).empty()); // => not found
    /// [rah2::ranges::find_end]
}
void test_find_first_of()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::find_first_of]
    static auto haystack = {1, 2, 3, 4};
    static auto needles = {0, 3, 4, 3};

    auto const found1 = rah2::ranges::find_first_of(
        haystack.begin(), haystack.end(), needles.begin(), needles.end());
    assert(std::distance(haystack.begin(), found1) == 2);

    auto const found2 = rah2::ranges::find_first_of(haystack, needles);
    assert(std::distance(haystack.begin(), found2) == 2);

    static auto negatives = {-6, -3, -4, -3};
    auto const not_found = rah2::ranges::find_first_of(haystack, negatives);
    assert(not_found == haystack.end());

    auto const found3 = rah2::ranges::find_first_of(
        haystack, negatives, [](int x, int y) { return x == -y; }); // uses a binary comparator
    assert(std::distance(haystack.begin(), found3) == 2);

    struct P
    {
        int x, y;
    };
    static auto p1 = {P{1, -1}, P{2, -2}, P{3, -3}, P{4, -4}};
    static auto p2 = {P{5, -5}, P{6, -3}, P{7, -5}, P{8, -3}};

    // Compare only P::y data members by projecting them:
    auto const found4 =
        rah2::ranges::find_first_of(p1, p2, [](auto a, auto b) { return a.y == b.y; });
    assert(std::distance(p1.begin(), found4) == 2); // {3, -3}
    /// [rah2::ranges::find_first_of]
}
void test_adjacent_find()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::adjacent_find]
    auto const v = {0, 1, 2, 3, 40, 40, 41, 41, 5}; /*
                                                ^^          ^^       */
    {
        auto const it = rah2::ranges::adjacent_find(v.begin(), v.end());
        assert(rah2::ranges::distance(v.begin(), it) == 4);
    }

    {
        auto const it = rah2::ranges::adjacent_find(v, rah2::greater());
        assert(rah2::ranges::distance(v.begin(), it) == 7);
    }
    /// [rah2::ranges::adjacent_find]
}
void test_search()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::search]
    std::string haystack{"abcd abcd"};
    std::string needle{"bcd"};

    // the search uses iterator pairs begin()/end():
    auto const found1 =
        rah2::ranges::search(haystack.begin(), haystack.end(), needle.begin(), needle.end());
    assert(found1.begin() - haystack.begin() == 1);
    assert(found1.end() - haystack.begin() == 4);

    // the search uses ranges r1, r2:
    auto const found2 = rah2::ranges::search(haystack, needle);
    assert(found2.begin() - haystack.begin() == 1);
    assert(found2.end() - haystack.begin() == 4);
    // print(2, haystack, needle, found2);

    // 'needle' range is empty:
    std::string none;
    auto const found3 = rah2::ranges::search(haystack, none);
    assert(found3.begin() - haystack.begin() == 0);
    assert(found3.end() - haystack.begin() == 0);
    // print(3, haystack, none, found3);

    // 'needle' will not be found:
    std::string awl{"efg"};
    auto const found4 = rah2::ranges::search(haystack, awl);
    assert(found4.begin() - haystack.begin() == 9);
    assert(found4.end() - haystack.begin() == 9);
    // print(4, haystack, awl, found4);

    // the search uses custom comparator and projections:
    std::string bodkin{"234"};
    auto const found5 = rah2::ranges::search(
        haystack,
        bodkin,
        [](int const x, int const y) { return x == y; }, // pred
        [](int const x) { return std::toupper(x); }, // proj1
        [](int const y) { return y + 'A' - '1'; }); // proj2
    assert(found5.begin() - haystack.begin() == 1);
    assert(found5.end() - haystack.begin() == 4);
    // print(5, haystack, bodkin, found5);
    /// [rah2::ranges::search]
}
void test_search_n()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::search_n]
    auto nums = {1, 2, 2, 3, 4, 1, 2, 2, 2, 1};
    constexpr int count{3};
    constexpr int value{2};
    using count_t = int;
    using value_t = int;

    auto result1 = rah2::ranges::search_n(nums.begin(), nums.end(), count, value);
    assert( // found
        result1.size() == count && std::distance(nums.begin(), result1.begin()) == 6
        && std::distance(nums.begin(), result1.end()) == 9);

    auto result2 = rah2::ranges::search_n(nums, count, value);
    assert( // found
        result2.size() == count && std::distance(nums.begin(), result2.begin()) == 6
        && std::distance(nums.begin(), result2.end()) == 9);

    auto result3 = rah2::ranges::search_n(nums, count, value_t{5});
    assert( // not found
        result3.size() == 0 && result3.begin() == result3.end() && result3.end() == nums.end());

    auto result4 = rah2::ranges::search_n(nums, count_t{0}, value_t{1});
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

    auto const result5 = rah2::ranges::search_n(nums, count, symbol, is_equ, to_ascii);
    assert(result5.begin() - nums.begin() == 6);
    /// [rah2::ranges::search_n]
}
void test_contains()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::contains]
    constexpr auto haystack = std::array<int, 5>{3, 1, 4, 1, 5};
    auto increment = [](int x)
    {
        return ++x;
    };

    assert(rah2::ranges::contains(haystack, 4));
    assert(not rah2::ranges::contains(haystack, 6));
    assert(rah2::ranges::contains(haystack, 6, increment));
    assert(not rah2::ranges::contains(haystack, 1, increment));
    /// [rah2::ranges::contains]
}
void test_contains_subrange()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::contains_subrange]
    constexpr auto haystack = std::array<int, 5>{3, 1, 4, 1, 5};
    constexpr auto needle = std::array<int, 3>{1, 4, 1};
    constexpr auto bodkin = std::array<int, 3>{2, 5, 2};
    auto increment = [](int x)
    {
        return ++x;
    };
    auto decrement = [](int x)
    {
        return --x;
    };

    assert(rah2::ranges::contains_subrange(haystack, needle));
    assert(not rah2::ranges::contains_subrange(haystack, bodkin));
    assert(rah2::ranges::contains_subrange(haystack, bodkin, {}, increment));
    assert(not rah2::ranges::contains_subrange(haystack, bodkin, {}, decrement));
    assert(rah2::ranges::contains_subrange(haystack, bodkin, {}, {}, decrement));
    /// [rah2::ranges::contains_subrange]
}
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

    assert(rah2::ranges::starts_with("const_cast", std::string("const")));
    assert(rah2::ranges::starts_with("constexpr", std::string("const")));
    assert(!rah2::ranges::starts_with("volatile", std::string("const")));

    assert(rah2::ranges::starts_with(
        "Constantinopolis", std::string("constant"), {}, ascii_upper, ascii_upper));
    assert(not rah2::ranges::starts_with(
        "Istanbul", std::string("constant"), {}, ascii_upper, ascii_upper));
    assert(rah2::ranges::starts_with("Metropolis", std::string("metro"), cmp_ignore_case));
    assert(not rah2::ranges::starts_with("Acropolis", std::string("metro"), cmp_ignore_case));

    auto v = {1, 3, 5, 7, 9};
    auto odd = [](int x)
    {
        return x % 2;
    };
    assert(rah2::ranges::starts_with(
        v,
        rah2::ranges::views::iota(1) | rah2::ranges::views::filter(odd)
            | rah2::ranges::views::take(3)));
    /// [rah2::ranges::starts_with]

    testSuite.test_case("iter");
    std::string acropolis = "Acropolis";
    std::string metro = "metro";
    assert(not rah2::ranges::starts_with(
        acropolis.begin(), acropolis.end(), metro.begin(), metro.end(), cmp_ignore_case));
}
void test_ends_with()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::ends_with]
    assert(rah2::ranges::ends_with("static_cast", "cast"));
    assert(rah2::ranges::ends_with("const_cast", "cast"));
    assert(rah2::ranges::ends_with("reinterpret_cast", "cast"));
    assert(rah2::ranges::ends_with("dynamic_cast", "cast"));
    assert(not rah2::ranges::ends_with("move", "cast"));
    assert(not rah2::ranges::ends_with("move_if_noexcept", "cast"));
    assert(not rah2::ranges::ends_with("forward", "cast"));
    assert(!rah2::ranges::ends_with("as_const", "cast"));
    assert(!!rah2::ranges::ends_with("bit_cast", "cast"));
    assert(!rah2::ranges::ends_with("to_underlying", "cast"));
    assert(!!rah2::ranges::ends_with(std::vector<int>{1, 2, 3, 4}, std::vector<int>{3, 4}));
    assert(!rah2::ranges::ends_with(std::vector<int>{1, 2, 3, 4}, std::vector<int>{4, 5}));
    /// [rah2::ranges::ends_with]
}
void test_copy()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::copy]
    std::vector<int> in{1, 2, 3};
    std::vector<int> out{0, 0, 0, 4, 5};
    assert(rah2::ranges::equal(
        rah2::ranges::make_subrange(rah2::ranges::copy(in, out.begin()).out, end(out)),
        std::initializer_list<int>({4, 5})));
    assert(out == (std::vector<int>{1, 2, 3, 4, 5}));
    /// [rah2::ranges::copy]
}
void test_copy_if()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::copy_if]
    std::vector<int> in{1, 2, 3, 4};
    std::vector<int> out{0, 0, 5, 6};
    assert(rah2::ranges::equal(
        rah2::ranges::make_subrange(
            rah2::ranges::copy_if(in, out.begin(), [](int i) { return i % 2 == 0; }).out, end(out)),
        std::initializer_list<int>({5, 6})));
    assert(out == (std::vector<int>{2, 4, 5, 6}));
    /// [rah2::ranges::copy_if]
}
void test_copy_n()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::copy_n]
    std::string const in{"ABCDEFGH"};
    std::string out;

    rah2::ranges::copy_n(in.begin(), 4, std::back_inserter(out));
    assert(out == "ABCD");

    out = "abcdefgh";
    auto const res = rah2::ranges::copy_n(in.begin(), 5, out.begin());
    assert(*(res.in) == 'F');
    assert(*(res.out) == 'f');
    assert(std::distance(std::begin(in), res.in) == 5);
    assert(std::distance(std::begin(out), res.out) == 5);
    /// [rah2::ranges::copy_n]
}
void test_copy_backward()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::copy_backward]
    auto const src = {1, 2, 3, 4};

    std::vector<int> dst(src.size() + 2);
    rah2::ranges::copy_backward(src, dst.end());
    assert(dst == (std::vector<int>{0, 0, 1, 2, 3, 4}));

    rah2::ranges::fill(dst, 0);
    auto const in_out = rah2::ranges::copy_backward(src.begin(), src.end() - 2, dst.end());
    assert(dst == (std::vector<int>{0, 0, 0, 0, 1, 2}));

    assert(rah2::ranges::distance(src.begin(), in_out.in) == 2);
    assert(rah2::ranges::distance(dst.begin(), in_out.out) == 4);
    /// [rah2::ranges::copy_backward]
}
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
    std::vector<NonCopyable> v;
    v.emplace_back();
    v.emplace_back();
    v.emplace_back();

    std::list<NonCopyable> l;
    auto res = rah2::ranges::move(v, std::back_inserter(l));
    assert(res.in == v.end());
    /// [rah2::ranges::move]
}
void test_move_backward()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::move_backward]
    using Vec = std::vector<std::string>;
    Vec a{"▁", "▂", "▃", "▄", "▅", "▆", "▇", "█"};
    Vec b(a.size());

    rah2::ranges::move_backward(a, b.end());
    assert(a == (Vec{"", "", "", "", "", "", "", ""}));
    assert(b == (Vec{"▁", "▂", "▃", "▄", "▅", "▆", "▇", "█"}));

    rah2::ranges::move_backward(b.begin(), b.end(), a.end());
    assert(b == (Vec{"", "", "", "", "", "", "", ""}));
    assert(a == (Vec{"▁", "▂", "▃", "▄", "▅", "▆", "▇", "█"}));

    rah2::ranges::move_backward(a.begin(), a.begin() + 3, a.end());
    assert(a == (Vec{"", "", "", "▄", "▅", "▁", "▂", "▃"}));
    /// [rah2::ranges::move_backward]
}
void test_fill()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::fill]
    std::vector<int> out{0, 0, 0, 4, 5};
    rah2::ranges::fill(out, 42);
    assert(out == (std::vector<int>{42, 42, 42, 42, 42}));
    rah2::ranges::fill(out.begin(), out.end(), 78);
    assert(out == (std::vector<int>{78, 78, 78, 78, 78}));
    /// [rah2::ranges::fill]
}
void test_fill_n()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::fill_n]
    std::vector<int> out(5);
    rah2::ranges::fill_n(out.begin(), 4, 42);
    assert(out == (std::vector<int>{42, 42, 42, 42, 0}));
    /// [rah2::ranges::fill_n]
}
void test_transform()
{
    testSuite.test_case("sample");
    {
        /// [rah2::ranges::transform]
        std::vector<int> vecIn1{0, 1, 2, 3};
        std::vector<int> vecOut{0, 0, 0, 0};
        rah2::ranges::transform(vecIn1, begin(vecOut), [](int a) { return a + 1; });
        assert(vecOut == std::vector<int>({1, 2, 3, 4}));
        /// [rah2::ranges::transform]
    }
    {
        /// [rah2::ranges::transform2]
        std::vector<int> vecIn1{0, 1, 2, 3};
        std::vector<int> const vecIn2{4, 3, 2, 1};
        std::vector<int> vecOut;
        rah2::ranges::transform(
            vecIn1, vecIn2, std::back_inserter(vecOut), [](int a, int b) { return a + b; });
        assert(vecOut == std::vector<int>({4, 4, 4, 4}));
        /// [rah2::ranges::transform2]
    }
}
void test_generate()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::generate]
    std::array<int, 8> v = {};
    rah2::ranges::generate(v, [n = 1]() mutable { return n++; });
    assert(v == (std::array<int, 8>{1, 2, 3, 4, 5, 6, 7, 8}));
    /// [rah2::ranges::generate]
}
void test_generate_n()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::generate_n]
    std::array<int, 8> v = {};
    rah2::ranges::generate_n(
        v.begin(), static_cast<intptr_t>(v.size()), [n{0}]() mutable { return n++; });
    assert(v == (std::array<int, 8>{0, 1, 2, 3, 4, 5, 6, 7}));
    /// [rah2::ranges::generate_n]
}
void test_remove()
{
    testSuite.test_case("sample");
    testSuite.test_case("return");
    /// [rah2::ranges::remove]
    std::vector<int> in{1, 2, 1, 3, 1};
    auto const to_erase = rah2::ranges::remove(in, 1);
    in.erase(to_erase.begin(), to_erase.end());
    std::sort(in.begin(), in.end());
    assert(in == std::vector<int>({2, 3}));
    /// [rah2::ranges::remove]
}
void test_remove_if()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::remove_if]
    std::vector<int> in{1, 2, 3, 4, 5};
    auto const to_erase = rah2::ranges::remove_if(in, [](auto a) { return a < 4; });
    in.erase(to_erase.begin(), to_erase.end());
    std::sort(in.begin(), in.end());
    assert(in == std::vector<int>({4, 5}));
    /// [rah2::ranges::remove_if]
}
void test_remove_copy()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::remove_copy]
    // Filter out the hash symbol from the given string.
    std::string const str{"#Small #Buffer #Optimization"};

    std::string out;
    rah2::ranges::remove_copy(str.begin(), str.end(), std::back_inserter(out), '#');
    assert(out == "Small Buffer Optimization");
    /// [rah2::ranges::remove_copy]
}
void test_remove_copy_if()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::remove_copy_if]
    // Copy only the complex numbers with positive imaginary part.
    using Ci = std::complex<int>;
    constexpr std::array<Ci, 5> source{Ci{1, 0}, Ci{0, 1}, Ci{2, -1}, Ci{3, 2}, Ci{4, -3}};
    std::vector<std::complex<int>> target;

    rah2::ranges::remove_copy_if(
        source, std::back_inserter(target), [](Ci z) { return z.imag() <= 0; });
    assert(target == (std::vector<std::complex<int>>{{0, 1}, {3, 2}}));
    /// [rah2::ranges::remove_copy_if]
}
void test_replace()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::replace]
    std::array<int, 6> p{1, 6, 1, 6, 1, 6};
    rah2::ranges::replace(p, 6, 9);
    assert(p == (std::array<int, 6>{1, 9, 1, 9, 1, 9}));
    /// [rah2::ranges::replace]
}
void test_replace_if()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::replace_if]
    std::array<int, 8> q{1, 2, 3, 6, 7, 8, 4, 5};
    rah2::ranges::replace_if(
        q, [](int x) { return 5 < x; }, 5);
    assert(q == (std::array<int, 8>{1, 2, 3, 5, 5, 5, 4, 5}));
    /// [rah2::ranges::replace_if]
}
void test_replace_copy()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::replace_copy]
    std::vector<int> o;
    std::array<int, 6> p{1, 6, 1, 6, 1, 6};
    o.resize(p.size());
    rah2::ranges::replace_copy(p, o.begin(), 6, 9);
    assert(o == (std::vector<int>{1, 9, 1, 9, 1, 9}));
    /// [rah2::ranges::replace_copy]
}
void test_replace_copy_if()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::replace_copy_if]
    std::vector<int> o;
    std::array<int, 8> q{1, 2, 3, 6, 7, 8, 4, 5};
    o.resize(q.size());
    rah2::ranges::replace_copy_if(
        q, o.begin(), [](int x) { return 5 < x; }, 5);
    assert(o == (std::vector<int>{1, 2, 3, 5, 5, 5, 4, 5}));
    /// [rah2::ranges::replace_copy_if]
}
void test_swap_ranges()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::swap_ranges]
    std::vector<char> p{'A', 'B', 'C', 'D', 'E'};
    std::list<char> q{'1', '2', '3', '4', '5', '6'};

    // swap p[0, 2) and q[1, 3):
    rah2::ranges::swap_ranges(
        p.begin(), p.begin() + 4, rah2::ranges::next(q.begin(), 1), rah2::ranges::next(q.begin(), 3));
    assert(p == (std::vector<char>{'2', '3', 'C', 'D', 'E'}));
    assert(q == (std::list<char>{'1', 'A', 'B', '4', '5', '6'}));

    // swap p[0, 5) and q[0, 5):
    rah2::ranges::swap_ranges(p, q);
    assert(q == (std::list<char>{'2', '3', 'C', 'D', 'E', '6'}));
    assert(p == (std::vector<char>{'1', 'A', 'B', '4', '5'}));
    /// [rah2::ranges::swap_ranges]
}
void test_reverse()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::reverse]
    std::string s{"ABCDEF"};
    rah2::ranges::reverse(s.begin(), s.end());
    assert(s == std::string{"FEDCBA"});
    rah2::ranges::reverse(s);
    assert(s == std::string{"ABCDEF"});

    std::array<int, 5> a{1, 2, 3, 4, 5};
    rah2::ranges::reverse(a);
    assert(a == (std::array<int, 5>{5, 4, 3, 2, 1}));
    /// [rah2::ranges::reverse]
}
void test_reverse_copy()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::reverse_copy]
    std::string x{"12345"};
    std::string y(x.size(), ' ');
    rah2::ranges::reverse_copy(x.begin(), x.end(), y.begin());
    assert(x == (std::string{"12345"}));
    assert(y == (std::string{"54321"}));
    rah2::ranges::reverse_copy(y, x.begin());
    assert(x == (std::string{"12345"}));
    /// [rah2::ranges::reverse_copy]
}
void test_rotate()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::rotate]
    std::string s(16, ' ');

    std::iota(s.begin(), s.end(), 'A');
    rah2::ranges::rotate(s, s.begin());
    assert(s == (std::string{"ABCDEFGHIJKLMNOP"}));
    rah2::ranges::rotate(s, s.begin() + 1);
    assert(s == (std::string{"BCDEFGHIJKLMNOPA"}));
    std::iota(s.begin(), s.end(), 'A');
    rah2::ranges::rotate(s, s.begin() + 3);
    assert(s == (std::string{"DEFGHIJKLMNOPABC"}));

    std::iota(s.begin(), s.end(), 'A');
    rah2::ranges::rotate(s, s.end());
    assert(s == (std::string{"ABCDEFGHIJKLMNOP"}));
    rah2::ranges::rotate(s, s.end() - 1);
    assert(s == (std::string{"PABCDEFGHIJKLMNO"}));
    std::iota(s.begin(), s.end(), 'A');
    rah2::ranges::rotate(s, s.end() - 3);
    assert(s == (std::string{"NOPABCDEFGHIJKLM"}));
    /// [rah2::ranges::rotate]
}
void test_rotate_copy()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::rotate_copy]
    std::vector<int> src{1, 2, 3, 4, 5};
    std::vector<int> dest(src.size());
    auto pivot = rah2::ranges::find(src, 3);

    rah2::ranges::rotate_copy(src, pivot, dest.begin());
    assert(dest == (std::vector<int>{3, 4, 5, 1, 2}));

    pivot = rah2::ranges::find(src, 3);
    rah2::ranges::rotate_copy(src.begin(), pivot, src.end(), dest.begin());
    assert(dest == (std::vector<int>{3, 4, 5, 1, 2}));
    /// [rah2::ranges::rotate_copy]
}
void test_shuffle()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::shuffle]
    std::random_device rd;
    std::mt19937 g(rd());
    std::vector<int> in{1, 2, 3, 4, 5, 6};
    rah2::ranges::shuffle(in, g);
    /// [rah2::ranges::shuffle]
}
void test_shift_left()
{
    testSuite.test_case("sample");
    testSuite.test_case("return");
    /// [rah2::ranges::shift_left]
    std::vector<int> b{1, 2, 3, 4, 5, 6, 7};

    auto b8 = rah2::ranges::shift_left(b, 8); // has no effect: n >= last - first
    assert(rah2::ranges::equal(b8, std::vector<int>{1, 2, 3, 4, 5, 6, 7}));
    assert(b == (std::vector<int>{1, 2, 3, 4, 5, 6, 7}));

    auto b0 = rah2::ranges::shift_left(b, 0); // has no effect: n == 0
    assert(rah2::ranges::equal(b0, std::vector<int>{1, 2, 3, 4, 5, 6, 7}));
    assert(b == (std::vector<int>{1, 2, 3, 4, 5, 6, 7}));

    std::vector<int> ref{4, 5, 6, 7};
    auto b3 = rah2::ranges::shift_left(b, 3);
    assert(rah2::ranges::equal(b3, ref));
    assert(rah2::ranges::equal(b.begin(), b.begin() + 4, ref.begin(), ref.end()));
    /// [rah2::ranges::shift_left]
}
void test_shift_right()
{
    // TODO : Test perf with all iterator/range type. Take care of random_access+sized_range
    testSuite.test_case("sample");
    testSuite.test_case("return");
    /// [rah2::ranges::shift_right]
    std::vector<int> b{1, 2, 3, 4, 5, 6, 7};

    auto b8 = rah2::ranges::shift_right(b, 8); // has no effect: n >= last - first
    assert(rah2::ranges::equal(b8, std::vector<int>{1, 2, 3, 4, 5, 6, 7}));
    assert(b == (std::vector<int>{1, 2, 3, 4, 5, 6, 7}));

    auto b0 = rah2::ranges::shift_right(b, 0); // has no effect: n == 0
    assert(rah2::ranges::equal(b0, std::vector<int>{1, 2, 3, 4, 5, 6, 7}));
    assert(b == (std::vector<int>{1, 2, 3, 4, 5, 6, 7}));

    std::vector<int> ref{1, 2, 3, 4};
    auto b3 = rah2::ranges::shift_right(b, 3);
    assert(rah2::ranges::equal(b3, ref));
    assert(rah2::ranges::equal(b.begin() + 3, b.end(), ref.begin(), ref.end()));
    /// [rah2::ranges::shift_right]
}
void test_sample()
{
    testSuite.test_case("sample");
    testSuite.test_case("return");
    /// [rah2::ranges::sample]
    auto const in = {1, 2, 3, 4, 5, 6};

    std::vector<int> out(in.size() + 2);
    auto const max = static_cast<intptr_t>(in.size() + 2);
    auto gen = std::mt19937{std::random_device{}()};

    for (intptr_t n{}; n != max; ++n)
    {
        auto o = rah2::ranges::sample(in, out.begin(), n, gen);
        assert((o - out.begin()) == std::min(n, static_cast<intptr_t>(in.size())));
    }

    auto const o = rah2::ranges::sample(in, out.begin(), static_cast<intptr_t>(in.size()), gen);
    assert(rah2::ranges::equal(in.begin(), in.end(), out.begin(), o));

    /// [rah2::ranges::sample]
}
void test_unique()
{
    testSuite.test_case("sample");
    {
        /// [rah2::ranges::unique]
        std::vector<int> in{2, 1, 1, 1, 5, 3, 3, 4};
        in.erase(rah2::ranges::unique(in).begin(), end(in));
        assert(in == std::vector<int>({2, 1, 5, 3, 4}));
        /// [rah2::ranges::unique]
    }
    {
        /// [rah2::ranges::unique_pred]
        std::vector<int> in{2, 1, 1, 1, 5, 3, 3, 4};
        in.erase(rah2::ranges::unique(in, [](auto a, auto b) { return a == b; }).begin(), end(in));
        assert(in == std::vector<int>({2, 1, 5, 3, 4}));
        /// [rah2::ranges::unique_pred]
    }
}
void test_unique_copy()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::unique_copy]
    std::string s1{"The      string    with many       spaces!"};

    std::string s2;
    rah2::ranges::unique_copy(
        s1.begin(),
        s1.end(),
        std::back_inserter(s2),
        [](char c1, char c2) { return c1 == ' ' && c2 == ' '; });
    assert(s2 == (std::string{"The string with many spaces!"}));
    /// [rah2::ranges::unique_copy]
}
void test_is_partitioned()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::is_partitioned]
    std::array<int, 9> v = {1, 2, 3, 4, 5, 6, 7, 8, 9};

    auto is_even = [](int i)
    {
        return i % 2 == 0;
    };

    assert(rah2::ranges::is_partitioned(v, is_even) == false);

    rah2::ranges::partition(v, is_even);
    assert(rah2::ranges::is_partitioned(v, is_even));

    rah2::ranges::reverse(v);
    assert(rah2::ranges::is_partitioned(v.cbegin(), v.cend(), is_even) == false);
    assert(rah2::ranges::is_partitioned(v.crbegin(), v.crend(), is_even));
    /// [rah2::ranges::is_partitioned]
}
void test_partition()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::partition]
    std::vector<int> in{1, 2, 3, 4, 5};
    auto const boundary = rah2::ranges::partition(in, [](auto a) { return a >= 4; });
    assert(boundary.begin() == in.begin() + 2);
    std::sort(in.begin(), boundary.begin());
    std::sort(boundary.begin(), in.end());
    assert(in == std::vector<int>({4, 5, 1, 2, 3}));
    /// [rah2::ranges::partition]
}
void test_partition_copy()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::partition_copy]
    std::vector<char> const in = {'N', '3', 'U', 'M', '1', 'B', '4', 'E', '1', '5', 'R', '9'};

    std::vector<int> o1(in.size());
    std::vector<int> o2(in.size());

    auto pred = [](char c)
    {
        return std::isalpha(c);
    };

    auto const ret = rah2::ranges::partition_copy(in, o1.begin(), o2.begin(), pred);

    assert(in == (std::vector<char>{'N', '3', 'U', 'M', '1', 'B', '4', 'E', '1', '5', 'R', '9'}));
    std::vector<int> o1_expected{'N', 'U', 'M', 'B', 'E', 'R'};
    std::vector<int> o2_expected{'3', '1', '4', '1', '5', '9'};
    assert(rah2::ranges::equal(o1.begin(), ret.out1, o1_expected.begin(), o1_expected.end()));
    assert(rah2::ranges::equal(o2.begin(), ret.out2, o2_expected.begin(), o2_expected.end()));
    /// [rah2::ranges::partition_copy]
}
void test_stable_partition()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::stable_partition]
    std::vector<int> in{1, 2, 3, 4, 5};
    auto const boundary = rah2::ranges::stable_partition(in, [](auto a) { return a >= 4; });
    assert(boundary.begin() == in.begin() + 2);
    assert(in == std::vector<int>({4, 5, 1, 2, 3}));
    /// [rah2::ranges::stable_partition]
}
void test_partition_point()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::partition_point]
    std::array<int, 9> v{1, 2, 3, 4, 5, 6, 7, 8, 9};

    auto is_even = [](int i)
    {
        return i % 2 == 0;
    };

    rah2::ranges::partition(v, is_even);

    auto const pp = rah2::ranges::partition_point(v, is_even);
    auto const i = rah2::ranges::distance(v.cbegin(), pp);
    assert(i == 4); // 4 even number in v
    /// [rah2::ranges::partition_point]
}
void test_is_sorted()
{
    testSuite.test_case("sample");
    testSuite.test_case("return");
    /// [rah2::ranges::is_sorted]

    std::array<int, 5> digits{3, 1, 4, 1, 5};
    assert(not rah2::ranges::is_sorted(digits));

    rah2::ranges::sort(digits);
    assert(rah2::ranges::is_sorted(digits));

    rah2::ranges::reverse(digits);
    assert(not rah2::ranges::is_sorted(digits));
    assert(rah2::ranges::is_sorted(digits, rah2::greater{}));
    /// [rah2::ranges::is_sorted]
}
void test_is_sorted_until()
{
    testSuite.test_case("sample");
    testSuite.test_case("return");
    testSuite.test_case("empty");
    /// [rah2::ranges::is_sorted_until]
    std::array<int, 0> a0 = {};
    auto const sorted_end0 = rah2::ranges::is_sorted_until(a0);
    assert(rah2::ranges::distance(a0.begin(), sorted_end0) == 0);

    std::array<int, 6> a1{3, 1, 4, 1, 5, 9};
    auto const sorted_end = rah2::ranges::is_sorted_until(a1);
    assert(rah2::ranges::distance(a1.begin(), sorted_end) == 1);

    std::array<int, 6> a3{3, 6, 18, 1, 5, 9};
    auto const sorted_end3 = rah2::ranges::is_sorted_until(a3);
    assert(rah2::ranges::distance(a3.begin(), sorted_end3) == 3);

    std::array<int, 6> a6{3, 6, 18, 19, 20, 78};
    auto const sorted_end6 = rah2::ranges::is_sorted_until(a6);
    assert(rah2::ranges::distance(a6.begin(), sorted_end6) == 6);
    /// [rah2::ranges::is_sorted_until]
}
void test_sort()
{
    testSuite.test_case("sample");
    {
        /// [rah2::ranges::sort]
        std::vector<int> in{2, 1, 5, 3, 4};
        rah2::ranges::sort(in);
        assert(in == std::vector<int>({1, 2, 3, 4, 5}));
        /// [rah2::ranges::sort]
    }
    {
        /// [rah2::ranges::sort_pred]
        std::vector<int> in{2, 1, 5, 3, 4};
        rah2::ranges::sort(in, [](auto a, auto b) { return a < b; });
        assert(in == std::vector<int>({1, 2, 3, 4, 5}));
        /// [rah2::ranges::sort_pred]
    }
}
void test_partial_sort()
{
    testSuite.test_case("sample");
    testSuite.test_case("return");
    /// [rah2::ranges::partial_sort]
    std::vector<char> v{'x', 'P', 'y', 'C', 'z', 'w', 'P', 'o'};

    int const m{3};
    rah2::ranges::partial_sort(v, v.begin() + m);
    assert((rah2::ranges::equal(v | rah2::ranges::views::take(3), std::string("CPP"))));

    std::string s{"3a1b41c5"};
    rah2::ranges::partial_sort(s.begin(), s.begin() + m, s.end(), rah2::greater{});
    assert((rah2::ranges::equal(s | rah2::ranges::views::take(3), std::string("cba"))));
    /// [rah2::ranges::partial_sort]
}
void test_partial_sort_copy()
{
    testSuite.test_case("sample");
    testSuite.test_case("return");
    /// [rah2::ranges::partial_sort_copy]
    std::forward_list<int> const source{4, 2, 5, 1, 3};

    std::vector<int> dest1{10, 11, 12};
    auto lastI_lastO = rah2::ranges::partial_sort_copy(source, dest1);
    assert(dest1 == (std::vector<int>{1, 2, 3}));
    assert(lastI_lastO.in == source.end());
    assert(lastI_lastO.out == dest1.end());

    std::vector<int> dest2{10, 11, 12, 13, 14, 15, 16};
    lastI_lastO = rah2::ranges::partial_sort_copy(source, dest2, rah2::greater{});
    assert(dest2 == (std::vector<int>{5, 4, 3, 2, 1, 15, 16}));
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
        std::vector<CmpA> in{{4, 1}, {2, 1}, {4, 2}, {1, 1}, {4, 3}, {2, 2}, {4, 4}};
        rah2::ranges::stable_sort(in);
        assert(in == std::vector<CmpA>({{1, 1}, {2, 1}, {2, 2}, {4, 1}, {4, 2}, {4, 3}, {4, 4}}));
    }
    /// [rah2::ranges::stable_sort]
    {
        /// [rah2::ranges::stable_sort_pred]
        std::vector<std::pair<int, int>> in{{4, 1}, {2, 1}, {4, 2}, {1, 1}, {4, 3}, {2, 2}, {4, 4}};
        rah2::ranges::stable_sort(in, [](auto l, auto r) { return l.second < r.second; });
        assert(
            in
            == (std::vector<std::pair<int, int>>(
                {{4, 1}, {2, 1}, {1, 1}, {4, 2}, {2, 2}, {4, 3}, {4, 4}})));
        /// [rah2::ranges::stable_sort_pred]
    }
}
void test_nth_element()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::nth_element]
    std::array<int, 9> v{5, 6, 4, 3, 2, 6, 7, 9, 3};

    auto out_last = rah2::ranges::nth_element(v, v.begin() + 4);
    assert(v[4] == 5);
    assert(out_last == v.end());

    out_last = rah2::ranges::nth_element(v, v.begin() + 1, rah2::greater());
    assert(v[1] == 7);
    assert(out_last == v.end());

    using namespace std::literals;
    std::array<std::string, 7> names{
        "Diva",
        "Cornelius",
        "Munro",
        "Rhod"
        "Zorg",
        "Korben",
        "Bender",
        "Leeloo"};
    auto const out_last2 = rah2::ranges::nth_element(names, names.begin() + 4);
    assert(names[4] == "Leeloo");
    assert(out_last2 == names.end());
    /// [rah2::ranges::nth_element]
}
void test_lower_bound()
{
    testSuite.test_case("sample");
    testSuite.test_case("return");
    /// [rah2::ranges::lower_bound]
    std::vector<int> data{1, 2, 2, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 5};
    auto const lower = rah2::ranges::lower_bound(data, 4);
    assert(lower == data.begin() + 6);
    /// [rah2::ranges::lower_bound]
}
void test_upper_bound()
{
    testSuite.test_case("sample");
    testSuite.test_case("return");
    /// [rah2::ranges::upper_bound]
    std::vector<int> data{1, 2, 2, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 5};
    auto const upper = rah2::ranges::upper_bound(data, 4);
    assert(upper == data.begin() + 10);
    /// [rah2::ranges::upper_bound]
}
void test_binary_search()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::binary_search]
    std::vector<int> vecIn1{1, 2, 2, 3, 4};
    assert(not rah2::ranges::binary_search(vecIn1, 0));
    assert(rah2::ranges::binary_search(vecIn1, 1));
    assert(rah2::ranges::binary_search(vecIn1, 2));
    /// [rah2::ranges::binary_search]
}
void test_equal_range()
{
    testSuite.test_case("sample");
    {
        /// [rah2::ranges::equal_range]
        std::vector<int> vecIn1{1, 2, 2, 3, 4};
        {
            std::vector<int> out;
            for (int const i : rah2::ranges::equal_range(vecIn1, 0))
                out.push_back(i);
            assert(out == std::vector<int>({}));
        }
        {
            std::vector<int> out;
            for (int const i : rah2::ranges::equal_range(vecIn1, 1))
                out.push_back(i);
            assert(out == std::vector<int>({1}));
        }
        {
            std::vector<int> out;
            for (int const i : rah2::ranges::equal_range(vecIn1, 2))
                out.push_back(i);
            assert(out == std::vector<int>({2, 2}));
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
            std::vector<S> vecIn1{{1, 'a'}, {2, 'b'}, {2, 'c'}, {3, 'd'}, {4, 'd'}};
            {
                std::vector<S> out;
                for (S const i : rah2::ranges::equal_range(vecIn1, 0, FindS{}))
                    out.push_back(i);
                assert(out == std::vector<S>({}));
            }
            {
                std::vector<S> out;
                for (S const i : rah2::ranges::equal_range(vecIn1, 1, FindS{}))
                    out.push_back(i);
                assert(out == std::vector<S>({{1, 'a'}}));
            }
            {
                std::vector<S> out;
                for (S const i : rah2::ranges::equal_range(vecIn1, 2, FindS{}))
                    out.push_back(i);
                assert(out == std::vector<S>({{2, 'b'}, {2, 'c'}}));
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
    std::vector<int> in1 = {1, 2, 3, 4, 5};
    std::vector<int> in2 = {3, 4, 5, 6, 7};
    std::vector<int> out(in1.size() + in2.size());

    auto const ret = rah2::ranges::merge(in1, in2, out.begin());
    assert((rah2::ranges::equal(
        rah2::ranges::make_subrange(out.begin(), ret.out),
        std::vector<int>{1, 2, 3, 3, 4, 4, 5, 5, 6, 7})));

    in1 = {1, 2, 3, 4, 5, 5, 5};
    in2 = {3, 4, 5, 6, 7};
    out.clear();
    rah2::ranges::merge(in1, in2, std::back_inserter(out));
    assert(out == (std::vector<int>{1, 2, 3, 3, 4, 4, 5, 5, 5, 5, 6, 7}));
    /// [rah2::ranges::merge]
}
void test_inplace_merge()
{
    testSuite.test_case("sample");
    testSuite.test_case("return");
    /// [rah2::ranges::inplace_merge]
    std::vector<int> v{1, 4, 8, 9, 10, 45, 2, 3, 4, 9, 11};
    auto const last = rah2::ranges::inplace_merge(v, v.begin() + 6);
    assert(last == v.end());
    assert(rah2::ranges::is_sorted(v));
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

    assert(rah2::ranges::includes(z.begin(), z.end(), a.begin(), a.end()));
    assert(rah2::ranges::includes(z, b));
    assert(not rah2::ranges::includes(z, g));
    assert(not rah2::ranges::includes(z, c));
    assert(not rah2::ranges::includes(z, d));
    assert(not rah2::ranges::includes(z, e));
    assert(rah2::ranges::includes(z, f, ignore_case));
    /// [rah2::ranges::includes]
}
void test_set_difference()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::set_difference]
    std::vector<int> in1{1, 3, 4};
    std::vector<int> in2{1, 2, 3};
    std::vector<int> out{0, 0, 0, 0};
    rah2::ranges::set_difference(in1, in2, out.begin());
    assert(out == std::vector<int>({4, 0, 0, 0}));
    /// [rah2::ranges::set_difference]
}
void test_set_intersection()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::set_intersection]
    std::vector<int> in1{1, 3, 4};
    std::vector<int> in2{1, 2, 3};
    std::vector<int> out{0, 0, 0, 0};
    rah2::ranges::set_intersection(in1, in2, out.begin());
    assert(out == std::vector<int>({1, 3, 0, 0}));
    /// [rah2::ranges::set_intersection]
}
void test_set_symmetric_difference()
{
    testSuite.test_case("sample");
    testSuite.test_case("return");
    /// [rah2::ranges::set_symmetric_difference]
    auto const in1 = {1, 3, 4, 6, 7, 9};
    auto const in2 = {1, 4, 5, 6, 9};

    std::vector<int> out(5);

    auto const res = rah2::ranges::set_symmetric_difference(in1, in2, out.begin());
    assert(out == (std::vector<int>{3, 5, 7, 0, 0}));
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
    std::vector<int> in1 = {1, 2, 3, 4, 5};
    std::vector<int> in2 = {3, 4, 5, 6, 7};
    std::vector<int> out(in1.size() + in2.size());
    auto const ret = rah2::ranges::set_union(in1, in2, out.begin());
    assert(out == (std::vector<int>{1, 2, 3, 4, 5, 6, 7, 0, 0, 0}));
    assert(ret.in1 == in1.end());
    assert(ret.in2 == in2.end());
    assert(ret.out == out.begin() + 7);

    in1 = {1, 2, 3, 4, 5, 5, 5};
    in2 = {3, 4, 5, 6, 7};
    out.clear();
    out.reserve(in1.size() + in2.size());
    auto const ret2 = rah2::ranges::set_union(in1, in2, std::back_inserter(out));
    assert(out == (std::vector<int>{1, 2, 3, 4, 5, 5, 5, 6, 7}));
    assert(ret2.in1 == in1.end());
    assert(ret2.in2 == in2.end());
    /// [rah2::ranges::set_union]
}
void test_is_heap()
{
    testSuite.test_case("sample");
    testSuite.test_case("return");
    /// [rah2::ranges::is_heap]
    std::vector<int> v{3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5, 8, 9, 7, 9, 3, 2, 3, 8};
    assert(!rah2::ranges::is_heap(v));
    std::make_heap(v.begin(), v.end());
    assert(rah2::ranges::is_heap(v));
    /// [rah2::ranges::is_heap]
}
void test_is_heap_until()
{
    testSuite.test_case("sample");
    testSuite.test_case("return");
    /// [rah2::ranges::is_heap_until]
    std::vector<int> v{3, 1, 4, 1, 5, 9};
    std::make_heap(v.begin(), v.end());
    assert(rah2::ranges::is_heap_until(v) == v.end());

    // mess up the heap
    v.push_back(10);
    v.push_back(20);

    auto const heap_end = rah2::ranges::is_heap_until(v);
    assert(v.begin() + 6 == heap_end);
    /// [rah2::ranges::is_heap_until]
}
void test_make_heap()
{
    testSuite.test_case("sample");
    testSuite.test_case("return");
    /// [rah2::ranges::make_heap]
    std::vector<int> h{1, 6, 1, 8, 0, 3, 3, 9, 8, 8, 7, 4, 9, 8, 9};
    assert(!std::is_heap(h.begin(), h.end()));
    auto const last = rah2::ranges::make_heap(h);
    assert(last == h.end());
    assert(std::is_heap(h.begin(), h.end()));

    assert(!std::is_heap(h.begin(), h.end(), rah2::greater{}));
    auto const last2 = rah2::ranges::make_heap(h, rah2::greater{});
    assert(last2 == h.end());
    assert(std::is_heap(h.begin(), h.end(), rah2::greater{}));
    /// [rah2::ranges::make_heap]
}
void test_push_heap()
{
    testSuite.test_case("sample");
    testSuite.test_case("return");
    /// [rah2::ranges::push_heap]

    std::vector<int> v{1, 6, 1, 8, 0, 3};
    rah2::ranges::make_heap(v);

    v.push_back(9);
    auto const last = rah2::ranges::push_heap(v);
    assert(last == v.end());

    assert(std::is_heap(v.begin(), v.end()));
    assert(std::count(v.begin(), v.end(), 9) != 0);
    /// [rah2::ranges::push_heap]
}
void test_pop_heap()
{
    testSuite.test_case("sample");
    testSuite.test_case("return");
    /// [rah2::ranges::pop_heap]
    std::vector<int> v{3, 1, 4, 1, 5, 9, 2, 6, 5, 3};

    std::make_heap(v.begin(), v.end());
    auto const last = rah2::ranges::pop_heap(v);
    assert(last == v.end());
    assert(v.back() == 9);
    v.pop_back();
    assert(std::is_heap(v.begin(), v.end()));

    rah2::ranges::pop_heap(v);
    assert(v.back() == 6);
    v.pop_back();
    assert(std::is_heap(v.begin(), v.end()));
    /// [rah2::ranges::pop_heap]
}
void test_sort_heap()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::sort_heap]
    std::array<int, 6> v{3, 1, 4, 1, 5, 9};
    std::make_heap(v.begin(), v.end());
    rah2::ranges::sort_heap(v);
    assert(std::is_sorted(v.begin(), v.end()));
    /// [rah2::ranges::sort_heap]
}
void test_max()
{
    testSuite.test_case("sample");
    testSuite.test_case("return");
    /// [rah2::ranges::max]
    assert(rah2::ranges::max(1, 3) == 3);
    assert(rah2::ranges::max(1, 3, rah2::greater{}) == 1);

    assert(rah2::ranges::max({1, 7, 5}) == 7);
    assert(rah2::ranges::max({1, 7, 5}, rah2::greater{}) == 1);

    std::vector<int> v{1, 7, 5};
    assert(rah2::ranges::max(v) == 7);
    assert(rah2::ranges::max(v, rah2::greater{}) == 1);
    /// [rah2::ranges::max]
}
void test_max_element()
{
    testSuite.test_case("sample");
    {
        /// [rah2::ranges::max_element]
        std::vector<int> in{1, 5, 3, 4};
        auto const iter = rah2::ranges::max_element(in);
        assert(*iter == 5);
        /// [rah2::ranges::max_element]
    }
    {
        /// [rah2::ranges::max_element_pred]
        std::vector<std::pair<int, int>> in{{100, 3}, {0, 5}, {0, 1}, {0, 4}};
        auto const iter =
            rah2::ranges::max_element(in, [](auto&& a, auto& b) { return a.second < b.second; });
        assert(*iter == (std::pair<int, int>{0, 5}));
        /// [rah2::ranges::max_element_pred]
    }
}
void test_min()
{
    testSuite.test_case("sample");
    testSuite.test_case("return");
    /// [rah2::ranges::min]
    assert(rah2::ranges::min(1, 3) == 1);
    assert(rah2::ranges::min(1, 3, rah2::greater{}) == 3);

    assert(rah2::ranges::min({1, 7, 5}) == 1);
    assert(rah2::ranges::min({1, 7, 5}, rah2::greater{}) == 7);

    std::vector<int> v{1, 7, 5};
    assert(rah2::ranges::min(v) == 1);
    assert(rah2::ranges::min(v, rah2::greater{}) == 7);
    /// [rah2::ranges::min]
}
void test_min_element()
{
    testSuite.test_case("sample");
    testSuite.test_case("range");
    testSuite.test_case("nopred");
    {
        /// [rah2::ranges::min_element]
        std::vector<int> in{1, -5, 3, 4};
        auto iter = rah2::ranges::min_element(in);
        assert(*iter == -5);
        /// [rah2::ranges::min_element]

        testSuite.test_case("iter");
        iter = rah2::ranges::min_element(in.begin(), in.end());
        assert(*iter == -5);
    }

    {
        testSuite.test_case("pred");
        /// [rah2::ranges::min_element_pred]
        std::vector<std::pair<int, int>> in{{-100, 3}, {0, -5}, {0, 1}, {0, 4}};
        auto const iter =
            rah2::ranges::min_element(in, [](auto&& a, auto& b) { return a.second < b.second; });
        assert(*iter == (std::pair<int, int>{0, -5}));
        /// [rah2::ranges::min_element_pred]
    }
}
void test_minmax()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::minmax]

    constexpr int a = 1;
    constexpr int b = 3;
    auto const res1 = rah2::ranges::minmax(a, b);
    assert(res1.min == 1);
    assert(res1.max == 3);
    auto const res2 = rah2::ranges::minmax(a, b, rah2::greater{});
    assert(res2.min == 3);
    assert(res2.max == 1);

    auto const res3 = rah2::ranges::minmax({1, 7, 5});
    assert(res3.min == 1);
    assert(res3.max == 7);
    auto const res4 = rah2::ranges::minmax({1, 7, 5}, rah2::greater{});
    assert(res4.min == 7);
    assert(res4.max == 1);

    std::vector<int> v{1, 7, 5};
    auto const res5 = rah2::ranges::minmax(v);
    assert(res5.min == 1);
    assert(res5.max == 7);
    auto const res6 = rah2::ranges::minmax(v, rah2::greater{});
    assert(res6.min == 7);
    assert(res6.max == 1);
    /// [rah2::ranges::minmax]
}
void test_minmax_element()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::minmax_element]
    std::vector<int> v{1, 7, 5};
    auto const res5 = rah2::ranges::minmax_element(v);
    assert(*res5.min == 1);
    assert(*res5.max == 7);
    auto const res6 = rah2::ranges::minmax_element(v, rah2::greater{});
    assert(*res6.min == 7);
    assert(*res6.max == 1);
    /// [rah2::ranges::minmax_element]
}
void test_clamp()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::clamp]
    assert(rah2::ranges::clamp(0, 4, 8) == 4);
    assert(rah2::ranges::clamp(4, 4, 8) == 4);
    assert(rah2::ranges::clamp(6, 4, 8) == 6);
    assert(rah2::ranges::clamp(8, 4, 8) == 8);
    assert(rah2::ranges::clamp(10, 4, 8) == 8);
    /// [rah2::ranges::clamp]

    testSuite.test_case("comp");
    assert(rah2::ranges::clamp(0, 8, 4, std::greater<>()) == 4);
    assert(rah2::ranges::clamp(4, 8, 4, std::greater<>()) == 4);
    assert(rah2::ranges::clamp(6, 8, 4, std::greater<>()) == 6);
    assert(rah2::ranges::clamp(8, 8, 4, std::greater<>()) == 8);
    assert(rah2::ranges::clamp(10, 8, 4, std::greater<>()) == 8);
}
void test_is_permutation()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::is_permutation]
    static constexpr auto r1 = {1, 2, 3, 4, 5};
    static constexpr auto r2 = {3, 5, 4, 1, 2};
    assert(rah2::ranges::is_permutation(r1, r1));
    assert(rah2::ranges::is_permutation(r1, r2));
    assert(rah2::ranges::is_permutation(r2, r1));
    assert(rah2::ranges::is_permutation(r1.begin(), r1.end(), r2.begin(), r2.end()));
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
    std::string s{"abc"};
    std::set<std::string> allPermutation;
    do
    {
        if (not allPermutation.empty())
            assert(s > *(--allPermutation.end()));
        allPermutation.emplace(s);
    } while (rah2::ranges::next_permutation(s.begin(), s.end()).found);
    assert(allPermutation.size() == factorial(s.size()));

    std::set<std::array<int, 3>> allPermutation2;

    // Generate all permutations (range case)
    std::array<int, 3> a{'a', 'b', 'c'};
    do
    {
        assert(allPermutation2.count(a) == 0);
        if (not allPermutation2.empty())
            assert(a > *(--allPermutation2.end()));
        allPermutation2.emplace(a);
    } while (rah2::ranges::next_permutation(a).found);
    assert(allPermutation2.size() == factorial(s.size()));

    std::set<std::array<std::string, 3>> allPermutation3;

    // Generate all permutations using comparator
    std::array<std::string, 3> z{"C", "B", "A"};
    do
    {
        if (not allPermutation3.empty())
            assert(z < *(allPermutation3.begin()));
        allPermutation3.emplace(z);
    } while (rah2::ranges::next_permutation(z, rah2::greater{}).found);
    assert(allPermutation3.size() == factorial(s.size()));
    /// [rah2::ranges::next_permutation]
}
void test_prev_permutation()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::prev_permutation]

    // Generate all permutations (iterators case)
    std::string s{"cba"};
    std::set<std::string> allPermutation;
    do
    {
        if (not allPermutation.empty())
            assert(s < *(allPermutation.begin()));
        allPermutation.emplace(s);
    } while (rah2::ranges::prev_permutation(s.begin(), s.end()).found);
    assert(allPermutation.size() == factorial(s.size()));

    std::set<std::array<int, 3>> allPermutation2;

    // Generate all permutations (range case)
    std::array<int, 3> a{'c', 'b', 'a'};
    do
    {
        if (not allPermutation2.empty())
            assert(a < *(allPermutation2.begin()));
        allPermutation2.emplace(a);
    } while (rah2::ranges::prev_permutation(a).found);
    assert(allPermutation2.size() == factorial(s.size()));

    std::set<std::array<std::string, 3>> allPermutation3;

    // Generate all permutations using comparator
    std::array<std::string, 3> z{"A", "B", "C"};
    do
    {
        if (not allPermutation3.empty())
            assert(z > *(--allPermutation3.end()));
        allPermutation3.emplace(z);
    } while (rah2::ranges::prev_permutation(z, rah2::greater{}).found);
    assert(allPermutation3.size() == factorial(s.size()));
    /// [rah2::ranges::prev_permutation]
}
void test_iota()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::iota]
    std::list<int> list(8);

    // Fill the list with ascending values: 0, 1, 2, ..., 7
    rah2::ranges::iota(list, 0);
    assert(list == (std::list<int>{0, 1, 2, 3, 4, 5, 6, 7}));
    /// [rah2::ranges::iota]
}
void test_fold_left()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::fold_left]
    std::vector<int> vecIn1{1, 2, 3, 4};
    assert(rah2::ranges::fold_left(vecIn1, 0, [](auto a, auto b) { return a + b; }) == 10);
    /// [rah2::ranges::fold_left]
}
void test_fold_left_first()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::fold_left_first]
    std::vector<int> v{1, 2, 3, 4, 5, 6, 7, 8};

    auto sum = rah2::ranges::fold_left_first(v.begin(), v.end(), std::plus<>()); // (1)
    assert(sum.value() == 36);

    auto mul = rah2::ranges::fold_left_first(v, std::multiplies<>()); // (2)
    assert(mul.value() == 40320);

    // get the product of the std::pair::second of all pairs in the vector:
    std::vector<std::pair<char, float>> data{{'A', 3.f}, {'B', 3.5f}, {'C', 4.f}};
    auto sec =
        rah2::ranges::fold_left_first(data | rah2::ranges::views::values(), std::multiplies<>());
    assert(*sec == 42);

    // use a program defined function object (lambda-expression):
    auto val = rah2::ranges::fold_left_first(v, [](int x, int y) { return x + y + 13; });
    assert(*val == 127);
    /// [rah2::ranges::fold_left_first]
}
void test_fold_right()
{
    testSuite.test_case("sample");
    testSuite.test_case("return");
    /// [rah2::ranges::fold_right]
    auto v = {1, 2, 3, 4, 5, 6, 7, 8};
    std::vector<std::string> vs{"A", "B", "C", "D"};

    auto const r1 = rah2::ranges::fold_right(v.begin(), v.end(), 6, std::plus<>()); // (1)
    assert(r1 == 42);

    auto const r2 = rah2::ranges::fold_right(vs, std::string("!"), std::plus<>()); // (2)
    assert(r2 == std::string("ABCD!"));

    // Use a program defined function object (lambda-expression):
    std::string const r3 = rah2::ranges::fold_right(
        v, "A", [](int x, std::string const& s) { return s + ':' + std::to_string(x); });
    assert(r3 == std::string("A:8:7:6:5:4:3:2:1"));

    // Get the product of the std::pair::second of all pairs in the vector:
    std::vector<std::pair<char, float>> data{{'A', 2.f}, {'B', 3.f}, {'C', 3.5f}};
    float const r4 =
        rah2::ranges::fold_right(data | rah2::ranges::views::values(), 2.0f, std::multiplies<>());
    assert(r4 == 42);
    /// [rah2::ranges::fold_right]
}
void test_fold_right_last()
{
    testSuite.test_case("sample");
    testSuite.test_case("return");
    /// [rah2::ranges::fold_right_last]
    auto v = {1, 2, 3, 4, 5, 6, 7, 8};
    std::vector<std::string> vs{"A", "B", "C", "D"};

    auto r1 = rah2::ranges::fold_right_last(v.begin(), v.end(), std::plus<>()); // (1)
    assert(*r1 == 36);

    auto r2 = rah2::ranges::fold_right_last(vs, std::plus<>()); // (2)
    assert(*r2 == "ABCD");

    // Use a program defined function object (lambda-expression):
    auto r3 = rah2::ranges::fold_right_last(v, [](int x, int y) { return x + y + 99; });
    assert(*r3 == 729);

    // Get the product of the std::pair::second of all pairs in the vector:
    std::vector<std::pair<char, float>> data{{'A', 3.f}, {'B', 3.5f}, {'C', 4.f}};
    auto r4 =
        rah2::ranges::fold_right_last(data | rah2::ranges::views::values(), std::multiplies<>());
    assert(*r4 == 42);
    /// [rah2::ranges::fold_right_last]
}
void test_fold_left_with_iter()
{
    testSuite.test_case("sample");
    testSuite.test_case("return");
    /// [rah2::ranges::fold_left_with_iter]
    std::vector<int> v{1, 2, 3, 4, 5, 6, 7, 8};

    auto const sum = rah2::ranges::fold_left_with_iter(v.begin(), v.end(), 6, std::plus<>());
    assert(sum.value == 42);
    assert(sum.in == v.end());

    auto const mul = rah2::ranges::fold_left_with_iter(v, 0X69, std::multiplies<>());
    assert(mul.value == 4233600);
    assert(mul.in == v.end());

    // get the product of the std::pair::second of all pairs in the vector:
    std::vector<std::pair<char, float>> data{{'A', 2.f}, {'B', 3.f}, {'C', 3.5f}};
    auto const sec = rah2::ranges::fold_left_with_iter(
        data | rah2::ranges::views::values(), 2.0f, std::multiplies<>());
    assert(sec.value == 42);

    // use a program defined function object (lambda-expression):
    auto lambda = [](int x, int y)
    {
        return x + 0B110 + y;
    };
    auto const val = rah2::ranges::fold_left_with_iter(v, -42, lambda);
    assert(val.value == 42);
    assert(val.in == v.end());
    /// [rah2::ranges::fold_left_with_iter]
}
void test_fold_left_first_with_iter()
{
    testSuite.test_case("sample");
    testSuite.test_case("return");
    /// [rah2::ranges::fold_left_first_with_iter]

    std::vector<int> v{1, 2, 3, 4, 5, 6, 7, 8};

    auto sum = rah2::ranges::fold_left_first_with_iter(v.begin(), v.end(), std::plus<>());
    assert(sum.value.value() == 36);
    assert(sum.in == v.end());

    auto mul = rah2::ranges::fold_left_first_with_iter(v, std::multiplies<>());
    assert(mul.value.value() == 40320);
    assert(mul.in == v.end());

    // get the product of the std::pair::second of all pairs in the vector:
    std::vector<std::pair<char, float>> data{{'A', 2.f}, {'B', 3.f}, {'C', 7.f}};
    auto sec = rah2::ranges::fold_left_first_with_iter(
        data | rah2::ranges::views::values(), std::multiplies<>());
    assert(sec.value.value() == 42);

    // use a program defined function object (lambda-expression):
    auto lambda = [](int x, int y)
    {
        return x + y + 2;
    };
    auto val = rah2::ranges::fold_left_first_with_iter(v, lambda);
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

    auto const sz{rah2::ranges::size(v)};
    alignas(alignof(std::string)) char pbuf[sz * sizeof(std::string)];
    auto const first{reinterpret_cast<std::string*>(pbuf)};
    auto const last{first + sz};
    rah2::ranges::uninitialized_copy(std::begin(v), std::end(v), first, last);

    for (size_t i = 0; i < 4; ++i)
        assert(v[i] == first[i]);

    rah2::ranges::destroy(first, last); // NOLINT(cppcoreguidelines-no-malloc)

    /// [rah2::ranges::uninitialized_copy]
}
void test_uninitialized_copy_n()
{
    testSuite.test_case("sample");
    // testSuite.test_case("return");
    /// [rah2::ranges::uninitialized_copy_n]
    char const* stars[] = {"Procyon", "Spica", "Pollux", "Deneb", "Polaris"};

    constexpr int n{4};
    alignas(alignof(std::string)) char out[n * sizeof(std::string)];

    auto const first{reinterpret_cast<std::string*>(out)};
    auto const last{first + n};
    auto const ret = rah2::ranges::uninitialized_copy_n(std::begin(stars), n, first, last);
    assert(ret.in == stars + n);
    assert(ret.out == last);

    for (size_t i = 0; i < n; ++i)
        assert(stars[i] == first[i]);

    rah2::ranges::destroy(first, last);
    /// [rah2::ranges::uninitialized_copy_n]
}
void test_uninitialized_fill()
{
    testSuite.test_case("sample");
    // testSuite.test_case("return");
    /// [rah2::ranges::uninitialized_fill]

    constexpr int n{4};
    alignas(alignof(std::string)) char out[n * sizeof(std::string)];

    auto const first{reinterpret_cast<std::string*>(out)};
    auto const last{first + n};
    rah2::ranges::uninitialized_fill(first, last, "▄▀▄▀▄▀▄▀");

    assert(rah2::ranges::all_of(first, last, ([](auto& x) { return x == "▄▀▄▀▄▀▄▀"; })));

    rah2::ranges::destroy(first, last);

    /// [rah2::ranges::uninitialized_fill]
}
void test_uninitialized_fill_n()
{
    testSuite.test_case("sample");
    testSuite.test_case("return");
    /// [rah2::ranges::uninitialized_fill_n]

    constexpr int n{3};
    alignas(alignof(std::string)) char out[n * sizeof(std::string)];

    auto const first{reinterpret_cast<std::string*>(out)};
    auto const last = rah2::ranges::uninitialized_fill_n(first, n, "cppreference");

    assert(rah2::ranges::all_of(first, last, ([](auto& x) { return x == "cppreference"; })));

    rah2::ranges::destroy(first, last);

    /// [rah2::ranges::uninitialized_fill_n]
}
void test_uninitialized_move()
{
    testSuite.test_case("sample");
    // testSuite.test_case("return");
    /// [rah2::ranges::uninitialized_move]

    std::string in[]{"Home", "World"};

    constexpr auto sz = rah2::ranges::size(in);
    alignas(alignof(std::string)) char out[sz * sizeof(std::string)];
    auto const first{reinterpret_cast<std::string*>(out)};
    auto const last{first + sz};
    rah2::ranges::uninitialized_move(std::begin(in), std::end(in), first, last);
    assert(*first == "Home");
    assert(*rah2::ranges::next(first) == "World");
    rah2::ranges::destroy(first, last);
    /// [rah2::ranges::uninitialized_move]
}
void test_uninitialized_move_n()
{
    testSuite.test_case("sample");
    // testSuite.test_case("return");
    /// [rah2::ranges::uninitialized_move_n]

    std::string in[] = {"No", "Diagnostic", "Required"};

    constexpr auto sz = rah2::ranges::size(in);
    alignas(alignof(std::string)) char out[sz * sizeof(std::string)];
    auto const first{reinterpret_cast<std::string*>(out)};
    auto const last{first + sz};
    rah2::ranges::uninitialized_move_n(std::begin(in), sz, first, last);
    rah2::ranges::equal(
        rah2::ranges::make_subrange(first, last),
        std::initializer_list<std::string>{"No", "Diagnostic", "Required"});

    rah2::ranges::destroy(first, last);

    /// [rah2::ranges::uninitialized_move_n]
}
void test_uninitialized_default_construct()
{
    testSuite.test_case("sample");
    // testSuite.test_case("return");
    /// [rah2::ranges::uninitialized_default_construct]

    struct S
    {
        std::string m{"▄▀▄▀▄▀▄▀"};
    };

    constexpr int n{4};
    alignas(alignof(S)) char out[n * sizeof(S)];

    auto const first{reinterpret_cast<S*>(out)};
    auto const last{first + n};

    rah2::ranges::uninitialized_default_construct(first, last);

    assert(rah2::ranges::all_of(first, last, [](S& s) { return s.m == "▄▀▄▀▄▀▄▀"; }));

    rah2::ranges::destroy(first, last); // NOLINT(cppcoreguidelines-no-malloc)

    // Notice that for "trivial types" the uninitialized_default_construct
    // generally does not zero-fill the given uninitialized memory area.
    constexpr char etalon[]{'A', 'B', 'C', 'D', '\n'};
    char v[]{'A', 'B', 'C', 'D', '\n'};
    rah2::ranges::uninitialized_default_construct(std::begin(v), std::end(v));
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
        std::string m{"█▓▒░ █▓▒░ "};
    };

    constexpr int n{4};
    alignas(alignof(S)) char out[n * sizeof(S)];

    auto const first{reinterpret_cast<S*>(out)};
    auto const last = rah2::ranges::uninitialized_default_construct_n(first, n);
    assert(rah2::ranges::all_of(first, last, [](S& s) { return s.m == "█▓▒░ █▓▒░ "; }));

    rah2::ranges::destroy(first, last);

    // Notice that for "trivial types" the uninitialized_default_construct_n
    // generally does not zero-fill the given uninitialized memory area.
    constexpr int etalon[]{1, 2, 3, 4, 5, 6};
    int v[]{1, 2, 3, 4, 5, 6};
    rah2::ranges::uninitialized_default_construct_n(
        std::begin(v), static_cast<intptr_t>(rah2::ranges::size(v)));
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
        std::string m{"▄▀▄▀▄▀▄▀"};
    };

    constexpr int n{4};
    alignas(alignof(S)) char out[n * sizeof(S)];

    auto const first{reinterpret_cast<S*>(out)};
    auto const last{first + n};

    rah2::ranges::uninitialized_value_construct(first, last);
    assert(rah2::ranges::all_of(first, last, [](S& s) { return s.m == "▄▀▄▀▄▀▄▀"; }));

    rah2::ranges::destroy(first, last);
    // Notice that for "trivial types" the uninitialized_value_construct
    // zero-fills the given uninitialized memory area.
    int v[]{0, 1, 2, 3};
    rah2::ranges::uninitialized_value_construct(std::begin(v), std::end(v));
    assert(rah2::ranges::all_of(v, [](int i) { return i == 0; }));
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
        std::string m{"█▓▒░ █▓▒░ █▓▒░ "};
    };

    constexpr int n{4};
    alignas(alignof(S)) char out[n * sizeof(S)];

    auto const first{reinterpret_cast<S*>(out)};
    auto const last = rah2::ranges::uninitialized_value_construct_n(first, n);
    assert(rah2::ranges::all_of(first, last, [](S& s) { return s.m == "█▓▒░ █▓▒░ █▓▒░ "; }));

    rah2::ranges::destroy(first, last);

    // Notice that for "trivial types" the uninitialized_value_construct_n
    // zero-initializes the given uninitialized memory area.
    int v[]{1, 2, 3, 4, 5, 6, 7, 8};
    rah2::ranges::uninitialized_value_construct_n(
        std::begin(v), static_cast<intptr_t>(rah2::ranges::size(v)));
    assert(rah2::ranges::all_of(v, [](int i) { return i == 0; }));

    /// [rah2::ranges::uninitialized_value_construct_n]
}
void test_destroy()
{
    testSuite.test_case("sample");
    // testSuite.test_case("return");
    /// [rah2::ranges::destroy]
    struct tracer // NOLINT(cppcoreguidelines-special-member-functions)
    {
        int value;
        ~tracer()
        {
            value = 42;
        }
    };
    alignas(tracer) unsigned char buffer[sizeof(tracer) * 8];

    for (int i = 0; i < 8; ++i)
        new (buffer + sizeof(tracer) * i) tracer{i}; //manually construct objects

    auto const ptr = reinterpret_cast<tracer*>(buffer);

    rah2::ranges::destroy(ptr, ptr + 8);
    assert(rah2::ranges::all_of(ptr, ptr + 8, [](tracer const& t) { return t.value == 42; }));
    /// [rah2::ranges::destroy]
}
void test_destroy_n()
{
    testSuite.test_case("sample");
    // testSuite.test_case("return");
    /// [rah2::ranges::destroy_n]
    struct tracer // NOLINT(cppcoreguidelines-special-member-functions)
    {
        int value;
        ~tracer()
        {
            value = 42;
        }
    };

    alignas(tracer) unsigned char buffer[sizeof(tracer) * 8];

    for (int i = 0; i < 8; ++i)
        new (buffer + sizeof(tracer) * i) tracer{i}; //manually construct objects

    auto const ptr = reinterpret_cast<tracer*>(buffer);

    rah2::ranges::destroy_n(ptr, 8);
    assert(rah2::ranges::all_of(ptr, ptr + 8, [](tracer& t) { return t.value == 42; }));
    /// [rah2::ranges::destroy_n]
}
void test_destroy_at()
{
    testSuite.test_case("sample");
    // testSuite.test_case("return");
    /// [rah2::ranges::destroy_at]
    struct tracer // NOLINT(cppcoreguidelines-special-member-functions)
    {
        int value;
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
        rah2::ranges::destroy_at(ptr + i);
    assert(rah2::ranges::all_of(ptr, ptr + 8, [](tracer& t) { return t.value == 42; }));
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

    S* ptr = rah2::ranges::construct_at(reinterpret_cast<S*>(buf), 42, 2.71828f, 3.1415);
    assert(ptr->x == 42);

    rah2::ranges::destroy_at(ptr);
    /// [rah2::ranges::construct_at]
}
