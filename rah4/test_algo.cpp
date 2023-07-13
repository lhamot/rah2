#include <array>
#include <list>

#include "test_helpers.hpp"
#include "rah4.hpp"
#include "eastl_sort.h"

#if RAH_CPP20
#include <ranges>
#endif

void test_all_of()
{
    testSuite.test_case("sample");
    testSuite.test_case("yes");
    /// [rah::all_of]
    assert(rah::all_of(std::initializer_list<int>{4, 4, 4, 4}, [](auto a) { return a == 4; }));
    /// [rah::all_of]
    testSuite.test_case("no");
    assert(
        rah::all_of(std::initializer_list<int>{4, 4, 3, 4}, [](auto a) { return a == 4; }) == false);
}
void test_any_of()
{
    testSuite.test_case("sample");
    testSuite.test_case("yes");
    /// [rah::any_of]
    assert(rah::any_of(std::initializer_list<int>{3, 0, 1, 3, 4, 6}, [](auto a) { return a == 3; }));
    /// [rah::any_of]
}
void test_none_of()
{
    testSuite.test_case("sample");
    testSuite.test_case("yes");
    /// [rah::none_of]
    assert((rah::none_of(std::initializer_list<int>{7, 8, 9, 10}, [](auto a) { return a == 11; })));
    /// [rah::none_of]
}
void test_for_each()
{
    testSuite.test_case("sample");
    /// [rah::for_each]
    std::vector<int> testFE{4, 4, 4, 4};
    rah::for_each(testFE, [](auto& value) { return ++value; });
    EQUAL_RANGE(testFE, std::initializer_list<int>({5, 5, 5, 5}));
    /// [rah::for_each]
}
void test_for_each_n()
{
    testSuite.test_case("sample");
    /// [rah::for_each_n]
    std::vector<int> testFE{4, 4, 4, 4};
    auto res = rah::for_each_n(testFE.begin(), testFE.size(), [](auto& value) { return ++value; });
    EQUAL_RANGE(testFE, std::initializer_list<int>({5, 5, 5, 5}));
    assert(res.in == testFE.end());
    /// [rah::for_each_n]
}

void test_algo_count()
{
    testSuite.test_case("sample");
    testSuite.test_case("noproj");
    testSuite.test_case("proj");
    {
        /// [rah::count]
        assert(rah::count(std::initializer_list<int>{4, 4, 4, 3}, 3) == 1);
        struct Coord
        {
            int x;
            int y;
        };
        static auto coords = {Coord{1, 1}, {1, 2}, {2, 1}, {2, 3}, {3, 1}};
        assert(rah::count(coords, 1, &Coord::x) == 2);
        /// [rah::count]
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

        auto value = 2;
        auto getter_lbd = [](Coord const& c)
        {
            return c.x;
        };

        const auto count_rah_noproj = COMPUTE_DURATION(
            [&]
            {
                const size_t count = rah::count(intsVec.data(), intsVec.data() + intsVec.size(), 2);
                assert(count == 79);
            });
        std::cout << "count_rah_noproj : " << count_rah_noproj.count() << std::endl;
        const auto count_raw_noproj = COMPUTE_DURATION(
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
        std::cout << "count_raw_noproj : " << count_raw_noproj.count() << std::endl;
        assert(count_rah_noproj < count_raw_noproj * 1.2);
        const auto count_std_noproj = COMPUTE_DURATION(
            [&]
            {
                const size_t count = std::count(intsVec.data(), intsVec.data() + intsVec.size(), 2);
                assert(count == 79);
            });
        std::cout << "count_std_noproj : " << count_std_noproj.count() << std::endl;
        assert(count_rah_noproj < count_std_noproj * 1.2);
#if RAH_CPP20
        const auto count_rng_noproj = COMPUTE_DURATION(
            [&]
            {
                const size_t count =
                    std::ranges::count(intsVec.data(), intsVec.data() + intsVec.size(), 2);
                assert(count == 79);
            });
        std::cout << "count_rng_noproj : " << count_rng_noproj.count() << std::endl;
        assert(count_rah_noproj < count_rng_noproj * 1.2);
#endif
        const auto count_rah_proj = COMPUTE_DURATION(
            [&]
            {
                const size_t count =
                    rah::count(coordsVec.data(), coordsVec.data() + coordsVec.size(), 2, getter_lbd);
                assert(count == 79);
            });
        std::cout << "count_rah_proj : " << count_rah_proj.count() << std::endl;
        const auto count_raw_proj = COMPUTE_DURATION(
            [&]
            {
                size_t count = 0;
                auto i = coordsVec.data();
                auto e = coordsVec.data() + coordsVec.size();
                // for (Coord const& c : coordsVec)
                for (; i != e; ++i)
                {
                    if (getter_lbd(*i) == value)
                        ++count;
                }
                assert(count == 79);
            });
        std::cout << "count_raw_proj : " << count_raw_proj.count() << std::endl;
        assert(count_rah_proj < count_raw_proj * 1.2);

#if RAH_CPP20
        const auto count_rgn_proj = COMPUTE_DURATION(
            [&]
            {
                const size_t count = std::ranges::count(
                    coordsVec.data(), coordsVec.data() + coordsVec.size(), 2, &Coord::x);
                assert(count == 79);
            });
        std::cout << "count_rgn_proj : " << count_rgn_proj.count() << std::endl;
        assert(count_rah_proj < count_rgn_proj * 1.2);
#endif
    }
}
void test_count_if()
{
    testSuite.test_case("sample");
    /// [rah::count_if]
    assert(rah::count_if(std::initializer_list<int>{4, 4, 4, 3}, [](auto a) { return a == 4; }) == 3);
    /// [rah::count_if]

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
        auto pred = [](Coord const& c)
        {
            return c.x == 2;
        };

        const auto count_if_rah_pred = COMPUTE_DURATION(
            [&]
            {
                const size_t count =
                    rah::count_if(coordsVec.data(), coordsVec.data() + coordsVec.size(), pred);
                assert(count == 79);
            });
        std::cout << "count_if_rah_pred : " << count_if_rah_pred.count() << std::endl;

        const auto count_if_raw_pred = COMPUTE_DURATION(
            [&]
            {
                size_t count = 0;
                auto i = coordsVec.data();
                auto e = coordsVec.data() + coordsVec.size();
                for (; i != e; ++i)
                {
                    if (pred(*i))
                        ++count;
                }
                assert(count == 79);
            });
        std::cout << "count_if_raw_pred : " << count_if_raw_pred.count() << std::endl;
        assert(count_if_rah_pred < count_if_raw_pred * 1.2);

        auto count_if_std_pred = COMPUTE_DURATION(
            [&]
            {
                const size_t count =
                    std::count_if(coordsVec.data(), coordsVec.data() + coordsVec.size(), pred);
                assert(count == 79);
            });
        std::cout << "count_if_std_pred : " << count_if_std_pred.count() << std::endl;
        assert(count_if_rah_pred < count_if_std_pred * 1.2);
    }
}
void test_mismatch()
{
    testSuite.test_case("sample");
    /// [rah::mismatch]
    std::vector<int> in1 = {1, 2, 3, 4};
    std::vector<int> in2 = {1, 2, 42, 43};
    auto r1_r2 = rah::mismatch(in1, in2);
    assert(*r1_r2.in1 == 3);
    assert(*r1_r2.in2 == 42);
    /// [rah::mismatch]
}
void test_equal()
{
    testSuite.test_case("sample");
    /// [rah::equal]
    std::vector<int> in1{1, 2, 3};
    std::vector<int> in2{1, 2, 3};
    std::vector<int> in3{11, 12, 13};
    assert(rah::equal(in1, in2));
    assert(rah::equal(in1, in3) == false);
    /// [rah::equal]
}
void test_lexicographical_compare()
{
    testSuite.test_case("sample");
    /// [rah::lexicographical_compare]
    std::vector<char> v1{'a', 'b', 'c', 'd'};
    std::vector<char> v2{'a', 'x', 'y', 'z'};
    assert(rah::lexicographical_compare(v1, v1) == false);
    assert(rah::lexicographical_compare(v1, v2) == true);
    assert(rah::lexicographical_compare(v2, v1) == false);
    /// [rah::lexicographical_compare]
}
void test_find()
{
    testSuite.test_case("sample");
    /// [rah::find]
    std::vector<int> in{1, 2, 3, 4};
    auto iter = rah::find(in, 3);
    assert(rah::equal(rah::make_subrange(iter, end(in)), std::initializer_list<int>({3, 4})));
    /// [rah::find]
}
void test_find_if()
{
    testSuite.test_case("sample");
    /// [rah::find_if]
    std::vector<int> in{1, 2, 3, 4};
    auto iter = rah::find_if(in, [](int i) { return i == 3; });
    assert(rah::equal(rah::make_subrange(iter, end(in)), std::initializer_list<int>({3, 4})));
    /// [rah::find_if]
}
void test_find_if_not()
{
    testSuite.test_case("sample");
    /// [rah::find_if_not]
    std::vector<int> in{1, 2, 3, 4};
    auto iter = rah::find_if_not(in, [](int i) { return i < 3; });
    assert(rah::equal(rah::make_subrange(iter, end(in)), std::initializer_list<int>({3, 4})));
    /// [rah::find_if_not]
}
void test_find_last()
{
    testSuite.test_case("sample");
    /// [rah::find_last]
    static auto v = {1, 2, 3, 1, 2, 3, 1, 2};
    {
        auto i1 = rah::find_last(v.begin(), v.end(), 3);
        auto i2 = rah::find_last(v, 3);
        assert(rah::distance(v.begin(), i1.begin()) == 5);
        assert(rah::distance(v.begin(), i2.begin()) == 5);
    }
    {
        auto i1 = rah::find_last(v.begin(), v.end(), -3);
        auto i2 = rah::find_last(v, -3);
        assert(i1.begin() == v.end());
        assert(i2.begin() == v.end());
    }
    /// [rah::find_last]
}
void test_find_last_if()
{
    testSuite.test_case("sample");
    /// [rah::find_last_if]
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
        auto i1 = RAH_NAMESPACE::find_last_if(v.begin(), v.end(), pred, abs);
        auto i2 = RAH_NAMESPACE::find_last_if(v, pred, abs);
        assert(RAH_NAMESPACE::distance(v.begin(), i1.begin()) == 5);
        assert(RAH_NAMESPACE::distance(v.begin(), i2.begin()) == 5);
    }
    {
        auto pred = [](int x)
        {
            return x == -3;
        };
        auto i1 = RAH_NAMESPACE::find_last_if(v.begin(), v.end(), pred, abs);
        auto i2 = RAH_NAMESPACE::find_last_if(v, pred, abs);
        assert(i1.begin() == v.end());
        assert(i2.begin() == v.end());
    }
    /// [rah::find_last_if]
}
void test_find_last_if_not()
{
    testSuite.test_case("sample");
    /// [rah::find_last_if_not]
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
        auto i1 = RAH_NAMESPACE::find_last_if_not(v.begin(), v.end(), pred, abs);
        auto i2 = RAH_NAMESPACE::find_last_if_not(v, pred, abs);
        assert(RAH_NAMESPACE::distance(v.begin(), i1.begin()) == 5);
        assert(RAH_NAMESPACE::distance(v.begin(), i2.begin()) == 5);
    }
    {
        auto pred = [](int x)
        {
            return x == 1 or x == 2 or x == 3;
        };
        auto i1 = RAH_NAMESPACE::find_last_if_not(v.begin(), v.end(), pred, abs);
        auto i2 = RAH_NAMESPACE::find_last_if_not(v, pred, abs);
        assert(i1.begin() == v.end());
        assert(i2.begin() == v.end());
    }
    /// [rah::find_last_if_not]
}
void test_find_end()
{
    testSuite.test_case("sample");
    /// [rah::find_end]
    using namespace std::literals;
    std::string secret{"password password word..."};
    std::string wanted{"password"};

    auto found1 = RAH_NAMESPACE::find_end(secret.begin(), secret.end(), wanted.begin(), wanted.end());
    assert(found1.begin() == secret.begin() + intptr_t(strlen("password ")));
    assert(found1.end() == secret.begin() + intptr_t(strlen("password password")));

    auto found2 = RAH_NAMESPACE::find_end(secret, std::string("word"));
    assert(found2.begin() == secret.begin() + intptr_t(strlen("password password ")));
    assert(found2.end() == secret.begin() + intptr_t(strlen("password password word")));

    auto found3 = RAH_NAMESPACE::find_end(
        secret, std::string("ORD"), [](const char x, const char y) { // uses a binary predicate
            return std::tolower(x) == std::tolower(y);
        });
    assert(found3.begin() == secret.begin() + intptr_t(strlen("password password w")));
    assert(found3.end() == secret.begin() + intptr_t(strlen("password password word")));

    auto found4 = RAH_NAMESPACE::find_end(
        secret, std::string("SWORD"), {}, {}, [](char c) { return std::tolower(c); }); // projects the 2nd range
    assert(found4.begin() == secret.begin() + intptr_t(strlen("password pas")));
    assert(found4.end() == secret.begin() + intptr_t(strlen("password password")));

    assert(RAH_NAMESPACE::find_end(secret, std::string("PASS")).empty()); // => not found
    /// [rah::find_end]
}
void test_find_first_of()
{
    testSuite.test_case("sample");
    /// [rah::find_first_of]
    static auto haystack = {1, 2, 3, 4};
    static auto needles = {0, 3, 4, 3};

    auto found1 =
        rah::find_first_of(haystack.begin(), haystack.end(), needles.begin(), needles.end());
    assert(std::distance(haystack.begin(), found1) == 2);

    auto found2 = rah::find_first_of(haystack, needles);
    assert(std::distance(haystack.begin(), found2) == 2);

    static auto negatives = {-6, -3, -4, -3};
    auto not_found = rah::find_first_of(haystack, negatives);
    assert(not_found == haystack.end());

    auto found3 = rah::find_first_of(
        haystack, negatives, [](int x, int y) { return x == -y; }); // uses a binary comparator
    assert(std::distance(haystack.begin(), found3) == 2);

    struct P
    {
        int x, y;
    };
    static auto p1 = {P{1, -1}, P{2, -2}, P{3, -3}, P{4, -4}};
    static auto p2 = {P{5, -5}, P{6, -3}, P{7, -5}, P{8, -3}};

    // Compare only P::y data members by projecting them:
    const auto found4 = rah::find_first_of(p1, p2, {}, &P::y, &P::y);
    assert(std::distance(p1.begin(), found4) == 2); // {3, -3}
    /// [rah::find_first_of]
}
void test_adjacent_find()
{
    testSuite.test_case("sample");
    /// [rah::adjacent_find]
    const auto v = {0, 1, 2, 3, 40, 40, 41, 41, 5}; /*
                                                ^^          ^^       */
    {
        auto it = rah::adjacent_find(v.begin(), v.end());
        assert(rah::distance(v.begin(), it) == 4);
    }

    {
        auto it = rah::adjacent_find(v, rah::greater());
        assert(rah::distance(v.begin(), it) == 7);
    }
    /// [rah::adjacent_find]
}
void test_search()
{
    testSuite.test_case("sample");
    /// [rah::search]
    std::string haystack{"abcd abcd"};
    std::string needle{"bcd"};

    // the search uses iterator pairs begin()/end():
    auto found1 =
        RAH_NAMESPACE::search(haystack.begin(), haystack.end(), needle.begin(), needle.end());
    assert(found1.begin() - haystack.begin() == 1);
    assert(found1.end() - haystack.begin() == 4);

    // the search uses ranges r1, r2:
    auto found2 = RAH_NAMESPACE::search(haystack, needle);
    assert(found2.begin() - haystack.begin() == 1);
    assert(found2.end() - haystack.begin() == 4);
    // print(2, haystack, needle, found2);

    // 'needle' range is empty:
    std::string none{""};
    auto found3 = RAH_NAMESPACE::search(haystack, none);
    assert(found3.begin() - haystack.begin() == 0);
    assert(found3.end() - haystack.begin() == 0);
    // print(3, haystack, none, found3);

    // 'needle' will not be found:
    std::string awl{"efg"};
    auto found4 = RAH_NAMESPACE::search(haystack, awl);
    assert(found4.begin() - haystack.begin() == 9);
    assert(found4.end() - haystack.begin() == 9);
    // print(4, haystack, awl, found4);

    // the search uses custom comparator and projections:
    std::string bodkin{"234"};
    auto found5 = RAH_NAMESPACE::search(
        haystack,
        bodkin,
        [](const int x, const int y) { return x == y; }, // pred
        [](const int x) { return std::toupper(x); }, // proj1
        [](const int y) { return y + 'A' - '1'; }); // proj2
    assert(found5.begin() - haystack.begin() == 1);
    assert(found5.end() - haystack.begin() == 4);
    // print(5, haystack, bodkin, found5);
    /// [rah::search]
}
void test_search_n()
{
    testSuite.test_case("sample");
    /// [rah::search_n]
    auto nums = {1, 2, 2, 3, 4, 1, 2, 2, 2, 1};
    constexpr int count{3};
    constexpr int value{2};
    typedef int count_t, value_t;

    auto result1 = RAH_NAMESPACE::search_n(nums.begin(), nums.end(), count, value);
    assert( // found
        result1.size() == count && std::distance(nums.begin(), result1.begin()) == 6
        && std::distance(nums.begin(), result1.end()) == 9);

    auto result2 = RAH_NAMESPACE::search_n(nums, count, value);
    assert( // found
        result2.size() == count && std::distance(nums.begin(), result2.begin()) == 6
        && std::distance(nums.begin(), result2.end()) == 9);

    auto result3 = RAH_NAMESPACE::search_n(nums, count, value_t{5});
    assert( // not found
        result3.size() == 0 && result3.begin() == result3.end() && result3.end() == nums.end());

    auto result4 = RAH_NAMESPACE::search_n(nums, count_t{0}, value_t{1});
    assert( // not found
        result4.size() == 0 && result4.begin() == result4.end() && result4.end() == nums.begin());

    char symbol{'B'};
    auto to_ascii = [](const int z) -> char
    {
        return char('A' + z - 1);
    };
    auto is_equ = [](const char x, const char y)
    {
        return x == y;
    };

    auto result5 = RAH_NAMESPACE::search_n(nums, count, symbol, is_equ, to_ascii);
    assert(result5.begin() - nums.begin() == 6);
    /// [rah::search_n]
}
void test_contains()
{
    testSuite.test_case("sample");
    /// [rah::contains]
    constexpr auto haystack = std::array<int, 5>{3, 1, 4, 1, 5};
    auto increment = [](int x)
    {
        return ++x;
    };

    assert(RAH_NAMESPACE::contains(haystack, 4));
    assert(not RAH_NAMESPACE::contains(haystack, 6));
    assert(RAH_NAMESPACE::contains(haystack, 6, increment));
    assert(not RAH_NAMESPACE::contains(haystack, 1, increment));
    /// [rah::contains]
}
void test_contains_subrange()
{
    testSuite.test_case("sample");
    /// [rah::contains_subrange]
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

    assert(RAH_NAMESPACE::contains_subrange(haystack, needle));
    assert(not RAH_NAMESPACE::contains_subrange(haystack, bodkin));
    assert(RAH_NAMESPACE::contains_subrange(haystack, bodkin, {}, increment));
    assert(not RAH_NAMESPACE::contains_subrange(haystack, bodkin, {}, decrement));
    assert(RAH_NAMESPACE::contains_subrange(haystack, bodkin, {}, {}, decrement));
    /// [rah::contains_subrange]
}
void test_starts_with()
{
    testSuite.test_case("sample");
    /// [rah::starts_with]
    using namespace std::literals;

    auto ascii_upper = [](char c)
    {
        return 'a' <= c && c <= 'z' ? static_cast<char>(c + 'A' - 'a') : c;
    };

    auto cmp_ignore_case = [=](char x, char y)
    {
        return ascii_upper(x) == ascii_upper(y);
    };

    assert(rah::starts_with("const_cast", std::string("const")));
    assert(rah::starts_with("constexpr", std::string("const")));
    assert(!rah::starts_with("volatile", std::string("const")));

    assert(rah::starts_with(
        "Constantinopolis", std::string("constant"), {}, ascii_upper, ascii_upper));
    assert(not rah::starts_with(u8"Istanbul", std::string("constant"), {}, ascii_upper, ascii_upper));
    assert(rah::starts_with(u8"Metropolis", std::string("metro"), cmp_ignore_case));
    assert(not rah::starts_with(u8"Acropolis", std::string("metro"), cmp_ignore_case));

    auto v = {1, 3, 5, 7, 9};
    auto odd = [](int x)
    {
        return x % 2;
    };
    assert(rah::starts_with(v, rah::views::iota(1) | rah::views::filter(odd) | rah::views::take(3)));
    /// [rah::starts_with]
}
void test_ends_with()
{
    testSuite.test_case("sample");
    /// [rah::ends_with]
    assert(RAH_NAMESPACE::ends_with("static_cast", "cast"));
    assert(RAH_NAMESPACE::ends_with("const_cast", "cast"));
    assert(RAH_NAMESPACE::ends_with("reinterpret_cast", "cast"));
    assert(RAH_NAMESPACE::ends_with("dynamic_cast", "cast"));
    assert(not RAH_NAMESPACE::ends_with("move", "cast"));
    assert(not RAH_NAMESPACE::ends_with("move_if_noexcept", "cast"));
    assert(not RAH_NAMESPACE::ends_with("forward", "cast"));
    assert(!RAH_NAMESPACE::ends_with("as_const", "cast"));
    assert(!!RAH_NAMESPACE::ends_with("bit_cast", "cast"));
    assert(!RAH_NAMESPACE::ends_with("to_underlying", "cast"));
    assert(!!RAH_NAMESPACE::ends_with(std::vector<int>{1, 2, 3, 4}, std::vector<int>{3, 4}));
    assert(!RAH_NAMESPACE::ends_with(std::vector<int>{1, 2, 3, 4}, std::vector<int>{4, 5}));
    /// [rah::ends_with]
}
void test_copy()
{
    testSuite.test_case("sample");
    /// [rah::copy]
    std::vector<int> in{1, 2, 3};
    std::vector<int> out{0, 0, 0, 4, 5};
    assert(rah::equal(
        rah::make_subrange(rah::copy(in, out.begin()).out, end(out)),
        std::initializer_list<int>({4, 5})));
    assert(out == (std::vector<int>{1, 2, 3, 4, 5}));
    /// [rah::copy]
}
void test_copy_if()
{
    testSuite.test_case("sample");
    /// [rah::copy_if]
    std::vector<int> in{1, 2, 3, 4};
    std::vector<int> out{0, 0, 5, 6};
    assert(rah::equal(
        rah::make_subrange(
            rah::copy_if(in, out.begin(), [](int i) { return i % 2 == 0; }).out, end(out)),
        std::initializer_list<int>({5, 6})));
    assert(out == (std::vector<int>{2, 4, 5, 6}));
    /// [rah::copy_if]
}
void test_copy_n()
{
    testSuite.test_case("sample");
    /// [rah::copy_n]
    const std::string in{"ABCDEFGH"};
    std::string out;

    rah::copy_n(in.begin(), 4, std::back_inserter(out));
    assert(out == "ABCD");

    out = "abcdefgh";
    const auto res = rah::copy_n(in.begin(), 5, out.begin());
    assert(*(res.in) == 'F');
    assert(*(res.out) == 'f');
    assert(std::distance(std::begin(in), res.in) == 5);
    assert(std::distance(std::begin(out), res.out) == 5);
    /// [rah::copy_n]
}
void test_copy_backward()
{
    testSuite.test_case("sample");
    /// [rah::copy_backward]
    const auto src = {1, 2, 3, 4};

    std::vector<int> dst(src.size() + 2);
    rah::copy_backward(src, dst.end());
    assert(dst == (std::vector<int>{0, 0, 1, 2, 3, 4}));

    rah::fill(dst, 0);
    const auto in_out = rah::copy_backward(src.begin(), src.end() - 2, dst.end());
    assert(dst == (std::vector<int>{0, 0, 0, 0, 1, 2}));

    assert(rah::distance(src.begin(), in_out.in) == 2);
    assert(rah::distance(dst.begin(), in_out.out) == 4);
    /// [rah::copy_backward]
}
void test_move()
{
    testSuite.test_case("sample");
    /// [rah::move]
    struct NonCopyable
    {
        NonCopyable() = default;
        NonCopyable(NonCopyable const&) = delete;
        NonCopyable& operator=(NonCopyable const&) = delete;
        NonCopyable(NonCopyable&&) = default;
        NonCopyable& operator=(NonCopyable&&) = default;
    };
    std::vector<NonCopyable> v;
    v.emplace_back();
    v.emplace_back();
    v.emplace_back();

    std::list<NonCopyable> l;
    rah::move(v, std::back_inserter(l));
    /// [rah::move]
}
void test_move_backward()
{
}
void test_fill()
{
    testSuite.test_case("sample");
    /// [rah::fill]
    std::vector<int> out{0, 0, 0, 4, 5};
    rah::fill(out, 42);
    assert(out == (std::vector<int>{42, 42, 42, 42, 42}));
    /// [rah::fill]
}
void test_fill_n()
{
}
void test_transform()
{
    testSuite.test_case("sample");
    {
        /// [rah::transform]
        std::vector<int> vecIn1{0, 1, 2, 3};
        std::vector<int> vecOut{0, 0, 0, 0};
        rah::transform(vecIn1, begin(vecOut), [](int a) { return a + 1; });
        assert(vecOut == std::vector<int>({1, 2, 3, 4}));
        /// [rah::transform]
    }
    {
        /// [rah::transform2]
        std::vector<int> vecIn1{0, 1, 2, 3};
        std::vector<int> vecIn2{4, 3, 2, 1};
        std::vector<int> vecOut;
        rah::transform(
            vecIn1, vecIn2, std::back_inserter(vecOut), [](int a, int b) { return a + b; });
        assert(vecOut == std::vector<int>({4, 4, 4, 4}));
        /// [rah::transform2]
    }
}
void test_generate()
{
}
void test_generate_n()
{
}
void test_remove()
{
    testSuite.test_case("sample");
    /// [rah::remove]
    std::vector<int> in{1, 2, 1, 3, 1};
    auto to_erase = rah::remove(in, 1);
    in.erase(to_erase.begin(), to_erase.end());
    std::sort(in.begin(), in.end());
    assert(in == std::vector<int>({2, 3}));
    /// [rah::remove]
}
void test_remove_if()
{
    testSuite.test_case("sample");
    /// [rah::remove_if]
    std::vector<int> in{1, 2, 3, 4, 5};
    auto to_erase = rah::remove_if(in, [](auto a) { return a < 4; });
    in.erase(to_erase.begin(), to_erase.end());
    std::sort(in.begin(), in.end());
    assert(in == std::vector<int>({4, 5}));
    /// [rah::remove_if]
}
void test_remove_copy()
{
}
void test_remove_copy_if()
{
}
void test_replace()
{
}
void test_replace_if()
{
}
void test_replace_copy()
{
}
void test_replace_copy_if()
{
}
void test_swap_ranges()
{
}
void test_reverse()
{
}
void test_reverse_copy()
{
}
void test_rotate()
{
}
void test_rotate_copy()
{
}
void test_shuffle()
{
    testSuite.test_case("sample");
    /// [rah::shuffle]
    std::random_device rd;
    std::mt19937 g(rd());
    std::vector<int> in{1, 2, 3, 4, 5, 6};
    rah::shuffle(in, g);
    /// [rah::shuffle]
}
void test_shift_left()
{
}
void test_shift_right()
{
}
void test_sample()
{
}
void test_unique()
{
    testSuite.test_case("sample");
    {
        /// [rah::unique]
        std::vector<int> in{2, 1, 1, 1, 5, 3, 3, 4};
        in.erase(rah::unique(in).begin(), end(in));
        assert(in == std::vector<int>({2, 1, 5, 3, 4}));
        /// [rah::unique]
    }
    {
        /// [rah::unique_pred]
        std::vector<int> in{2, 1, 1, 1, 5, 3, 3, 4};
        in.erase(rah::unique(in, [](auto a, auto b) { return a == b; }).begin(), end(in));
        assert(in == std::vector<int>({2, 1, 5, 3, 4}));
        /// [rah::unique_pred]
    }
}
void test_unique_copy()
{
}
void test_is_partitioned()
{
}
void test_partition()
{
    testSuite.test_case("sample");
    /// [rah::partition]
    std::vector<int> in{1, 2, 3, 4, 5};
    auto boundary = rah::partition(in, [](auto a) { return a >= 4; });
    assert(boundary.begin() == in.begin() + 2);
    std::sort(in.begin(), boundary.begin());
    std::sort(boundary.begin(), in.end());
    assert(in == std::vector<int>({4, 5, 1, 2, 3}));
    /// [rah::partition]
}
void test_partition_copy()
{
}
void test_stable_partition()
{
    testSuite.test_case("sample");
    /// [rah::stable_partition]
    std::vector<int> in{1, 2, 3, 4, 5};
    auto boundary = rah::stable_partition(in, [](auto a) { return a >= 4; });
    assert(boundary == in.begin() + 2);
    assert(in == std::vector<int>({4, 5, 1, 2, 3}));
    /// [rah::stable_partition]
}
void test_partition_point()
{
}
void test_is_sorted()
{
}
void test_is_sorted_until()
{
}
void test_sort()
{
    testSuite.test_case("sample");
    {
        /// [rah::sort]
        std::vector<int> in{2, 1, 5, 3, 4};
        rah::sort(in);
        assert(in == std::vector<int>({1, 2, 3, 4, 5}));
        /// [rah::sort]
    }
    {
        /// [rah::sort_pred]
        std::vector<int> in{2, 1, 5, 3, 4};
        rah::sort(in, [](auto a, auto b) { return a < b; });
        assert(in == std::vector<int>({1, 2, 3, 4, 5}));
        /// [rah::sort_pred]
    }
}
void test_partial_sort()
{
}
void test_partial_sort_copy()
{
}
void test_stable_sort()
{
    testSuite.test_case("sample");
    /// [rah::stable_sort]
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
        rah::stable_sort(in);
        assert(in == std::vector<CmpA>({{1, 1}, {2, 1}, {2, 2}, {4, 1}, {4, 2}, {4, 3}, {4, 4}}));
    }
    /// [rah::stable_sort]
    {
        /// [rah::stable_sort_pred]
        std::vector<std::pair<int, int>> in{{4, 1}, {2, 1}, {4, 2}, {1, 1}, {4, 3}, {2, 2}, {4, 4}};
        rah::stable_sort(in, [](auto l, auto r) { return l.second < r.second; });
        assert(
            in
            == (std::vector<std::pair<int, int>>(
                {{4, 1}, {2, 1}, {1, 1}, {4, 2}, {2, 2}, {4, 3}, {4, 4}})));
        /// [rah::stable_sort_pred]
    }
}
void test_nth_element()
{
}
void test_lower_bound()
{
}
void test_upper_bound()
{
}
void test_binary_search()
{
    testSuite.test_case("sample");
    /// [rah::binary_search]
    std::vector<int> vecIn1{1, 2, 2, 3, 4};
    assert(not rah::binary_search(vecIn1, 0));
    assert(rah::binary_search(vecIn1, 1));
    assert(rah::binary_search(vecIn1, 2));
    /// [rah::binary_search]
}
void test_equal_range()
{
    testSuite.test_case("sample");
    {
        /// [rah::equal_range]
        std::vector<int> vecIn1{1, 2, 2, 3, 4};
        {
            std::vector<int> out;
            for (int i : rah::equal_range(vecIn1, 0))
                out.push_back(i);
            assert(out == std::vector<int>({}));
        }
        {
            std::vector<int> out;
            for (int i : rah::equal_range(vecIn1, 1))
                out.push_back(i);
            assert(out == std::vector<int>({1}));
        }
        {
            std::vector<int> out;
            for (int i : rah::equal_range(vecIn1, 2))
                out.push_back(i);
            assert(out == std::vector<int>({2, 2}));
        }
        /// [rah::equal_range]
    }
    {
        /// [rah::equal_range_pred_0]
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
        /// [rah::equal_range_pred_0]
        {
            /// [rah::equal_range_pred]
            std::vector<S> vecIn1{{1, 'a'}, {2, 'b'}, {2, 'c'}, {3, 'd'}, {4, 'd'}};
            {
                std::vector<S> out;
                for (S i : rah::equal_range(vecIn1, 0, FindS{}))
                    out.push_back(i);
                assert(out == std::vector<S>({}));
            }
            {
                std::vector<S> out;
                for (S i : rah::equal_range(vecIn1, 1, FindS{}))
                    out.push_back(i);
                assert(out == std::vector<S>({{1, 'a'}}));
            }
            {
                std::vector<S> out;
                for (S i : rah::equal_range(vecIn1, 2, FindS{}))
                    out.push_back(i);
                assert(out == std::vector<S>({{2, 'b'}, {2, 'c'}}));
            }
            /// [rah::equal_range_pred]
        }
    }
}
void test_merge()
{
}
void test_inplace_merge()
{
}
void test_includes()
{
}
void test_set_difference()
{
    testSuite.test_case("sample");
    /// [rah::set_difference]
    std::vector<int> in1{1, 3, 4};
    std::vector<int> in2{1, 2, 3};
    std::vector<int> out{0, 0, 0, 0};
    rah::set_difference(in1, in2, out.begin());
    assert(out == std::vector<int>({4, 0, 0, 0}));
    /// [rah::set_difference]
}
void test_set_intersection()
{
    testSuite.test_case("sample");
    /// [rah::set_intersection]
    std::vector<int> in1{1, 3, 4};
    std::vector<int> in2{1, 2, 3};
    std::vector<int> out{0, 0, 0, 0};
    rah::set_intersection(in1, in2, out.begin());
    assert(out == std::vector<int>({1, 3, 0, 0}));
    /// [rah::set_intersection]
}
void test_set_symmetric_difference()
{
}
void test_set_union()
{
}
void test_is_heap()
{
}
void test_is_heap_until()
{
}
void test_make_heap()
{
}
void test_push_heap()
{
}
void test_pop_heap()
{
}
void test_sort_heap()
{
}
void test_max()
{
}
void test_max_element()
{
    testSuite.test_case("sample");
    {
        /// [rah::max_element]
        std::vector<int> in{1, 5, 3, 4};
        auto iter = rah::max_element(in);
        assert(*iter == 5);
        /// [rah::max_element]
    }
    {
        /// [rah::max_element_pred]
        std::vector<std::pair<int, int>> in{{100, 3}, {0, 5}, {0, 1}, {0, 4}};
        auto iter = rah::max_element(in, [](auto&& a, auto& b) { return a.second < b.second; });
        assert(*iter == (std::pair<int, int>{0, 5}));
        /// [rah::max_element_pred]
    }
}
void test_min()
{
}
void test_min_element()
{
    testSuite.test_case("sample");
    {
        /// [rah::min_element]
        std::vector<int> in{1, -5, 3, 4};
        auto iter = rah::min_element(in);
        assert(*iter == -5);
        /// [rah::min_element]
    }

    {
        /// [rah::min_element_pred]
        std::vector<std::pair<int, int>> in{{-100, 3}, {0, -5}, {0, 1}, {0, 4}};
        auto iter = rah::min_element(in, [](auto&& a, auto& b) { return a.second < b.second; });
        assert(*iter == (std::pair<int, int>{0, -5}));
        /// [rah::min_element_pred]
    }
}
void test_minmax()
{
}
void test_minmax_element()
{
}
void test_clamp()
{
    testSuite.test_case("sample");
    /// [rah::clamp]
    assert(rah::clamp(0, 4, 8) == 4);
    assert(rah::clamp(4, 4, 8) == 4);
    assert(rah::clamp(6, 4, 8) == 6);
    assert(rah::clamp(8, 4, 8) == 8);
    assert(rah::clamp(10, 4, 8) == 8);
    /// [rah::clamp]

    testSuite.test_case("comp");
    assert(rah::clamp(0, 8, 4, std::greater<>()) == 4);
    assert(rah::clamp(4, 8, 4, std::greater<>()) == 4);
    assert(rah::clamp(6, 8, 4, std::greater<>()) == 6);
    assert(rah::clamp(8, 8, 4, std::greater<>()) == 8);
    assert(rah::clamp(10, 8, 4, std::greater<>()) == 8);
}
void test_is_permutation()
{
    testSuite.test_case("sample");
    /// [rah::is_permutation]
    static constexpr auto r1 = {1, 2, 3, 4, 5};
    static constexpr auto r2 = {3, 5, 4, 1, 2};
    assert(rah::is_permutation(r1, r1));
    assert(rah::is_permutation(r1, r2));
    assert(rah::is_permutation(r2, r1));
    assert(rah::is_permutation(r1.begin(), r1.end(), r2.begin(), r2.end()));
    /// [rah::is_permutation]
}
void test_next_permutation()
{
}
void test_prev_permutation()
{
}
void test_iota()
{
}
void test_fold_left()
{
    testSuite.test_case("sample");
    /// [rah::fold_left]
    std::vector<int> vecIn1{1, 2, 3, 4};
    assert(rah::fold_left(vecIn1, 0, [](auto a, auto b) { return a + b; }) == 10);
    /// [rah::fold_left]
}
void test_fold_left_first()
{
}
void test_fold_right()
{
}
void test_fold_right_last()
{
}
void test_fold_left_with_iter()
{
}
void test_fold_left_first_with_iter()
{
}
void test_uninitialized_copy()
{
}
void test_uninitialized_copy_n()
{
}
void test_uninitialized_fill()
{
}
void test_uninitialized_fill_n()
{
}
void test_uninitialized_move()
{
}
void test_uninitialized_move_n()
{
}
void test_uninitialized_default_construct()
{
}
void test_uninitialized_default_construct_n()
{
}
void test_uninitialized_value_construct()
{
}
void test_uninitialized_value_construct_n()
{
}
void test_destroy()
{
}
void test_destroy_n()
{
}
void test_destroy_at()
{
}
void test_construct_at()
{
}
