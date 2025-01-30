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
struct test_uninitialized_copy_
{
    template <bool = true>
    void test()
    {
        RAH2_STD::vector<RAH2_STD::string> in_{"11", "22", "33"};
        auto in = make_test_view_adapter<CS, Tag, Sized>(in_);
        {
            alignas(alignof(RAH2_STD::string)) uint8_t out_[sizeof(RAH2_STD::string) * 5];
            auto out = make_test_view_adapter<CS, Tag, Sized>(RAH2_NS::ranges::make_subrange(
                reinterpret_cast<RAH2_STD::string*>(out_),
                reinterpret_cast<RAH2_STD::string*>(out_) + 5));
            testSuite.test_case("iter");
            auto result = RAH2_NS::ranges::uninitialized_copy(
                RAH2_NS::ranges::begin(in),
                RAH2_NS::ranges::end(in),
                out.begin(),
                out.end());
            CHECK(result.out == RAH2_NS::ranges::next(out.begin(), in_.size()));
            CHECK(result.in == in.end());
            for (size_t i = 0; i < in_.size(); ++i)
            {
                auto const strptr = (&(*out.begin())) + i;
                CHECK_EQUAL(*strptr, in_[i]);
                strptr->~basic_string();
            }
        }

        testSuite.test_case("range");
        {
            alignas(alignof(RAH2_STD::string)) uint8_t out_[sizeof(RAH2_STD::string) * 5];
            auto out = make_test_view_adapter<CS, Tag, Sized>(RAH2_NS::ranges::make_subrange(
                reinterpret_cast<RAH2_STD::string*>(out_),
                reinterpret_cast<RAH2_STD::string*>(out_) + 5));
            auto result2 = RAH2_NS::ranges::uninitialized_copy(in, out);
            CHECK(result2.out == RAH2_NS::ranges::next(out.begin(), in_.size()));
            CHECK(result2.in == in.end());
            for (size_t i = 0; i < in_.size(); ++i)
            {
                auto const strptr = (&(*out.begin())) + i;
                CHECK_EQUAL(*strptr, in_[i]);
                strptr->~basic_string();
            }
        }

        testSuite.test_case("empty");
        {
            alignas(alignof(RAH2_STD::string)) uint8_t out_[sizeof(RAH2_STD::string) * 5];
            auto out = make_test_view_adapter<CS, Tag, Sized>(RAH2_NS::ranges::make_subrange(
                reinterpret_cast<RAH2_STD::string*>(out_),
                reinterpret_cast<RAH2_STD::string*>(out_) + 5));
            RAH2_STD::vector<RAH2_STD::string> empty_in_;
            auto empty_in = make_test_view_adapter<CS, Tag, Sized>(empty_in_);
            auto result3 = RAH2_NS::ranges::uninitialized_copy(empty_in, out);
            CHECK(result3.out == RAH2_NS::ranges::next(out.begin(), empty_in_.size()));
            CHECK(result3.in == empty_in.end());
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
        {
            COMPARE_DURATION_TO_STD_ALGO_AND_RANGES_2(
                CS == Common,
                "uninitialized_copy_iter",
                range_type,
                (
                    [&]
                    {
                        for (size_t i = 0; i < 5; ++i)
                        {
                            auto result2 = STD::uninitialized_copy(fwd(in.begin()), in.end(), out.begin());
                            CHECK(result2 == out.end());
                        }
                    }),
                (
                    [&]
                    {
                        for (size_t i = 0; i < 5; ++i)
                        {
                            auto result2 = STD::uninitialized_copy(
                                fwd(in.begin()), in.end(), out.begin(), out.end());
                            CHECK(result2.out == out.begin() + in_.size());
                            CHECK(result2.in == in.end());
                        }
                    }));
        }
        {
            COMPARE_DURATION_TO_STD_RANGES(
                "uninitialized_copy_ranges",
                range_type,
                (
                    [&]
                    {
                        for (size_t i = 0; i < 5; ++i)
                        {
                            auto result2 = STD::uninitialized_copy(in, out);
                            CHECK(result2.out == out.begin() + in_.size());
                            CHECK(result2.in == in.end());
                        }
                    }));
        }
    }
    static constexpr bool do_test = RAH2_NS::derived_from<Tag, RAH2_NS::contiguous_iterator_tag>;
};
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
    auto result =
        RAH2_NS::ranges::uninitialized_copy(RAH2_STD::begin(v), RAH2_STD::end(v), first, last);
    assert(result.out == first + sz);
    assert(result.in == RAH2_STD::end(v));

    for (size_t i = 0; i < 4; ++i)
        assert(v[i] == first[i]);

    RAH2_NS::ranges::destroy(first, last); // NOLINT(cppcoreguidelines-no-malloc)

    /// [rah2::ranges::uninitialized_copy]

    foreach_range_combination<test_algo<test_uninitialized_copy_>>();
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
