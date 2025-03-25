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
struct test_rotate_
{
    template <bool = true>
    void test()
    {
        {
            testSuite.test_case("iter");
            RAH2_STD::vector<int> out_{1, 2, 3, 4, 5};
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto middle = RAH2_NS::ranges::next(out.begin(), 2);
            auto result = RAH2_NS::ranges::rotate(out.begin(), middle, out.end());
            CHECK(RAH2_NS::ranges::begin(result) == RAH2_NS::ranges::next(out.begin(), 3));
            CHECK(RAH2_NS::ranges::end(result) == RAH2_NS::end(out));
            CHECK(out_ == (RAH2_STD::vector<int>{3, 4, 5, 1, 2}));
        }
        {
            testSuite.test_case("range");
            RAH2_STD::vector<int> out_{1, 2, 3, 4, 5, 6};
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto middle = RAH2_NS::ranges::next(out.begin(), 2);
            auto result = RAH2_NS::ranges::rotate(out, middle);
            CHECK(RAH2_NS::begin(result) == RAH2_NS::ranges::next(out.begin(), 4));
            CHECK(RAH2_NS::end(result) == RAH2_NS::end(out));
            CHECK(out_ == (RAH2_STD::vector<int>{3, 4, 5, 6, 1, 2}));
        }
        testSuite.test_case("non_pod");
        {
            RAH2_STD::vector<std::string> out_{"1", "2", "3", "4", "5"};
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto middle = RAH2_NS::ranges::next(out.begin(), 2);
            auto result = RAH2_NS::ranges::rotate(out.begin(), middle, out.end());
            CHECK(RAH2_NS::ranges::begin(result) == RAH2_NS::ranges::next(out.begin(), 3));
            CHECK(RAH2_NS::ranges::end(result) == RAH2_NS::end(out));
            CHECK(out_ == (RAH2_STD::vector<std::string>{"3", "4", "5", "1", "2"}));
        }
        {
            RAH2_STD::vector<std::string> out_{"1", "2", "3", "4", "5", "6"};
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto middle = RAH2_NS::ranges::next(out.begin(), 2);
            auto result = RAH2_NS::ranges::rotate(out, middle);
            CHECK(RAH2_NS::begin(result) == RAH2_NS::ranges::next(out.begin(), 4));
            CHECK(RAH2_NS::end(result) == RAH2_NS::end(out));
            CHECK(out_ == (RAH2_STD::vector<std::string>{"3", "4", "5", "6", "1", "2"}));
        }

        {
            testSuite.test_case("empty");
            RAH2_STD::vector<int> out_{};
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto result = RAH2_NS::ranges::rotate(out.begin(), out.begin(), out.end());
            CHECK(RAH2_NS::begin(result) == RAH2_NS::end(out));
            CHECK(RAH2_NS::end(result) == RAH2_NS::end(out));
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
                "rotate_iter",
                range_type,
                [&]
                {
                    STD::rotate(
                        RAH2_NS::ranges::begin(fwd(out)),
                        ++RAH2_NS::ranges::begin(fwd(out)),
                        RAH2_NS::ranges::end(out));
                    CHECK(out.front() == 0);
                });
        }

        {
            COMPARE_DURATION_TO_STD_RANGES(
                "rotate_ranges",
                range_type,
                (
                    [&]
                    {
                        auto result2 = STD::rotate(out, ++RAH2_NS::ranges::begin(out));
                        DONT_OPTIM(result2);
                    }));
        }
    }
    static constexpr bool do_test = RAH2_NS::derived_from<Tag, RAH2_NS::forward_iterator_tag>;
};
void test_rotate()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::rotate]
    RAH2_STD::vector<int> vec = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
    auto middle = vec.begin() + 3;
    RAH2_NS::ranges::rotate(vec, middle);
    assert((vec == RAH2_STD::vector<int>{4, 5, 6, 7, 8, 9, 10, 1, 2, 3}));
    /// [rah2::ranges::rotate]

    foreach_range_combination<test_algo<test_rotate_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_rotate_copy_
{
    template <bool = true>
    void test()
    {
        {
            testSuite.test_case("iter");
            RAH2_STD::vector<int> in_{1, 2, 3, 4, 5};
            auto in = make_test_view_adapter<CS, Tag, Sized>(in_);
            RAH2_STD::vector<int> out{0, 0, 0, 0, 0};
            auto middle = RAH2_NS::ranges::next(in.begin(), 2);
            auto result = RAH2_NS::ranges::rotate_copy(in.begin(), middle, in.end(), out.begin());
            CHECK(result.in == in.end());
            CHECK(result.out == out.end());
            CHECK(out == (RAH2_STD::vector<int>{3, 4, 5, 1, 2}));
        }

        {
            testSuite.test_case("range");
            RAH2_STD::vector<int> in_{1, 2, 3, 4, 5, 6};
            auto in = make_test_view_adapter<CS, Tag, Sized>(in_);
            RAH2_STD::vector<int> out{0, 0, 0, 0, 0, 0};
            auto middle = RAH2_NS::ranges::next(in.begin(), 2);
            auto result = RAH2_NS::ranges::rotate_copy(in, middle, out.begin());
            CHECK(result.in == in.end());
            CHECK(result.out == out.end());
            CHECK(out == (RAH2_STD::vector<int>{3, 4, 5, 6, 1, 2}));
        }

        {
            testSuite.test_case("empty");
            RAH2_STD::vector<int> in_{};
            auto in = make_test_view_adapter<CS, Tag, Sized>(in_);
            RAH2_STD::vector<int> out{};
            auto result = RAH2_NS::ranges::rotate_copy(in, in.begin(), out.begin());
            CHECK(result.in == in.end());
            CHECK(result.out == out.end());
            CHECK(out == (RAH2_STD::vector<int>{}));
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
        auto middle = RAH2_NS::ranges::next(in.begin(), 10);
        RAH2_STD::vector<int> out;
        out.resize(1000000 * RELEASE_MULTIPLIER);
        out.emplace_back();
        out.emplace_back();
        {
            COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(
                CS == CommonOrSent::Common,
                "rotate_copy_iter",
                range_type,
                [&]
                {
                    auto result = STD::rotate_copy(
                        RAH2_NS::ranges::begin(fwd(in)), middle, RAH2_NS::ranges::end(in), out.begin());
                    DONT_OPTIM(result);
                });
        }

        {
            COMPARE_DURATION_TO_STD_RANGES(
                "rotate_copy_ranges",
                range_type,
                (
                    [&]
                    {
                        auto result2 = STD::rotate_copy(in, middle, out.begin());
                        CHECK(result2.out == out.begin() + in_.size());
                        CHECK(result2.in == in.end());
                    }));
        }
    }
    static constexpr bool do_test = RAH2_NS::derived_from<Tag, RAH2_NS::bidirectional_iterator_tag>;
};
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

    foreach_range_combination<test_algo<test_rotate_copy_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_shuffle_
{
    template <bool = true>
    void test()
    {
        {
            testSuite.test_case("iter");
            std::random_device rd;
            std::mt19937 g(rd());
            RAH2_STD::vector<int> out_{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15};
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            RAH2_NS::ranges::shuffle(out.begin(), out.end(), g);
            CHECK(out_ != (RAH2_STD::vector<int>{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15}));
        }

        {
            testSuite.test_case("range");
            std::random_device rd;
            std::mt19937 g(rd());
            RAH2_STD::vector<int> out_{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15};
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            RAH2_NS::ranges::shuffle(out, g);
            CHECK(out_ != (RAH2_STD::vector<int>{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15}));
        }

        {
            testSuite.test_case("empty");
            std::random_device rd;
            std::mt19937 g(rd());
            RAH2_STD::vector<int> out_{};
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            RAH2_NS::ranges::shuffle(out.begin(), out.end(), g);
            CHECK(out_.empty());
        }
    }

    template <bool = true>
    void test_perf(char const* range_type)
    {
        testSuite.test_case("perf");
        RAH2_STD::vector<uint32_t> out_;
        out_.resize(1000000 * RELEASE_MULTIPLIER);
        out_.emplace_back();
        out_.emplace_back();
        auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
        std::random_device rd;
        std::mt19937 g(rd());

#ifdef RAH2_USE_EASTL
        {
            COMPARE_DURATION_TO_STD_RANGES(
                "shuffle_iter",
                range_type,
                [&]
                {
                    STD::shuffle(RAH2_NS::ranges::begin(fwd(out)), RAH2_NS::ranges::end(out), g);
                    CHECK(out.front() == 0);
                });
        }
#else
        {
            COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(
                CS == CommonOrSent::Common,
                "shuffle_iter",
                range_type,
                [&]
                {
                    STD::shuffle(RAH2_NS::ranges::begin(fwd(out)), RAH2_NS::ranges::end(out), g);
                    CHECK(out.front() == 0);
                });
        }
#endif

        {
            COMPARE_DURATION_TO_STD_RANGES(
                "shuffle_ranges",
                range_type,
                (
                    [&]
                    {
                        STD::shuffle(out, g);
                        CHECK(out.front() == 0);
                    }));
        }
    }
    static constexpr bool do_test = RAH2_NS::derived_from<Tag, RAH2_NS::random_access_iterator_tag>;
};
void test_shuffle()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::shuffle]
    std::random_device rd;
    std::mt19937 g(rd());
    RAH2_STD::vector<uint32_t> in{1, 2, 3, 4, 5, 6};
    RAH2_NS::ranges::shuffle(in, g);
    /// [rah2::ranges::shuffle]

    foreach_range_combination<test_algo<test_shuffle_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_shift_left_
{
    template <bool = true>
    void test()
    {
        {
            testSuite.test_case("iter");
            RAH2_STD::vector<int> out_{1, 2, 3, 4, 5};
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto result = RAH2_NS::ranges::shift_left(out.begin(), out.end(), 3);
            CHECK(RAH2_NS::ranges::begin(result) == RAH2_NS::ranges::next(out.begin(), 0));
            CHECK(RAH2_NS::ranges::end(result) == RAH2_NS::ranges::next(out.begin(), 2));
            CHECK(out_ == (RAH2_STD::vector<int>{4, 5, 3, 4, 5}));
        }
        {
            testSuite.test_case("range");
            RAH2_STD::vector<int> out_{1, 2, 3, 4, 5, 6};
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto result = RAH2_NS::ranges::shift_left(out, 4);
            CHECK(RAH2_NS::ranges::begin(result) == RAH2_NS::ranges::next(out.begin(), 0));
            CHECK(RAH2_NS::ranges::end(result) == RAH2_NS::ranges::next(out.begin(), 2));
            CHECK(out_ == (RAH2_STD::vector<int>{5, 6, 3, 4, 5, 6}));
        }

        {
            RAH2_STD::vector<int> out_{1, 2, 3, 4, 5, 6};
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto result = RAH2_NS::ranges::shift_left(out, 9);
            CHECK(RAH2_NS::ranges::begin(result) == out.begin());
            CHECK(RAH2_NS::ranges::end(result) == out.begin());
            CHECK(out_ == (RAH2_STD::vector<int>{1, 2, 3, 4, 5, 6}));
        }

        {
            testSuite.test_case("empty");
            RAH2_STD::vector<int> out_{};
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto result = RAH2_NS::ranges::shift_left(out.begin(), out.begin(), 3000);
            CHECK(RAH2_NS::begin(result) == RAH2_NS::end(out));
            CHECK(RAH2_NS::end(result) == RAH2_NS::end(out));
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
            COMPARE_DURATION_TO_STD_ALGO_20_AND_RANGES_23(
                CS == CommonOrSent::Common,
                "shift_left_iter",
                range_type,
                (
                    [&]
                    {
                        STD::shift_left(
                            RAH2_NS::ranges::begin(fwd(out)), RAH2_NS::ranges::end(out), 12);
                        CHECK(out.front() == 0);
                    }));
        }

        {
            COMPARE_DURATION_TO_STD_RANGES_23(
                "shift_left_ranges",
                range_type,
                (
                    [&]
                    {
                        auto result2 = STD::shift_left(out, 12);
                        DONT_OPTIM(result2);
                    }));
        }
    }
    static constexpr bool do_test = RAH2_NS::derived_from<Tag, RAH2_NS::forward_iterator_tag>;
};
void test_shift_left()
{
    testSuite.test_case("sample");
    testSuite.test_case("return");
    /// [rah2::ranges::shift_left]
    RAH2_STD::vector<int> b{1, 2, 3, 4, 5, 6, 7};

    auto b8 = RAH2_NS::ranges::shift_left(b, 8); // has no effect: n >= last - first
    assert(RAH2_NS::ranges::equal(b, RAH2_STD::vector<int>{1, 2, 3, 4, 5, 6, 7}));
    assert(RAH2_NS::ranges::empty(b8));
    assert(b == (RAH2_STD::vector<int>{1, 2, 3, 4, 5, 6, 7}));

    auto b0 = RAH2_NS::ranges::shift_left(b, 0); // has no effect: n == 0
    assert(RAH2_NS::ranges::equal(b0, RAH2_STD::vector<int>{1, 2, 3, 4, 5, 6, 7}));
    assert(b == (RAH2_STD::vector<int>{1, 2, 3, 4, 5, 6, 7}));

    RAH2_STD::vector<int> ref{4, 5, 6, 7};
    auto b3 = RAH2_NS::ranges::shift_left(b, 3);
    assert(RAH2_NS::ranges::equal(b3, ref));
    assert(RAH2_NS::ranges::equal(b.begin(), b.begin() + 4, ref.begin(), ref.end()));
    /// [rah2::ranges::shift_left]

    foreach_range_combination<test_algo<test_shift_left_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_shift_right_
{
    template <bool = true>
    void test()
    {
        {
            testSuite.test_case("iter");
            RAH2_STD::vector<int> out_{1, 2, 3, 4, 5};
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto result = RAH2_NS::ranges::shift_right(out.begin(), out.end(), 3);
            CHECK(RAH2_NS::ranges::begin(result) == RAH2_NS::ranges::next(out.begin(), 3));
            CHECK(RAH2_NS::ranges::end(result) == out.end());
            CHECK(out_ == (RAH2_STD::vector<int>{1, 2, 3, 1, 2}));
        }
        {
            testSuite.test_case("range");
            RAH2_STD::vector<int> out_{1, 2, 3, 4, 5, 6};
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto result = RAH2_NS::ranges::shift_right(out, 2);
            CHECK(RAH2_NS::ranges::begin(result) == RAH2_NS::ranges::next(out.begin(), 2));
            CHECK(RAH2_NS::ranges::end(result) == out.end());
            CHECK(RAH2_NS::ranges::equal(result, RAH2_STD::vector<int>{1, 2, 3, 4}));
        }

        {
            RAH2_STD::vector<int> out_{1, 2, 3, 4, 5, 6};
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto result = RAH2_NS::ranges::shift_right(out, 9);
            CHECK(RAH2_NS::ranges::begin(result) == out.end());
            CHECK(RAH2_NS::ranges::end(result) == out.end());
            CHECK(out_ == (RAH2_STD::vector<int>{1, 2, 3, 4, 5, 6}));
        }

        {
            testSuite.test_case("empty");
            RAH2_STD::vector<int> out_{};
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto result = RAH2_NS::ranges::shift_right(out.begin(), out.begin(), 3000);
            CHECK(RAH2_NS::begin(result) == RAH2_NS::end(out));
            CHECK(RAH2_NS::end(result) == RAH2_NS::end(out));
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
            COMPARE_DURATION_TO_STD_ALGO_20_AND_RANGES_23(
                CS == CommonOrSent::Common,
                "shift_right_iter",
                range_type,
                [&]
                {
                    STD::shift_right(RAH2_NS::ranges::begin(fwd(out)), RAH2_NS::ranges::end(out), 12);
                    CHECK(out.front() == 0);
                });
        }

        {
            COMPARE_DURATION_TO_STD_RANGES_23(
                "shift_right_ranges",
                range_type,
                (
                    [&]
                    {
                        auto result2 = STD::shift_right(out, 12);
                        DONT_OPTIM(result2);
                    }));
        }
    }
    static constexpr bool do_test = RAH2_NS::derived_from<Tag, RAH2_NS::forward_iterator_tag>;
};
void test_shift_right()
{
    // TODO : Test perf with all iterator/range type. Take care of random_access+sized_range
    testSuite.test_case("sample");
    testSuite.test_case("return");
    /// [rah2::ranges::shift_right]
    RAH2_STD::vector<int> b{1, 2, 3, 4, 5, 6, 7};

    auto b8 = RAH2_NS::ranges::shift_right(b, 8); // has no effect: n >= last - first
    assert(RAH2_NS::ranges::equal(b, RAH2_STD::vector<int>{1, 2, 3, 4, 5, 6, 7}));
    assert(RAH2_NS::ranges::empty(b8));
    assert(b == (RAH2_STD::vector<int>{1, 2, 3, 4, 5, 6, 7}));

    auto b0 = RAH2_NS::ranges::shift_right(b, 0); // has no effect: n == 0
    assert(RAH2_NS::ranges::equal(b0, RAH2_STD::vector<int>{1, 2, 3, 4, 5, 6, 7}));
    assert(b == (RAH2_STD::vector<int>{1, 2, 3, 4, 5, 6, 7}));

    RAH2_STD::vector<int> ref{1, 2, 3, 4};
    auto b3 = RAH2_NS::ranges::shift_right(b, 3);
    assert(RAH2_NS::ranges::equal(b3, ref));
    assert(RAH2_NS::ranges::equal(b.begin() + 3, b.end(), ref.begin(), ref.end()));
    /// [rah2::ranges::shift_right]

    foreach_range_combination<test_algo<test_shift_right_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_sample_
{
    template <bool = true>
    void test()
    {
        std::random_device rd;
        std::mt19937 g(rd());
        RAH2_STD::vector<int> in_{1, 2, 3, 4, 2, 3, 1};
        auto in = make_test_view_adapter<CS, Tag, Sized>(in_);

        {
            testSuite.test_case("iter");
            RAH2_STD::vector<int> out{0, 0, 0, 0, 0, 0, 4, 5};
            auto result = RAH2_NS::ranges::sample(
                RAH2_NS::ranges::begin(in), RAH2_NS::ranges::end(in), out.begin(), 3, g);
            CHECK(result == out.begin() + 3);
            CHECK(
                RAH2_NS::ranges::none_of(out.begin(), out.begin() + 3, [](auto v) { return v == 0; }));
        }

        {
            testSuite.test_case("range");
            RAH2_STD::vector<int> out{0, 0, 0, 0, 0, 0, 4, 5};
            auto result = RAH2_NS::ranges::sample(in, out.begin(), 3, g);
            CHECK(result == out.begin() + 3);
            CHECK(
                RAH2_NS::ranges::none_of(out.begin(), out.begin() + 3, [](auto v) { return v == 0; }));
        }

        testSuite.test_case("empty");
        {
            RAH2_STD::vector<int> empty_in_;
            auto empty_in = make_test_view_adapter<CS, Tag, Sized>(empty_in_);
            RAH2_STD::vector<int> empty_out;

            auto result = RAH2_NS::ranges::sample(empty_in, empty_out.begin(), 0, g);
            CHECK(result == empty_out.begin());
            CHECK(empty_out.empty());
        }
    }

    template <bool = true>
    void test_perf(char const* range_type)
    {
        testSuite.test_case("perf");

        std::random_device rd;
        std::mt19937 g(rd());

#ifndef RAH2_USE_EASTL
        // eastl do not have sample algo, so there is nothing to compare
        {
            RAH2_STD::vector<int> in_;
            for (size_t i = 0; i < 1000010 * RELEASE_MULTIPLIER; ++i)
            {
                in_.push_back(i % 15);
            }
            auto in = make_test_view_adapter<CS, Tag, Sized>(in_);
            RAH2_STD::vector<int> out;
            out.resize(1000000 * RELEASE_MULTIPLIER);
            out.emplace_back();
            out.emplace_back();

            COMPARE_DURATION_TO_STD_ALGO_17_AND_RANGES(
                CS == Common,
                "sample_iter",
                range_type,
                [&]
                {
                    auto result = STD::sample(
                        fwd(RAH2_NS::ranges::begin(in)),
                        RAH2_NS::ranges::end(in),
                        out.begin(),
                        1000000 * RELEASE_MULTIPLIER,
                        g);
                    DONT_OPTIM(result);
                });
        }
#endif

        {
            RAH2_STD::vector<Coord> in2_;
            for (intptr_t i = 0; i < 1000010 * RELEASE_MULTIPLIER; ++i)
            {
                in2_.emplace_back(Coord{i % 15, 0});
            }
            auto in2 = make_test_view_adapter<CS, Tag, Sized>(in2_);

            RAH2_STD::vector<Coord> out2;
            out2.resize(1000000 * RELEASE_MULTIPLIER);
            out2.emplace_back();
            out2.emplace_back();

            COMPARE_DURATION_TO_STD_RANGES(
                "sample_range",
                range_type,
                (
                    [&]
                    {
                        auto result2 =
                            STD::sample(in2, out2.begin(), 1000000 * RELEASE_MULTIPLIER, g);
                        DONT_OPTIM(result2);
                    }));
        }
    }
    static constexpr bool do_test = true;
};
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

    foreach_range_combination<test_algo<test_sample_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_unique_
{
    template <bool = true>
    void test()
    {
        {
            testSuite.test_case("iter");
            RAH2_STD::vector<int> in_{2, 3, 1, 1, 1, 5, 3, 3, 4};
            auto in = make_test_view_adapter<CS, Tag, Sized>(in_);
            auto result =
                RAH2_NS::ranges::unique(RAH2_NS::ranges::begin(in), RAH2_NS::ranges::end(in));
            CHECK(result.begin() == RAH2_NS::ranges::next(in.begin(), 6));
            CHECK(result.end() == in.end());
        }

        {
            RAH2_STD::vector<Coord> in_{
                {2, 0}, {3, 0}, {1, 0}, {11, 0}, {1, 0}, {5, 0}, {13, 0}, {3, 0}, {4, 0}};
            auto in = make_test_view_adapter<CS, Tag, Sized>(in_);
            testSuite.test_case("range");
            auto result = RAH2_NS::ranges::unique(
                in, [](auto a, auto b) { return a % 10 == b % 10; }, &Coord::x);
            CHECK(result.begin() == RAH2_NS::ranges::next(in.begin(), 6));
            CHECK(result.end() == in.end());
        }

        {
            testSuite.test_case("empty");
            RAH2_STD::vector<int> empty_in_;
            auto empty_in = make_test_view_adapter<CS, Tag, Sized>(empty_in_);
            auto result = RAH2_NS::ranges::unique(empty_in);
            CHECK(result.begin() == empty_in.begin());
            CHECK(result.end() == empty_in.end());
        }
    }

    template <bool = true>
    void test_perf(char const* range_type)
    {
        testSuite.test_case("perf");
        RAH2_STD::vector<int> in_;
        for (size_t i = 0; i < 100000 * RELEASE_MULTIPLIER; ++i)
        {
            in_.push_back(0);
            in_.push_back(0);
            in_.push_back(1);
            in_.push_back(2);
            in_.push_back(2);
            in_.push_back(2);
            in_.push_back(3);
        }
        auto in = make_test_view_adapter<CS, Tag, Sized>(in_);

        {
            COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(
                CS == Common,
                "unique_iter",
                range_type,
                [&]
                {
                    auto result =
                        STD::unique(fwd(RAH2_NS::ranges::begin(in)), RAH2_NS::ranges::end(in));
                    DONT_OPTIM(result);
                });
        }

        RAH2_STD::vector<Coord> in2_;
        for (intptr_t i = 0; i < 100000 * RELEASE_MULTIPLIER; ++i)
        {
            in2_.push_back({0, 0});
            in2_.push_back({11, 0});
            in2_.push_back({1, 0});
            in2_.push_back({12, 0});
            in2_.push_back({2, 0});
            in2_.push_back({2, 0});
            in2_.push_back({3, 0});
        }
        auto in2 = make_test_view_adapter<CS, Tag, Sized>(in2_);

        {
            COMPARE_DURATION_TO_STD_RANGES(
                "unique_proj_range",
                range_type,
                (
                    [&]
                    {
                        auto result2 = STD::unique(
                            in2, [](auto a, auto b) { return a % 10 == b % 10; }, &Coord::x);
                        DONT_OPTIM(result2);
                    }));
        }
    }
    static constexpr bool do_test = RAH2_NS::derived_from<Tag, RAH2_NS::forward_iterator_tag>;
};
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

    foreach_range_combination<test_algo<test_unique_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_unique_copy_
{
    template <bool = true>
    void test()
    {
        {
            testSuite.test_case("iter");
            RAH2_STD::vector<int> in_{2, 3, 1, 1, 1, 5, 3, 3, 4};
            RAH2_STD::vector<int> out(in_.size());
            auto in = make_test_view_adapter<CS, Tag, Sized>(in_);
            auto result = RAH2_NS::ranges::unique_copy(
                RAH2_NS::ranges::begin(in), RAH2_NS::ranges::end(in), out.begin());
            CHECK(result.in == in.end());
            CHECK(result.out == out.begin() + 6);
            CHECK((out == RAH2_STD::vector<int>{2, 3, 1, 5, 3, 4, 0, 0, 0}));
        }

        {
            RAH2_STD::vector<Coord> in_{
                {2, 0}, {3, 0}, {1, 0}, {11, 0}, {1, 0}, {5, 0}, {13, 0}, {3, 0}, {4, 0}};
            RAH2_STD::vector<Coord> out(in_.size());
            auto in = make_test_view_adapter<CS, Tag, Sized>(in_);
            testSuite.test_case("range");
            auto result = RAH2_NS::ranges::unique_copy(
                in, out.begin(), [](auto a, auto b) { return a % 10 == b % 10; }, &Coord::x);
            CHECK(result.in == in.end());
            CHECK(result.out == out.begin() + 6);
            CHECK(
                (out
                 == RAH2_STD::vector<Coord>{
                     {2, 0}, {3, 0}, {1, 0}, {5, 0}, {13, 0}, {4, 0}, {0, 0}, {0, 0}, {0, 0}}));
        }

        {
            testSuite.test_case("empty");
            RAH2_STD::vector<int> empty_in_;
            RAH2_STD::vector<int> out(2);
            auto empty_in = make_test_view_adapter<CS, Tag, Sized>(empty_in_);
            auto result = RAH2_NS::ranges::unique_copy(empty_in, out.begin());
            CHECK(result.in == empty_in.end());
            CHECK(result.out == out.begin());
            CHECK((out == RAH2_STD::vector<int>{0, 0}));
        }
    }

    template <bool = true>
    void test_perf(char const* range_type)
    {
        testSuite.test_case("perf");
#ifndef RAH2_USE_EASTL
        {
            RAH2_STD::vector<int> in_;
            for (size_t i = 0; i < 100000 * RELEASE_MULTIPLIER; ++i)
            {
                in_.push_back(0);
                in_.push_back(0);
                in_.push_back(1);
                in_.push_back(2);
                in_.push_back(2);
                in_.push_back(2);
                in_.push_back(3);
            }
            RAH2_STD::vector<int> out(in_.size() + 2);
            auto in = make_test_view_adapter<CS, Tag, Sized>(in_);

            COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(
                CS == Common,
                "unique_copy_iter",
                range_type,
                [&]
                {
                    auto result = STD::unique_copy(
                        fwd(RAH2_NS::ranges::begin(in)), RAH2_NS::ranges::end(in), out.begin());
                    DONT_OPTIM(result);
                });
        }
#endif

        {
            RAH2_STD::vector<Coord> in2_;
            for (intptr_t i = 0; i < 100000 * RELEASE_MULTIPLIER; ++i)
            {
                in2_.push_back({0, 0});
                in2_.push_back({11, 0});
                in2_.push_back({1, 0});
                in2_.push_back({12, 0});
                in2_.push_back({2, 0});
                in2_.push_back({2, 0});
                in2_.push_back({3, 0});
            }
            auto in2 = make_test_view_adapter<CS, Tag, Sized>(in2_);
            RAH2_STD::vector<Coord> out2(in2_.size() + 2);

            COMPARE_DURATION_TO_STD_RANGES(
                "unique_copy_proj_range",
                range_type,
                (
                    [&]
                    {
                        auto result2 = STD::unique_copy(
                            in2,
                            out2.begin(),
                            [](auto a, auto b) { return a % 10 == b % 10; },
                            &Coord::x);
                        DONT_OPTIM(result2);
                    }));
        }
    }
    static constexpr bool do_test = RAH2_NS::derived_from<Tag, RAH2_NS::forward_iterator_tag>;
};
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

    foreach_range_combination<test_algo<test_unique_copy_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_is_partitioned_
{
    static bool is_even(int n)
    {
        return n % 2 == 0;
    }

    static bool is_even_64(intptr_t n)
    {
        return n % 2 == 0;
    }

    static bool is_even_coord(Coord n)
    {
        return n.x % 2 == 0;
    }

    template <bool = true>
    void test()
    {
        testSuite.test_case("iter");
        {
            RAH2_STD::vector<int> all_even_ = {2, 4, 6, 8};
            auto all_even = make_test_view_adapter<CS, Tag, Sized>(all_even_);
            CHECK(RAH2_NS::ranges::is_partitioned(all_even.begin(), all_even.end(), is_even));
        }

        {
            RAH2_STD::vector<int> all_odd_ = {1, 3, 5, 7};
            auto all_odd = make_test_view_adapter<CS, Tag, Sized>(all_odd_);
            CHECK(RAH2_NS::ranges::is_partitioned(all_odd.begin(), all_odd.end(), is_even));
        }

        {
            RAH2_STD::vector<int> mixed_ = {2, 3, 4, 5};
            auto mixed = make_test_view_adapter<CS, Tag, Sized>(mixed_);
            CHECK(!RAH2_NS::ranges::is_partitioned(mixed.begin(), mixed.end(), is_even));
        }

        {
            RAH2_STD::vector<int> partitioned_ = {2, 4, 6, 1, 3, 5};
            auto partitioned = make_test_view_adapter<CS, Tag, Sized>(partitioned_);
            CHECK(RAH2_NS::ranges::is_partitioned(partitioned.begin(), partitioned.end(), is_even));
        }

        testSuite.test_case("range");
        {
            RAH2_STD::vector<Coord> all_even_ = {{2, 0}, {4, 0}, {6, 0}, {8, 0}};
            auto all_even = make_test_view_adapter<CS, Tag, Sized>(all_even_);
            CHECK(RAH2_NS::ranges::is_partitioned(all_even, is_even_64, &Coord::x));
        }

        {
            RAH2_STD::vector<Coord> all_odd_ = {{1, 0}, {3, 0}, {5, 0}, {7, 0}};
            auto all_odd = make_test_view_adapter<CS, Tag, Sized>(all_odd_);
            CHECK(RAH2_NS::ranges::is_partitioned(all_odd, is_even_64, &Coord::x));
        }

        {
            RAH2_STD::vector<Coord> mixed_ = {{2, 0}, {3, 0}, {4, 0}, {5, 0}};
            auto mixed = make_test_view_adapter<CS, Tag, Sized>(mixed_);
            CHECK(!RAH2_NS::ranges::is_partitioned(mixed, is_even_64, &Coord::x));
        }

        {
            RAH2_STD::vector<Coord> partitioned_ = {{2, 0}, {4, 0}, {6, 0}, {1, 0}, {3, 0}, {5, 0}};
            auto partitioned = make_test_view_adapter<CS, Tag, Sized>(partitioned_);
            CHECK(RAH2_NS::ranges::is_partitioned(partitioned, is_even_64, &Coord::x));
        }

        {
            RAH2_STD::vector<Coord> empty_;
            auto empty = make_test_view_adapter<CS, Tag, Sized>(empty_);
            CHECK(RAH2_NS::ranges::is_partitioned(empty, is_even_64, &Coord::x));
        }
    }
    template <bool = true>
    void test_perf(char const* range_type)
    {
        RAH2_STD::vector<Coord> perf_(1000000 * RELEASE_MULTIPLIER);
        perf_.insert(perf_.end(), 1000000 * RELEASE_MULTIPLIER, {1, 0});
        auto perf = make_test_view_adapter<CS, Tag, Sized>(perf_);

        COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(
            CS == Common,
            "is_partitioned",
            range_type,
            [&]
            {
                auto result = STD::is_partitioned(fwd(perf.begin()), perf.end(), is_even_coord);
                CHECK(result);
            });
        COMPARE_DURATION_TO_STD_RANGES(
            "is_partitioned_proj",
            range_type,
            [&]
            {
                auto result = STD::is_partitioned(perf, is_even_64, &Coord::x);
                CHECK(result);
            });
    }
    static constexpr bool do_test = true;
};
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

    foreach_range_combination<test_algo<test_is_partitioned_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_partition_
{
    static bool is_even(int n)
    {
        return n % 2 == 0;
    }

    static bool is_even_64(intptr_t n)
    {
        return n % 2 == 0;
    }

    template <bool = true>
    void test()
    {
        testSuite.test_case("iter");
        {
            // empty
            RAH2_STD::vector<int> out_;
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto result = RAH2_NS::ranges::partition(out.begin(), out.end(), is_even);
            CHECK(result.begin() == out.begin());
            CHECK(result.end() == out.begin());
            CHECK(out_ == (RAH2_STD::vector<int>{}));
        }
        {
            // single
            RAH2_STD::vector<int> out_{2};
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto result = RAH2_NS::ranges::partition(out.begin(), out.end(), is_even);
            CHECK(result.begin() == RAH2_NS::ranges::next(out.begin(), 1));
            CHECK(result.end() == RAH2_NS::ranges::next(out.begin(), 1));
            CHECK(out_ == (RAH2_STD::vector<int>{2}));
        }
        {
            // all true
            RAH2_STD::vector<int> out_{2, 4, 6, 8};
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto result = RAH2_NS::ranges::partition(out.begin(), out.end(), is_even);
            CHECK(result.begin() == RAH2_NS::ranges::next(out.begin(), 4));
            CHECK(result.end() == RAH2_NS::ranges::next(out.begin(), 4));
            CHECK(out_ == (RAH2_STD::vector<int>{2, 4, 6, 8}));
        }
        {
            // all false
            RAH2_STD::vector<int> out_{1, 3, 5, 7};
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto result = RAH2_NS::ranges::partition(out.begin(), out.end(), is_even);
            CHECK(result.begin() == out.begin());
            CHECK(result.end() == RAH2_NS::ranges::next(out.begin(), 4));
            CHECK(out_ == (RAH2_STD::vector<int>{1, 3, 5, 7}));
        }
        {
            // mixed
            RAH2_STD::vector<int> out_{2, 3, 4, 5};
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto result = RAH2_NS::ranges::partition(out.begin(), out.end(), is_even);
            CHECK(result.begin() == RAH2_NS::ranges::next(out.begin(), 2));
            CHECK(result.end() == RAH2_NS::ranges::next(out.begin(), 4));
            CHECK(RAH2_NS::ranges::is_partitioned(out_, is_even));
        }

        testSuite.test_case("range");
        {
            // empty
            RAH2_STD::vector<Coord> out_;
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto result = RAH2_NS::ranges::partition(out, is_even_64, &Coord::x);
            CHECK(result.begin() == out.begin());
            CHECK(result.end() == out.begin());
            CHECK(out_ == (RAH2_STD::vector<Coord>{}));
        }
        {
            // single
            RAH2_STD::vector<Coord> out_{Coord{2, 0}};
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto result = RAH2_NS::ranges::partition(out, is_even_64, &Coord::x);
            CHECK(result.begin() == RAH2_NS::ranges::next(out.begin(), 1));
            CHECK(result.end() == RAH2_NS::ranges::next(out.begin(), 1));
            CHECK(out_ == (RAH2_STD::vector<Coord>{Coord{2, 0}}));
        }
        {
            // all true
            RAH2_STD::vector<Coord> out_{Coord{2, 0}, Coord{4, 0}, Coord{6, 0}, Coord{8, 0}};
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto result = RAH2_NS::ranges::partition(out, is_even_64, &Coord::x);
            CHECK(result.begin() == RAH2_NS::ranges::next(out.begin(), 4));
            CHECK(result.end() == RAH2_NS::ranges::next(out.begin(), 4));
            CHECK(out_ == (RAH2_STD::vector<Coord>{{2, 0}, {4, 0}, {6, 0}, {8, 0}}));
        }
        {
            // all false
            RAH2_STD::vector<Coord> out_{Coord{1, 0}, Coord{3, 0}, Coord{5, 0}, Coord{7, 0}};
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto result = RAH2_NS::ranges::partition(out, is_even_64, &Coord::x);
            CHECK(result.begin() == out.begin());
            CHECK(result.end() == RAH2_NS::ranges::next(out.begin(), 4));
            CHECK(out_ == (RAH2_STD::vector<Coord>{{1, 0}, {3, 0}, {5, 0}, {7, 0}}));
        }
        {
            // mixed
            RAH2_STD::vector<Coord> out_{{2, 0}, {3, 0}, {4, 0}, {5, 0}};
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto result = RAH2_NS::ranges::partition(out, is_even_64, &Coord::x);
            CHECK(result.begin() == RAH2_NS::ranges::next(out.begin(), 2));
            CHECK(result.end() == RAH2_NS::ranges::next(out.begin(), 4));
            CHECK(RAH2_NS::ranges::is_partitioned(out_, is_even_64, &Coord::x));
        }
    }

    template <bool = true>
    void test_perf(char const* range_type)
    {
        testSuite.test_case("perf");
        {
            COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(
                CS == CommonOrSent::Common,
                "partition_iter",
                range_type,
                (
                    [&]
                    {
                        RAH2_STD::vector<int> out_;
                        out_.reserve(2000000 * RELEASE_MULTIPLIER);
                        for (size_t i = 0; i < 1000000 * RELEASE_MULTIPLIER; ++i)
                        {
                            out_.emplace_back(0);
                            out_.emplace_back(1);
                        }
                        auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
                        STD::partition(
                            RAH2_NS::ranges::begin(fwd(out)), RAH2_NS::ranges::end(out), is_even);
                        CHECK(out.front() == 0);
                    }));
        }

        {
            COMPARE_DURATION_TO_STD_RANGES(
                "partition_ranges",
                range_type,
                ((
                    [&]
                    {
                        RAH2_STD::vector<Coord> out_;
                        out_.reserve(2000000 * RELEASE_MULTIPLIER);
                        for (size_t i = 0; i < 1000000 * RELEASE_MULTIPLIER; ++i)
                        {
                            out_.emplace_back(Coord{0, 0});
                            out_.emplace_back(Coord{1, 0});
                        }
                        auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
                        STD::partition(out, is_even_64, &Coord::x);
                        CHECK(out.front() == Coord());
                    })));
        }
    }
    static constexpr bool do_test = RAH2_NS::derived_from<Tag, RAH2_NS::forward_iterator_tag>;
};
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

    foreach_range_combination<test_algo<test_partition_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_partition_copy_
{
    static bool is_even(int n)
    {
        return n % 2 == 0;
    }

    static bool is_even_64(intptr_t n)
    {
        return n % 2 == 0;
    }

    template <bool = true>
    void test()
    {
        testSuite.test_case("iter");
        {
            // empty
            RAH2_STD::vector<int> in_;
            RAH2_STD::vector<int> out(in_.size()), out2(in_.size());
            auto in = make_test_view_adapter<CS, Tag, Sized>(in_);
            auto result = RAH2_NS::ranges::partition_copy(
                in.begin(), in.end(), out.begin(), out2.begin(), is_even);
            CHECK(result.in == in.end());
            CHECK(result.out1 == out.begin());
            CHECK(result.out2 == out2.begin());
        }
        {
            // single
            RAH2_STD::vector<int> in_{2};
            RAH2_STD::vector<int> out(in_.size()), out2(in_.size());
            auto in = make_test_view_adapter<CS, Tag, Sized>(in_);
            auto result = RAH2_NS::ranges::partition_copy(
                in.begin(), in.end(), out.begin(), out2.begin(), is_even);
            CHECK(result.in == in.end());
            CHECK(result.out1 == out.begin() + 1);
            CHECK(result.out2 == out2.begin());
            CHECK(*out.begin() == 2);
        }
        {
            // all true
            RAH2_STD::vector<int> in_{2, 4, 6, 8};
            RAH2_STD::vector<int> out(in_.size()), out2(in_.size());
            auto in = make_test_view_adapter<CS, Tag, Sized>(in_);
            auto result = RAH2_NS::ranges::partition_copy(
                in.begin(), in.end(), out.begin(), out2.begin(), is_even);
            CHECK(result.in == in.end());
            CHECK(result.out1 == out.begin() + 4);
            CHECK(result.out2 == out2.begin());
            CHECK(RAH2_NS::ranges::views::take(out, 4) == (RAH2_STD::vector<int>{2, 4, 6, 8}));
        }
        {
            // all false
            RAH2_STD::vector<int> in_{1, 3, 5, 7};
            RAH2_STD::vector<int> out(in_.size()), out2(in_.size());
            auto in = make_test_view_adapter<CS, Tag, Sized>(in_);
            auto result = RAH2_NS::ranges::partition_copy(
                in.begin(), in.end(), out.begin(), out2.begin(), is_even);
            CHECK(result.in == in.end());
            CHECK(result.out1 == out.begin());
            CHECK(result.out2 == out2.begin() + 4);
            CHECK(RAH2_NS::ranges::views::take(out2, 4) == (RAH2_STD::vector<int>{1, 3, 5, 7}));
        }
        {
            // mixed
            RAH2_STD::vector<int> in_{2, 3, 4, 5};
            RAH2_STD::vector<int> out(in_.size()), out2(in_.size());
            auto in = make_test_view_adapter<CS, Tag, Sized>(in_);
            auto result = RAH2_NS::ranges::partition_copy(
                in.begin(), in.end(), out.begin(), out2.begin(), is_even);
            CHECK(result.in == in.end());
            CHECK(result.out1 == out.begin() + 2);
            CHECK(result.out2 == out2.begin() + 2);
            CHECK(
                (RAH2_NS::ranges::views::take(out, 2) == RAH2_STD::vector<int>{2, 4}
                 || RAH2_NS::ranges::views::take(out, 2) == RAH2_STD::vector<int>{4, 2}));
            CHECK(
                (RAH2_NS::ranges::views::take(out2, 2) == RAH2_STD::vector<int>{3, 5}
                 || RAH2_NS::ranges::views::take(out2, 2) == RAH2_STD::vector<int>{5, 3}));
        }

        testSuite.test_case("range");
        {
            // empty
            RAH2_STD::vector<Coord> in_;
            RAH2_STD::vector<Coord> out(in_.size()), out2(in_.size());
            auto in = make_test_view_adapter<CS, Tag, Sized>(in_);
            auto result = RAH2_NS::ranges::partition_copy(
                in, out.begin(), out2.begin(), is_even_64, &Coord::x);
            // WhatIs<decltype(result)>();
            CHECK(result.in == in.end());
            CHECK(result.out1 == out.begin());
            CHECK(result.out2 == out2.begin());
        }
        {
            // single
            RAH2_STD::vector<Coord> in_{Coord{2, 0}};
            RAH2_STD::vector<Coord> out(in_.size()), out2(in_.size());
            auto in = make_test_view_adapter<CS, Tag, Sized>(in_);
            auto result = RAH2_NS::ranges::partition_copy(
                in, out.begin(), out2.begin(), is_even_64, &Coord::x);
            CHECK(result.in == in.end());
            CHECK(result.out1 == out.begin() + 1);
            CHECK(result.out2 == out2.begin());
            CHECK((*out.begin() == Coord{2, 0}));
        }
        {
            // all true
            RAH2_STD::vector<Coord> in_{{2, 0}, {4, 0}, {6, 0}, {8, 0}};
            RAH2_STD::vector<Coord> out(in_.size()), out2(in_.size());
            auto in = make_test_view_adapter<CS, Tag, Sized>(in_);
            auto result = RAH2_NS::ranges::partition_copy(
                in, out.begin(), out2.begin(), is_even_64, &Coord::x);
            CHECK(result.in == in.end());
            CHECK(result.out1 == out.begin() + 4);
            CHECK(result.out2 == out2.begin());
            CHECK(
                RAH2_NS::ranges::views::take(out, 4)
                == (RAH2_STD::vector<Coord>{{2, 0}, {4, 0}, {6, 0}, {8, 0}}));
        }
        {
            // all false
            RAH2_STD::vector<Coord> in_{{1, 0}, {3, 0}, {5, 0}, {7, 0}};
            RAH2_STD::vector<Coord> out(in_.size()), out2(in_.size());
            auto in = make_test_view_adapter<CS, Tag, Sized>(in_);
            auto result = RAH2_NS::ranges::partition_copy(
                in, out.begin(), out2.begin(), is_even_64, &Coord::x);
            CHECK(result.in == in.end());
            CHECK(result.out1 == out.begin());
            CHECK(result.out2 == out2.begin() + 4);
            CHECK(
                RAH2_NS::ranges::views::take(out2, 4)
                == (RAH2_STD::vector<Coord>{{1, 0}, {3, 0}, {5, 0}, {7, 0}}));
        }
        {
            // mixed
            RAH2_STD::vector<Coord> in_{{2, 0}, {3, 0}, {4, 0}, {5, 0}};
            RAH2_STD::vector<Coord> out(in_.size()), out2(in_.size());
            auto in = make_test_view_adapter<CS, Tag, Sized>(in_);
            auto result = RAH2_NS::ranges::partition_copy(
                in, out.begin(), out2.begin(), is_even_64, &Coord::x);
            CHECK(result.in == in.end());
            CHECK(result.out1 == out.begin() + 2);
            CHECK(result.out2 == out2.begin() + 2);
            CHECK(
                (RAH2_NS::ranges::views::take(out, 2) == RAH2_STD::vector<Coord>{{2, 0}, {4, 0}}
                 || RAH2_NS::ranges::views::take(out, 2) == RAH2_STD::vector<Coord>{{4, 0}, {2, 0}}));
            CHECK((
                RAH2_NS::ranges::views::take(out2, 2) == RAH2_STD::vector<Coord>{{3, 0}, {5, 0}}
                || RAH2_NS::ranges::views::take(out2, 2) == RAH2_STD::vector<Coord>{{5, 0}, {3, 0}}));
        }
    }

    template <bool = true>
    void test_perf(char const* range_type)
    {
        testSuite.test_case("perf");
#ifndef RAH2_USE_EASTL
        {
            COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(
                CS == CommonOrSent::Common,
                "partition_copy_iter",
                range_type,
                (
                    [&]
                    {
                        RAH2_STD::vector<int> in_;
                        in_.reserve(2000000 * RELEASE_MULTIPLIER);
                        for (size_t i = 0; i < 1000000 * RELEASE_MULTIPLIER; ++i)
                        {
                            in_.emplace_back(0);
                            in_.emplace_back(1);
                        }
                        RAH2_STD::vector<int> out(in_.size()), out2(in_.size());
                        auto in = make_test_view_adapter<CS, Tag, Sized>(in_);
                        auto result = STD::partition_copy(
                            RAH2_NS::ranges::begin(fwd(in)),
                            RAH2_NS::ranges::end(in),
                            out.begin(),
                            out2.begin(),
                            is_even);
                        DONT_OPTIM(result);
                    }));
        }
#endif

        {
            COMPARE_DURATION_TO_STD_RANGES(
                "partition_copy_ranges",
                range_type,
                ((
                    [&]
                    {
                        RAH2_STD::vector<Coord> in_;
                        in_.reserve(2000000 * RELEASE_MULTIPLIER);
                        for (size_t i = 0; i < 1000000 * RELEASE_MULTIPLIER; ++i)
                        {
                            in_.push_back(Coord{0, 0});
                            in_.push_back(Coord{1, 0});
                        }
                        RAH2_STD::vector<Coord> out(in_.size()), out2(in_.size());
                        auto in = make_test_view_adapter<CS, Tag, Sized>(in_);
                        auto result = STD::partition_copy(
                            in, out.begin(), out2.begin(), is_even_64, &Coord::x);
                        DONT_OPTIM(result);
                    })));
        }
    }
    static constexpr bool do_test = true;
};
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

    foreach_range_combination<test_algo<test_partition_copy_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_stable_partition_
{
    static bool is_even(int n)
    {
        return n % 2 == 0;
    }

    static bool is_even_64(intptr_t n)
    {
        return n % 2 == 0;
    }

    template <bool = true>
    void test()
    {
        testSuite.test_case("iter");
        {
            // empty
            RAH2_STD::vector<int> out_;
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto result = RAH2_NS::ranges::stable_partition(out.begin(), out.end(), is_even);
            CHECK(result.begin() == out.begin());
            CHECK(result.end() == out.begin());
            CHECK(out_ == (RAH2_STD::vector<int>{}));
        }
        {
            // single
            RAH2_STD::vector<int> out_{2};
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto result = RAH2_NS::ranges::stable_partition(out.begin(), out.end(), is_even);
            CHECK(result.begin() == RAH2_NS::ranges::next(out.begin(), 1));
            CHECK(result.end() == RAH2_NS::ranges::next(out.begin(), 1));
            CHECK(out_ == (RAH2_STD::vector<int>{2}));
        }
        {
            // all true
            RAH2_STD::vector<int> out_{2, 4, 6, 8};
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto result = RAH2_NS::ranges::stable_partition(out.begin(), out.end(), is_even);
            CHECK(result.begin() == RAH2_NS::ranges::next(out.begin(), 4));
            CHECK(result.end() == RAH2_NS::ranges::next(out.begin(), 4));
            CHECK(out_ == (RAH2_STD::vector<int>{2, 4, 6, 8}));
        }
        {
            // all false
            RAH2_STD::vector<int> out_{1, 3, 5, 7};
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto result = RAH2_NS::ranges::stable_partition(out.begin(), out.end(), is_even);
            CHECK(result.begin() == out.begin());
            CHECK(result.end() == RAH2_NS::ranges::next(out.begin(), 4));
            CHECK(out_ == (RAH2_STD::vector<int>{1, 3, 5, 7}));
        }
        {
            // mixed
            RAH2_STD::vector<int> out_{2, 3, 4, 5};
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto result = RAH2_NS::ranges::stable_partition(out.begin(), out.end(), is_even);
            CHECK(result.begin() == RAH2_NS::ranges::next(out.begin(), 2));
            CHECK(result.end() == RAH2_NS::ranges::next(out.begin(), 4));
            CHECK(out_ == (RAH2_STD::vector<int>{2, 4, 3, 5}));
        }

        testSuite.test_case("range");
        {
            // empty
            RAH2_STD::vector<Coord> out_;
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto result = RAH2_NS::ranges::stable_partition(out, is_even_64, &Coord::x);
            CHECK(result.begin() == out.begin());
            CHECK(result.end() == out.begin());
            CHECK(out_ == (RAH2_STD::vector<Coord>{}));
        }
        {
            // single
            RAH2_STD::vector<Coord> out_{Coord{2, 0}};
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto result = RAH2_NS::ranges::stable_partition(out, is_even_64, &Coord::x);
            CHECK(result.begin() == RAH2_NS::ranges::next(out.begin(), 1));
            CHECK(result.end() == RAH2_NS::ranges::next(out.begin(), 1));
            CHECK(out_ == (RAH2_STD::vector<Coord>{Coord{2, 0}}));
        }
        {
            // all true
            RAH2_STD::vector<Coord> out_{Coord{2, 0}, Coord{4, 0}, Coord{6, 0}, Coord{8, 0}};
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto result = RAH2_NS::ranges::stable_partition(out, is_even_64, &Coord::x);
            CHECK(result.begin() == RAH2_NS::ranges::next(out.begin(), 4));
            CHECK(result.end() == RAH2_NS::ranges::next(out.begin(), 4));
            CHECK(out_ == (RAH2_STD::vector<Coord>{{2, 0}, {4, 0}, {6, 0}, {8, 0}}));
        }
        {
            // all false
            RAH2_STD::vector<Coord> out_{Coord{1, 0}, Coord{3, 0}, Coord{5, 0}, Coord{7, 0}};
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto result = RAH2_NS::ranges::stable_partition(out, is_even_64, &Coord::x);
            CHECK(result.begin() == out.begin());
            CHECK(result.end() == RAH2_NS::ranges::next(out.begin(), 4));
            CHECK(out_ == (RAH2_STD::vector<Coord>{{1, 0}, {3, 0}, {5, 0}, {7, 0}}));
        }
        {
            // mixed
            RAH2_STD::vector<Coord> out_{{2, 0}, {3, 0}, {4, 0}, {5, 0}};
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto result = RAH2_NS::ranges::stable_partition(out, is_even_64, &Coord::x);
            CHECK(result.begin() == RAH2_NS::ranges::next(out.begin(), 2));
            CHECK(result.end() == RAH2_NS::ranges::next(out.begin(), 4));
            CHECK(out_ == (RAH2_STD::vector<Coord>{{2, 0}, {4, 0}, {3, 0}, {5, 0}}));
        }
    }

    template <bool = true>
    void test_perf(char const* range_type)
    {
        testSuite.test_case("perf");
#if defined(__clang__)
        RAH2_EXT_WARNING_PUSH
#pragma GCC diagnostic ignored "-Wdeprecated-declarations"
#endif
        {
            COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(
                CS == CommonOrSent::Common,
                "stable_partition_iter",
                range_type,
                (
                    [&]
                    {
                        RAH2_STD::vector<int> out_;
                        out_.reserve(2000000 * RELEASE_MULTIPLIER);
                        for (size_t i = 0; i < 1000000 * RELEASE_MULTIPLIER; ++i)
                        {
                            out_.emplace_back(0);
                            out_.emplace_back(1);
                        }
                        auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
                        STD::stable_partition(
                            RAH2_NS::ranges::begin(fwd(out)), RAH2_NS::ranges::end(out), is_even);
                        CHECK(out.front() == 0);
                    }));
        }
#if defined(__clang__)
        RAH2_EXT_WARNING_POP
#endif

        {
            COMPARE_DURATION_TO_STD_RANGES(
                "stable_partition_ranges",
                range_type,
                ((
                    [&]
                    {
                        RAH2_STD::vector<Coord> out_;
                        out_.reserve(2000000 * RELEASE_MULTIPLIER);
                        for (size_t i = 0; i < 1000000 * RELEASE_MULTIPLIER; ++i)
                        {
                            out_.emplace_back(Coord{0, 0});
                            out_.emplace_back(Coord{1, 0});
                        }
                        auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
                        STD::stable_partition(out, is_even_64, &Coord::x);
                        CHECK(out.front() == Coord());
                    })));
        }
    }
    static constexpr bool do_test = RAH2_NS::derived_from<Tag, RAH2_NS::bidirectional_iterator_tag>;
};
void test_stable_partition()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::stable_partition]
    RAH2_STD::vector<int> in{1, 2, 3, 4, 5};
    auto const boundary = RAH2_NS::ranges::stable_partition(in, [](auto a) { return a >= 4; });
    assert(boundary.begin() == in.begin() + 2);
    assert(in == RAH2_STD::vector<int>({4, 5, 1, 2, 3}));
    /// [rah2::ranges::stable_partition]

    foreach_range_combination<test_algo<test_stable_partition_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_partition_point_
{
    template <bool = true>
    void test()
    {
        {
            RAH2_STD::vector<Coord> in{{1, 0}, {2, 0}, {3, 0}, {4, 0}};
            auto r1 = make_test_view_adapter<CS, Tag, Sized>(in);
            testSuite.test_case("range");
            testSuite.test_case("noproj");
            auto const iter = RAH2_NS::ranges::partition_point(r1, [](auto c) { return c.x < 3; });
            testSuite.test_case("iter");
            testSuite.test_case("proj");
            assert((*iter == Coord{3, 0}));
            auto const iter2 = RAH2_NS::ranges::partition_point(
                r1.begin(), r1.end(), [](auto c) { return c < 3; }, &Coord::x);
            assert((*iter2 == Coord{3, 0}));
        }
    }
    template <bool = true>
    void test_perf(char const* range_type)
    {
        size_t PerfMultiplier =
            (RAH2_NS::derived_from<Tag, RAH2_NS::random_access_iterator_tag> && Sized) ? 100llu :
                                                                                         10llu;

        testSuite.test_case("perf");
        RAH2_STD::vector<Coord> in(10000000llu * RELEASE_MULTIPLIER, Coord{1, 2});
        in.push_back(Coord{3, 4});
        auto r1 = make_test_view_adapter<CS, Tag, Sized>(in);

        COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(
            CS == Common,
            "partition_point",
            range_type,
            [&]
            {
                for (size_t i = 0; i < PerfMultiplier; ++i)
                {
                    auto iter = STD::partition_point(
                        fwd(r1.begin()), r1.end(), [](auto c) { return c.x < 3; });
                    assert((*iter == Coord{3, 4}));
                }
            });
        COMPARE_DURATION_TO_STD_RANGES(
            "partition_point_proj",
            range_type,
            [&]
            {
                for (size_t i = 0; i < PerfMultiplier; ++i)
                {
                    auto iter = STD::partition_point(r1, [](int64_t c) { return c < 3; }, &Coord::x);
                    assert((*iter == Coord{3, 4}));
                }
            });
    }
    static constexpr bool do_test = RAH2_NS::derived_from<Tag, RAH2_NS::forward_iterator_tag>;
};
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

    foreach_range_combination<test_algo<test_partition_point_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_is_sorted_
{
    static bool descending(int a, int b)
    {
        return b < a;
    }

    static bool descending_64(intptr_t a, intptr_t b)
    {
        return b < a;
    }

    static bool descending_coord(Coord a, Coord b)
    {
        return b.x < a.x;
    }

    template <bool = true>
    void test()
    {
        testSuite.test_case("iter");
        {
            RAH2_STD::vector<int> sorted_ = {8, 6, 4, 2};
            auto sorted = make_test_view_adapter<CS, Tag, Sized>(sorted_);
            CHECK(RAH2_NS::ranges::is_sorted(sorted.begin(), sorted.end(), descending));
        }

        {
            RAH2_STD::vector<int> not_sorted_ = {2, 3, 4, 5};
            auto not_sorted = make_test_view_adapter<CS, Tag, Sized>(not_sorted_);
            CHECK(!RAH2_NS::ranges::is_sorted(not_sorted.begin(), not_sorted.end(), descending));
        }

        {
            RAH2_STD::vector<int> empty_{};
            auto empty = make_test_view_adapter<CS, Tag, Sized>(empty_);
            CHECK(RAH2_NS::ranges::is_sorted(empty.begin(), empty.end(), descending));
        }

        testSuite.test_case("range");
        {
            RAH2_STD::vector<Coord> sorted_ = {{8, 0}, {6, 0}, {4, 0}, {2, 0}};
            auto sorted = make_test_view_adapter<CS, Tag, Sized>(sorted_);
            CHECK(RAH2_NS::ranges::is_sorted(sorted, descending_64, &Coord::x));
        }

        {
            RAH2_STD::vector<Coord> not_sorted_ = {{1, 0}, {3, 0}, {5, 0}, {7, 0}};
            auto not_sorted = make_test_view_adapter<CS, Tag, Sized>(not_sorted_);
            CHECK(!RAH2_NS::ranges::is_sorted(not_sorted, descending_64, &Coord::x));
        }

        {
            RAH2_STD::vector<Coord> empty_{};
            auto empty = make_test_view_adapter<CS, Tag, Sized>(empty_);
            CHECK(RAH2_NS::ranges::is_sorted(empty, descending_64, &Coord::x));
        }
    }
    template <bool = true>
    void test_perf(char const* range_type)
    {
        RAH2_STD::vector<int> perf_iter_(1000000 * RELEASE_MULTIPLIER);
        perf_iter_.insert(perf_iter_.end(), 1000000 * RELEASE_MULTIPLIER, 1);
        auto perf_iter = make_test_view_adapter<CS, Tag, Sized>(perf_iter_);

        RAH2_STD::vector<Coord> perf_(1000000 * RELEASE_MULTIPLIER, {1, 0});
        perf_.insert(perf_.end(), 1000000 * RELEASE_MULTIPLIER, {0, 0});
        auto perf = make_test_view_adapter<CS, Tag, Sized>(perf_);

        COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(
            CS == Common,
            "is_sorted",
            range_type,
            [&]
            {
                auto result = STD::is_sorted(fwd(perf_iter.begin()), perf_iter.end());
                CHECK(result);
            });
        COMPARE_DURATION_TO_STD_RANGES(
            "is_sorted_proj",
            range_type,
            [&]
            {
                auto result = STD::is_sorted(perf, descending_64, &Coord::x);
                CHECK(result);
            });
    }
    static constexpr bool do_test = RAH2_NS::derived_from<Tag, RAH2_NS::forward_iterator_tag>;
};
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

    foreach_range_combination<test_algo<test_is_sorted_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_is_sorted_until_
{
    static bool descending(int a, int b)
    {
        return b < a;
    }

    static bool descending_64(intptr_t a, intptr_t b)
    {
        return b < a;
    }

    static bool descending_coord(Coord a, Coord b)
    {
        return b.x < a.x;
    }

    template <bool = true>
    void test()
    {
        testSuite.test_case("iter");
        {
            RAH2_STD::vector<int> sorted_ = {2, 4, 6, 8};
            auto not_sorted = make_test_view_adapter<CS, Tag, Sized>(sorted_);
            auto point = RAH2_NS::ranges::is_sorted_until(not_sorted.begin(), not_sorted.end());
            CHECK(point == not_sorted.end());
        }

        {
            RAH2_STD::vector<int> not_sorted_ = {2, 4, 6, 5};
            auto not_sorted = make_test_view_adapter<CS, Tag, Sized>(not_sorted_);
            auto point = RAH2_NS::ranges::is_sorted_until(not_sorted.begin(), not_sorted.end());
            CHECK(RAH2_NS::ranges::distance(not_sorted.begin(), point) == 3);
        }

        {
            RAH2_STD::vector<int> empty_{};
            auto empty = make_test_view_adapter<CS, Tag, Sized>(empty_);
            auto point = RAH2_NS::ranges::is_sorted_until(empty.begin(), empty.end());
            CHECK(empty.end() == point);
        }

        testSuite.test_case("range");
        {
            RAH2_STD::vector<Coord> sorted_ = {{8, 0}, {6, 0}, {4, 0}, {2, 0}};
            auto sorted = make_test_view_adapter<CS, Tag, Sized>(sorted_);
            auto point = RAH2_NS::ranges::is_sorted_until(sorted, descending_64, &Coord::x);
            CHECK(sorted.end() == point);
        }

        {
            RAH2_STD::vector<Coord> not_sorted_ = {{8, 0}, {7, 0}, {5, 0}, {7, 0}};
            auto not_sorted = make_test_view_adapter<CS, Tag, Sized>(not_sorted_);
            auto point = RAH2_NS::ranges::is_sorted_until(not_sorted, descending_64, &Coord::x);
            CHECK(RAH2_NS::ranges::distance(not_sorted.begin(), point) == 3);
        }

        {
            RAH2_STD::vector<Coord> empty_{};
            auto empty = make_test_view_adapter<CS, Tag, Sized>(empty_);
            auto point = RAH2_NS::ranges::is_sorted_until(empty, descending_64, &Coord::x);
            CHECK(empty.end() == point);
        }
    }
    template <bool = true>
    void test_perf(char const* range_type)
    {
        RAH2_STD::vector<int> perf_iter_(1000000 * RELEASE_MULTIPLIER);
        perf_iter_.insert(perf_iter_.end(), 1000000 * RELEASE_MULTIPLIER, 1);
        auto perf_iter = make_test_view_adapter<CS, Tag, Sized>(perf_iter_);

        RAH2_STD::vector<Coord> perf_(1000000 * RELEASE_MULTIPLIER, {1, 0});
        perf_.insert(perf_.end(), 1000000 * RELEASE_MULTIPLIER, {0, 0});
        auto perf = make_test_view_adapter<CS, Tag, Sized>(perf_);

        COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(
            CS == Common,
            "is_sorted_until",
            range_type,
            [&]
            {
                auto result = STD::is_sorted_until(fwd(perf_iter.begin()), perf_iter.end());
                DONT_OPTIM(result);
            });
        COMPARE_DURATION_TO_STD_RANGES(
            "is_sorted_until_proj",
            range_type,
            [&]
            {
                auto result = STD::is_sorted_until(perf, descending_64, &Coord::x);
                DONT_OPTIM(result);
            });
    }
    static constexpr bool do_test = RAH2_NS::derived_from<Tag, RAH2_NS::forward_iterator_tag>;
};
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

    foreach_range_combination<test_algo<test_is_sorted_until_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_sort_
{
    struct Comp64
    {
        inline constexpr bool operator()(intptr_t a, intptr_t b) const
        {
            return b < a;
        }
    };
    static inline constexpr bool comp_64(intptr_t a, intptr_t b)
    {
        return b < a;
    }

    template <bool = true>
    void test()
    {
        testSuite.test_case("iter");
        {
            // empty
            RAH2_STD::vector<int> out_;
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto result = RAH2_NS::ranges::sort(out.begin(), out.end());
            CHECK(result == out.end());
            CHECK(out_ == (RAH2_STD::vector<int>{}));
        }
        {
            // single
            RAH2_STD::vector<int> out_{2};
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto result = RAH2_NS::ranges::sort(out.begin(), out.end());
            CHECK(result == out.end());
            CHECK(out_ == (RAH2_STD::vector<int>{2}));
        }
        {
            // sorted
            RAH2_STD::vector<int> out_{2, 4, 6, 8};
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto result = RAH2_NS::ranges::sort(out.begin(), out.end());
            CHECK(result == out.end());
            CHECK(out_ == (RAH2_STD::vector<int>{2, 4, 6, 8}));
        }
        {
            // unsorted
            RAH2_STD::vector<int> out_{7, 3, 5, 1};
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto result = RAH2_NS::ranges::sort(out.begin(), out.end());
            CHECK(result == out.end());
            CHECK(out_ == (RAH2_STD::vector<int>{1, 3, 5, 7}));
        }
        {
            // longer than kQuickSortLimit
            RAH2_STD::vector<int> out_;
            for (size_t i = 0; i < 50; ++i)
            {
                out_.push_back(int(49) - int(i));
            }
            out_[10] = 10;
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto result = RAH2_NS::ranges::sort(out.begin(), out.end());
            CHECK(result == out.end());
            CHECK(RAH2_NS::ranges::is_sorted(out_));
        }

        testSuite.test_case("range");
        {
            // empty
            RAH2_STD::vector<Coord> out_;
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto result = RAH2_NS::ranges::sort(out, comp_64, &Coord::x);
            CHECK(result == out.end());
            CHECK(out_ == (RAH2_STD::vector<Coord>{}));
        }
        {
            // single
            RAH2_STD::vector<Coord> out_{Coord{2, 0}};
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto result = RAH2_NS::ranges::sort(out, comp_64, &Coord::x);
            CHECK(result == out.end());
            CHECK(out_ == (RAH2_STD::vector<Coord>{Coord{2, 0}}));
        }
        {
            // sorted
            RAH2_STD::vector<Coord> out_{Coord{8, 0}, Coord{6, 0}, Coord{4, 0}, Coord{2, 0}};
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto result = RAH2_NS::ranges::sort(out, comp_64, &Coord::x);
            CHECK(result == out.end());
            CHECK(out_ == (RAH2_STD::vector<Coord>{{8, 0}, {6, 0}, {4, 0}, {2, 0}}));
        }
        {
            // unsorted
            RAH2_STD::vector<Coord> out_{Coord{1, 0}, Coord{3, 0}, Coord{5, 0}, Coord{7, 0}};
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto result = RAH2_NS::ranges::sort(out, comp_64, &Coord::x);
            CHECK(result == out.end());
            CHECK(out_ == (RAH2_STD::vector<Coord>{{7, 0}, {5, 0}, {3, 0}, {1, 0}}));
        }
        {
            // longer than kQuickSortLimit
            RAH2_STD::vector<Coord> out_;
            for (size_t i = 0; i < 50; ++i)
            {
                out_.push_back(Coord{int(49) - int(i), 0});
            }
            out_[10] = Coord{10, 0};
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto result = RAH2_NS::ranges::sort(out, comp_64, &Coord::x);
            CHECK(result == out.end());
            CHECK(RAH2_NS::ranges::is_sorted(out_, comp_64, &Coord::x));
        }
    }

    template <bool = true>
    void test_perf(char const* range_type)
    {
        testSuite.test_case("perf");
        {
            COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(
                CS == CommonOrSent::Common,
                "sort_iter",
                range_type,
                (
                    [&]
                    {
                        RAH2_STD::vector<int> out_;
                        out_.reserve(200000 * RELEASE_MULTIPLIER);
                        for (size_t i = 0; i < 100000 * RELEASE_MULTIPLIER; ++i)
                        {
                            out_.emplace_back(0);
                            // out_.emplace_back(Coord{1, 0}); // TODO : Test perf with this version
                            out_.emplace_back(0);
                        }
                        out_[2] = 1;
                        auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
                        STD::sort(RAH2_NS::ranges::begin(fwd(out)), RAH2_NS::ranges::end(out));
                        CHECK(out.front() == 0);
                    }));
        }

        {
            COMPARE_DURATION_TO_STD_RANGES(
                "sort_ranges",
                range_type,
                ((
                    [&]
                    {
                        RAH2_STD::vector<Coord> out_;
                        out_.reserve(20000 * RELEASE_MULTIPLIER);
                        for (size_t i = 0; i < 10000 * RELEASE_MULTIPLIER; ++i)
                        {
                            out_.emplace_back(Coord{0, 0});
                            // out_.emplace_back(Coord{1, 0}); // TODO : Test perf with this version
                            out_.emplace_back(Coord{0, 0});
                        }
                        out_[2] = {1, 0};
                        auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
                        STD::sort(out, comp_64, &Coord::x);
                        CHECK((out.front() == Coord{1, 0}));
                    })));
        }
    }
    static constexpr bool do_test = RAH2_NS::derived_from<Tag, RAH2_NS::random_access_iterator_tag>;
};
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

    foreach_range_combination<test_algo<test_sort_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_partial_sort_
{
    static bool comp_64(intptr_t a, intptr_t b)
    {
        return b < a;
    }

    template <bool = true>
    void test()
    {
        testSuite.test_case("iter");
        {
            // empty
            RAH2_STD::vector<int> out_;
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto result = RAH2_NS::ranges::partial_sort(out.begin(), out.begin(), out.end());
            CHECK(result == out.end());
            CHECK(out_ == (RAH2_STD::vector<int>{}));
        }
        {
            // single
            RAH2_STD::vector<int> out_{2};
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto result = RAH2_NS::ranges::partial_sort(out.begin(), out.begin(), out.end());
            CHECK(result == out.end());
            CHECK(out_ == (RAH2_STD::vector<int>{2}));
            auto result2 = RAH2_NS::ranges::partial_sort(out.begin(), ++out.begin(), out.end());
            CHECK(result2 == out.end());
            CHECK(out_ == (RAH2_STD::vector<int>{2}));
        }
        {
            // sorted
            RAH2_STD::vector<int> out_{2, 4, 6, 8};
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto midle = RAH2_NS::ranges::next(out.begin(), 2);
            auto result = RAH2_NS::ranges::partial_sort(out.begin(), midle, out.end());
            CHECK(result == out.end());
            CHECK(out_ == (RAH2_STD::vector<int>{2, 4, 6, 8}));
        }
        {
            // unsorted
            RAH2_STD::vector<int> out_{7, 3, 5, 1};
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto midle = RAH2_NS::ranges::next(out.begin(), 2);
            auto result = RAH2_NS::ranges::partial_sort(out.begin(), midle, out.end());
            CHECK(result == out.end());
            CHECK(
                (out_ == (RAH2_STD::vector<int>{1, 3, 5, 7}))
                || (out_ == (RAH2_STD::vector<int>{1, 3, 7, 5})));
        }

        testSuite.test_case("range");
        {
            // empty
            RAH2_STD::vector<Coord> out_;
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto result = RAH2_NS::ranges::partial_sort(out, out.begin(), comp_64, &Coord::x);
            CHECK(result == out.end());
            CHECK(out_ == (RAH2_STD::vector<Coord>{}));
        }
        {
            // single
            RAH2_STD::vector<Coord> out_{Coord{2, 0}};
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto result = RAH2_NS::ranges::partial_sort(out, out.begin(), comp_64, &Coord::x);
            CHECK(result == out.end());
            CHECK(out_ == (RAH2_STD::vector<Coord>{Coord{2, 0}}));
            auto result2 = RAH2_NS::ranges::partial_sort(out, ++out.begin(), comp_64, &Coord::x);
            CHECK(result2 == out.end());
            CHECK(out_ == (RAH2_STD::vector<Coord>{Coord{2, 0}}));
        }
        {
            // sorted
            RAH2_STD::vector<Coord> out_{Coord{8, 0}, Coord{6, 0}, Coord{4, 0}, Coord{2, 0}};
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto midle = RAH2_NS::ranges::next(out.begin(), 2);
            auto result = RAH2_NS::ranges::partial_sort(out, midle, comp_64, &Coord::x);
            CHECK(result == out.end());
            CHECK(out_ == (RAH2_STD::vector<Coord>{{8, 0}, {6, 0}, {4, 0}, {2, 0}}));
        }
        {
            // unsorted
            RAH2_STD::vector<Coord> out_{Coord{1, 0}, Coord{3, 0}, Coord{5, 0}, Coord{7, 0}};
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto midle = RAH2_NS::ranges::next(out.begin(), 2);
            auto result = RAH2_NS::ranges::partial_sort(out, midle, comp_64, &Coord::x);
            CHECK(result == out.end());
            CHECK(
                (out_ == (RAH2_STD::vector<Coord>{{7, 0}, {5, 0}, {1, 0}, {3, 0}}))
                || (out_ == (RAH2_STD::vector<Coord>{{7, 0}, {5, 0}, {3, 0}, {1, 0}})));
        }
    }

    template <bool = true>
    void test_perf(char const* range_type)
    {
        testSuite.test_case("perf");
        {
            COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(
                CS == CommonOrSent::Common,
                "partial_sort_iter",
                range_type,
                (
                    [&]
                    {
                        RAH2_STD::vector<int> out_;
                        out_.reserve(200000 * RELEASE_MULTIPLIER);
                        for (size_t i = 0; i < 100000 * RELEASE_MULTIPLIER; ++i)
                        {
                            out_.emplace_back(0);
                            out_.emplace_back(1);
                        }
                        auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
                        auto midle =
                            RAH2_NS::ranges::next(fwd(out).begin(), 100000 * RELEASE_MULTIPLIER);
                        STD::partial_sort(
                            RAH2_NS::ranges::begin(out), fwd(midle), RAH2_NS::ranges::end(out));
                        CHECK(out.front() == 0);
                    }));
        }

        {
            COMPARE_DURATION_TO_STD_RANGES(
                "partial_sort_ranges",
                range_type,
                ((
                    [&]
                    {
                        RAH2_STD::vector<Coord> out_;
                        out_.reserve(20000 * RELEASE_MULTIPLIER);
                        for (size_t i = 0; i < 10000 * RELEASE_MULTIPLIER; ++i)
                        {
                            out_.emplace_back(Coord{0, 0});
                            out_.emplace_back(Coord{1, 0});
                        }
                        auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
                        auto midle = RAH2_NS::ranges::next(out.begin(), 10000 * RELEASE_MULTIPLIER);
                        STD::partial_sort(out, midle, comp_64, &Coord::x);
                        CHECK((out.front() == Coord{1, 0}));
                    })));
        }
    }
    static constexpr bool do_test = RAH2_NS::derived_from<Tag, RAH2_NS::random_access_iterator_tag>;
};
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

    foreach_range_combination<test_algo<test_partial_sort_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_partial_sort_copy_
{
    static bool comp_64(intptr_t a, intptr_t b)
    {
        return b < a;
    }

    template <bool = true>
    void test()
    {
        testSuite.test_case("iter");
        {
            // empty
            RAH2_STD::vector<int> in_;
            auto in =
                make_test_view_adapter<CommonOrSent::Sentinel, RAH2_STD::input_iterator_tag, false>(
                    in_);
            RAH2_STD::vector<int> out_;
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto result =
                RAH2_NS::ranges::partial_sort_copy(in.begin(), in.end(), out.begin(), out.end());
            CHECK(result.in == in.end());
            CHECK(result.out == out.end());
        }
        {
            // single to empty
            RAH2_STD::vector<int> in_{2};
            auto in =
                make_test_view_adapter<CommonOrSent::Sentinel, RAH2_STD::forward_iterator_tag, false>(
                    in_);
            RAH2_STD::vector<int> out_;
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto result =
                RAH2_NS::ranges::partial_sort_copy(in.begin(), in.end(), out.begin(), out.end());
            CHECK(result.in == in.end());
            CHECK(result.out == out.end());
        }
        {
            // single to single
            RAH2_STD::vector<int> in_{2};
            auto in =
                make_test_view_adapter<CommonOrSent::Sentinel, RAH2_STD::forward_iterator_tag, false>(
                    in_);
            RAH2_STD::vector<int> out_(1);
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto result =
                RAH2_NS::ranges::partial_sort_copy(in.begin(), in.end(), out.begin(), out.end());
            CHECK(result.in == in.end());
            CHECK(result.out == out.end());
            CHECK(out_ == (RAH2_STD::vector<int>{2}));
        }
        {
            // sorted
            RAH2_STD::vector<int> in_{2, 4, 6, 8};
            auto in =
                make_test_view_adapter<CommonOrSent::Sentinel, RAH2_STD::forward_iterator_tag, false>(
                    in_);
            RAH2_STD::vector<int> out_(2);
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto result =
                RAH2_NS::ranges::partial_sort_copy(in.begin(), in.end(), out.begin(), out.end());
            CHECK(result.in == in.end());
            CHECK(result.out == out.end());
            CHECK(out_ == (RAH2_STD::vector<int>{2, 4}));
        }
        {
            // unsorted
            RAH2_STD::vector<int> in_{7, 3, 5, 1};
            auto in =
                make_test_view_adapter<CommonOrSent::Sentinel, RAH2_STD::forward_iterator_tag, false>(
                    in_);
            RAH2_STD::vector<int> out_(2);
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto result =
                RAH2_NS::ranges::partial_sort_copy(in.begin(), in.end(), out.begin(), out.end());
            CHECK(result.in == in.end());
            CHECK(result.out == out.end());
            CHECK(out_ == (RAH2_STD::vector<int>{1, 3}));
        }

        testSuite.test_case("range");
        {
            // empty
            RAH2_STD::vector<Coord> in_;
            auto in =
                make_test_view_adapter<CommonOrSent::Sentinel, RAH2_STD::forward_iterator_tag, false>(
                    in_);
            RAH2_STD::vector<Complex> out_;
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto result =
                RAH2_NS::ranges::partial_sort_copy(in, out, comp_64, &Coord::x, &Complex::x);
            CHECK(result.in == in.end());
            CHECK(result.out == out.end());
            CHECK(out_ == (RAH2_STD::vector<Complex>{}));
        }
        {
            // single to empty
            RAH2_STD::vector<Coord> in_{Coord{2, 0}};
            auto in =
                make_test_view_adapter<CommonOrSent::Sentinel, RAH2_STD::forward_iterator_tag, false>(
                    in_);
            RAH2_STD::vector<Complex> out_;
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto result =
                RAH2_NS::ranges::partial_sort_copy(in, out, comp_64, &Coord::x, &Complex::x);
            CHECK(result.in == in.end());
            CHECK(result.out == out.end());
            CHECK(out_ == (RAH2_STD::vector<Complex>{}));
        }

        {
            // single to single
            RAH2_STD::vector<Coord> in_{Coord{2, 0}};
            auto in =
                make_test_view_adapter<CommonOrSent::Sentinel, RAH2_STD::forward_iterator_tag, false>(
                    in_);
            RAH2_STD::vector<Complex> out_(1);
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto result =
                RAH2_NS::ranges::partial_sort_copy(in, out, comp_64, &Coord::x, &Complex::x);
            CHECK(result.in == in.end());
            CHECK(result.out == out.end());
            CHECK(out_ == (RAH2_STD::vector<Complex>{Complex{2, 0}}));
        }
        {
            // sorted
            RAH2_STD::vector<Coord> in_{Coord{8, 0}, Coord{6, 0}, Coord{4, 0}, Coord{2, 0}};
            auto in =
                make_test_view_adapter<CommonOrSent::Sentinel, RAH2_STD::forward_iterator_tag, false>(
                    in_);
            RAH2_STD::vector<Complex> out_(2);
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto result =
                RAH2_NS::ranges::partial_sort_copy(in, out, comp_64, &Coord::x, &Complex::x);
            CHECK(result.in == in.end());
            CHECK(result.out == out.end());
            CHECK(out_ == (RAH2_STD::vector<Complex>{{8, 0}, {6, 0}}));
        }
        {
            // unsorted
            RAH2_STD::vector<Coord> in_{Coord{1, 0}, Coord{3, 0}, Coord{5, 0}, Coord{7, 0}};
            auto in =
                make_test_view_adapter<CommonOrSent::Sentinel, RAH2_STD::forward_iterator_tag, false>(
                    in_);
            RAH2_STD::vector<Complex> out_(2);
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto result =
                RAH2_NS::ranges::partial_sort_copy(in, out, comp_64, &Coord::x, &Complex::x);
            CHECK(result.in == in.end());
            CHECK(result.out == out.end());
            CHECK((out_ == (RAH2_STD::vector<Complex>{{7, 0}, {5, 0}})));
        }
    }

    template <bool = true>
    void test_perf(char const* range_type)
    {
        testSuite.test_case("perf");
        {
#ifndef RAH2_USE_EASTL
            COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(
                CS == CommonOrSent::Common,
                "partial_sort_copy_iter",
                range_type,
                (
                    [&]
                    {
                        RAH2_STD::vector<int> in_;
                        in_.reserve(200000 * RELEASE_MULTIPLIER);
                        for (size_t i = 0; i < 100000 * RELEASE_MULTIPLIER; ++i)
                        {
                            in_.emplace_back(0);
                            in_.emplace_back(1);
                        }
                        auto in =
                            make_test_view_adapter<CommonOrSent::Common, RAH2_STD::forward_iterator_tag, false>(
                                in_);
                        RAH2_STD::vector<int> out_(100000 * RELEASE_MULTIPLIER);
                        auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
                        auto result = STD::partial_sort_copy(
                            RAH2_NS::ranges::begin(fwd(in)),
                            RAH2_NS::ranges::end(in),
                            RAH2_NS::ranges::begin(out),
                            RAH2_NS::ranges::end(out));
                        DONT_OPTIM(result);
                    }));
#endif
        }

        {
            COMPARE_DURATION_TO_STD_RANGES(
                "partial_sort_copy_ranges",
                range_type,
                ((
                    [&]
                    {
                        RAH2_STD::vector<Coord> in_;
                        in_.reserve(20000 * RELEASE_MULTIPLIER);
                        for (size_t i = 0; i < 10000 * RELEASE_MULTIPLIER; ++i)
                        {
                            in_.emplace_back(Coord{0, 0});
                            in_.emplace_back(Coord{1, 0});
                        }
                        auto in =
                            make_test_view_adapter<CommonOrSent::Sentinel, RAH2_STD::forward_iterator_tag, false>(
                                in_);
                        RAH2_STD::vector<Complex> out_(20000 * RELEASE_MULTIPLIER);
                        auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
                        auto result =
                            STD::partial_sort_copy(in, out, comp_64, &Coord::x, &Complex::x);
                        DONT_OPTIM(result);
                    })));
        }
    }
    static constexpr bool do_test = RAH2_NS::derived_from<Tag, RAH2_NS::random_access_iterator_tag>;
};
void test_partial_sort_copy()
{
    testSuite.test_case("sample");
    testSuite.test_case("return");
    /// [rah2::ranges::partial_sort_copy]
#ifdef RAH2_USE_EASTL
    RAH2_STD::list<int> const source{4, 2, 5, 1, 3};
#else
    std::forward_list<int> const source{4, 2, 5, 1, 3};
#endif

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

    foreach_range_combination<test_algo<test_partial_sort_copy_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_stable_sort_
{
    static bool comp_64(intptr_t a, intptr_t b)
    {
        return b < a;
    }

    template <bool = true>
    void test()
    {
        testSuite.test_case("iter");
        {
            // empty
            RAH2_STD::vector<int> out_;
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto result = RAH2_NS::ranges::stable_sort(out.begin(), out.end());
            CHECK(result == out.end());
            CHECK(out_ == (RAH2_STD::vector<int>{}));
        }
        {
            // single
            RAH2_STD::vector<int> out_{2};
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto result = RAH2_NS::ranges::stable_sort(out.begin(), out.end());
            CHECK(result == out.end());
            CHECK(out_ == (RAH2_STD::vector<int>{2}));
        }
        {
            // sorted
            RAH2_STD::vector<int> out_{2, 4, 6, 8};
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto result = RAH2_NS::ranges::stable_sort(out.begin(), out.end());
            CHECK(result == out.end());
            CHECK(out_ == (RAH2_STD::vector<int>{2, 4, 6, 8}));
        }
        {
            // unsorted
            RAH2_STD::vector<int> out_{7, 3, 5, 1};
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto result = RAH2_NS::ranges::stable_sort(out.begin(), out.end());
            CHECK(result == out.end());
            CHECK(out_ == (RAH2_STD::vector<int>{1, 3, 5, 7}));
        }
        {
            // longer than kQuickSortLimit
            RAH2_STD::vector<int> out_;
            for (size_t i = 0; i < 260; ++i)
            {
                out_.push_back(int(259) - int(i));
            }
            out_[10] = 10;
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto result = RAH2_NS::ranges::stable_sort(out.begin(), out.end());
            CHECK(result == out.end());
            CHECK(RAH2_NS::ranges::is_sorted(out_));
        }

        testSuite.test_case("range");
        {
            // empty
            RAH2_STD::vector<Coord> out_;
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto result = RAH2_NS::ranges::stable_sort(out, comp_64, &Coord::x);
            CHECK(result == out.end());
            CHECK(out_ == (RAH2_STD::vector<Coord>{}));
        }
        {
            // single
            RAH2_STD::vector<Coord> out_{Coord{2, 0}};
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto result = RAH2_NS::ranges::stable_sort(out, comp_64, &Coord::x);
            CHECK(result == out.end());
            CHECK(out_ == (RAH2_STD::vector<Coord>{Coord{2, 0}}));
        }
        {
            // sorted
            RAH2_STD::vector<Coord> out_{
                Coord{8, 0}, Coord{6, 0}, Coord{4, 1}, Coord{4, 2}, Coord{2, 0}};
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto result = RAH2_NS::ranges::stable_sort(out, comp_64, &Coord::x);
            CHECK(result == out.end());
            CHECK(out_ == (RAH2_STD::vector<Coord>{{8, 0}, {6, 0}, {4, 1}, {4, 2}, {2, 0}}));
        }
        {
            // unsorted
            RAH2_STD::vector<Coord> out_{
                Coord{1, 0}, Coord{3, 1}, Coord{3, 2}, Coord{5, 0}, Coord{7, 0}};
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto result = RAH2_NS::ranges::stable_sort(out, comp_64, &Coord::x);
            CHECK(result == out.end());
            CHECK(out_ == (RAH2_STD::vector<Coord>{{7, 0}, {5, 0}, {3, 1}, {3, 2}, {1, 0}}));
        }
    }

    template <bool = true>
    void test_perf(char const* range_type)
    {
        testSuite.test_case("perf");
        {
            COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(
                CS == CommonOrSent::Common,
                "stable_sort_iter",
                range_type,
                (
                    [&]
                    {
                        RAH2_STD::vector<int> out_;
                        out_.reserve(200000 * RELEASE_MULTIPLIER);
                        for (size_t i = 0; i < 100000 * RELEASE_MULTIPLIER; ++i)
                        {
                            out_.emplace_back(0);
                            out_.emplace_back(1);
                        }
                        auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
                        STD::stable_sort(RAH2_NS::ranges::begin(fwd(out)), RAH2_NS::ranges::end(out));
                        CHECK(out.front() == 0);
                    }));
        }

        {
            COMPARE_DURATION_TO_STD_RANGES(
                "stable_sort_ranges",
                range_type,
                ((
                    [&]
                    {
                        RAH2_STD::vector<Coord> out_;
                        out_.reserve(20000 * RELEASE_MULTIPLIER);
                        for (size_t i = 0; i < 10000 * RELEASE_MULTIPLIER; ++i)
                        {
                            out_.emplace_back(Coord{0, 0});
                            out_.emplace_back(Coord{1, 0});
                        }
                        auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
                        STD::stable_sort(out, comp_64, &Coord::x);
                        CHECK((out.front() == Coord{1, 0}));
                    })));
        }
    }
    static constexpr bool do_test = RAH2_NS::derived_from<Tag, RAH2_NS::random_access_iterator_tag>;
};
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

    foreach_range_combination<test_algo<test_stable_sort_>>();
}

struct NotCopyable
{
    int i = 9;
    NotCopyable() = default;
    NotCopyable(int u)
        : i(u)
    {
    }
    NotCopyable(NotCopyable const&) = delete;
    NotCopyable(NotCopyable&&) = default;
    NotCopyable& operator=(NotCopyable const&) = delete;
    NotCopyable& operator=(NotCopyable&&) = default;
    ~NotCopyable() = default;

    friend bool operator<(NotCopyable const& a, NotCopyable const& b)
    {
        return a.i < b.i;
    }
    friend bool operator>(NotCopyable const& a, NotCopyable const& b)
    {
        return a.i > b.i;
    }
};

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_nth_element_
{
    static bool comp_64(intptr_t a, intptr_t b)
    {
        return b < a;
    }

    template <bool = true>
    void test()
    {
        testSuite.test_case("iter");
        {
            // empty
            RAH2_STD::vector<int> out_;
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto result = RAH2_NS::ranges::nth_element(out.begin(), out.begin(), out.end());
            CHECK(result == out.end());
            CHECK(out_ == (RAH2_STD::vector<int>{}));
        }
        {
            // single
            RAH2_STD::vector<int> out_{2};
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto result = RAH2_NS::ranges::nth_element(out.begin(), out.begin(), out.end());
            CHECK(result == out.end());
            CHECK(out_ == (RAH2_STD::vector<int>{2}));
            auto result2 = RAH2_NS::ranges::nth_element(out.begin(), ++out.begin(), out.end());
            CHECK(result2 == out.end());
            CHECK(out_ == (RAH2_STD::vector<int>{2}));
        }
        {
            // sorted
            RAH2_STD::vector<int> out_{2, 4, 6, 8};
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto midle = RAH2_NS::ranges::next(out.begin(), 2);
            auto result = RAH2_NS::ranges::nth_element(out.begin(), midle, out.end());
            CHECK(result == out.end());
            CHECK(out_ == (RAH2_STD::vector<int>{2, 4, 6, 8}));
        }
        {
            // unsorted
            RAH2_STD::vector<int> out_{7, 3, 5, 1};
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto midle = RAH2_NS::ranges::next(out.begin(), 2);
            auto result = RAH2_NS::ranges::nth_element(out.begin(), midle, out.end());
            CHECK(result == out.end());
            CHECK(RAH2_NS::ranges::all_of(out.begin(), midle, [](auto v) { return v < 5; }));
            auto after_middle = midle;
            ++after_middle;
            CHECK(RAH2_NS::ranges::all_of(after_middle, out.end(), [](auto v) { return v > 5; }));
            CHECK(*midle == 5);
        }
        {
            // Not-copyable
            RAH2_STD::vector<NotCopyable> out_;
            out_.emplace_back(7);
            out_.emplace_back(3);
            out_.emplace_back(5);
            out_.emplace_back(1);
            out_.emplace_back(4);
            out_.emplace_back(18);
            out_.emplace_back(1);
            out_.emplace_back(6);
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto midle = RAH2_NS::ranges::next(out.begin(), 4);
            auto result = RAH2_NS::ranges::nth_element(out.begin(), midle, out.end());
            CHECK(result == out.end());
            CHECK(RAH2_NS::ranges::all_of(out.begin(), midle, [](auto const& v) { return v < 5; }));
            auto after_middle = midle;
            ++after_middle;
            CHECK(
                RAH2_NS::ranges::all_of(after_middle, out.end(), [](auto const& v) { return v > 5; }));
            CHECK_EQUAL(midle->i, 5);
        }

        testSuite.test_case("range");
        {
            // empty
            RAH2_STD::vector<Coord> out_;
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto result = RAH2_NS::ranges::nth_element(out, out.begin(), comp_64, &Coord::x);
            CHECK(result == out.end());
            CHECK(out_ == (RAH2_STD::vector<Coord>{}));
        }
        {
            // single
            RAH2_STD::vector<Coord> out_{Coord{2, 0}};
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto result = RAH2_NS::ranges::nth_element(out, out.begin(), comp_64, &Coord::x);
            CHECK(result == out.end());
            CHECK(out_ == (RAH2_STD::vector<Coord>{Coord{2, 0}}));
            auto result2 = RAH2_NS::ranges::nth_element(out, ++out.begin(), comp_64, &Coord::x);
            CHECK(result2 == out.end());
            CHECK(out_ == (RAH2_STD::vector<Coord>{Coord{2, 0}}));
        }
        {
            // sorted
            RAH2_STD::vector<Coord> out_{Coord{8, 0}, Coord{6, 0}, Coord{4, 0}, Coord{2, 0}};
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto midle = RAH2_NS::ranges::next(out.begin(), 2);
            auto result = RAH2_NS::ranges::nth_element(out, midle, comp_64, &Coord::x);
            CHECK(result == out.end());
            CHECK(out_ == (RAH2_STD::vector<Coord>{{8, 0}, {6, 0}, {4, 0}, {2, 0}}));
        }
        {
            // unsorted
            RAH2_STD::vector<Coord> out_{Coord{1, 0}, Coord{3, 0}, Coord{5, 0}, Coord{7, 0}};
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            auto midle = RAH2_NS::ranges::next(out.begin(), 2);
            auto result = RAH2_NS::ranges::nth_element(out, midle, comp_64, &Coord::x);
            CHECK(result == out.end());
            CHECK(RAH2_NS::ranges::all_of(out.begin(), midle, [](auto v) { return v > 3; }, &Coord::x));
            auto after_middle = midle;
            ++after_middle;
            CHECK(
                RAH2_NS::ranges::all_of(
                    after_middle, out.end(), [](auto v) { return v < 3; }, &Coord::x));
            CHECK(midle->x == 3);
        }
    }

    template <bool = true>
    void test_perf(char const* range_type)
    {
        testSuite.test_case("perf");
        {
            COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(
                CS == CommonOrSent::Common,
                "nth_element_iter",
                range_type,
                (
                    [&]
                    {
                        RAH2_STD::vector<int> out_;
                        out_.reserve(200000 * RELEASE_MULTIPLIER);
                        for (size_t i = 0; i < 100000 * RELEASE_MULTIPLIER; ++i)
                        {
                            out_.emplace_back(0);
                            out_.emplace_back(1);
                        }
                        auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
                        auto midle =
                            RAH2_NS::ranges::next(fwd(out).begin(), 100000 * RELEASE_MULTIPLIER);
                        STD::nth_element(
                            RAH2_NS::ranges::begin(out), fwd(midle), RAH2_NS::ranges::end(out));
                        CHECK(out.front() == 0);
                    }));
        }

        {
            COMPARE_DURATION_TO_STD_RANGES(
                "nth_element_ranges",
                range_type,
                ((
                    [&]
                    {
                        RAH2_STD::vector<Coord> out_;
                        out_.reserve(20000 * RELEASE_MULTIPLIER);
                        for (size_t i = 0; i < 10000 * RELEASE_MULTIPLIER; ++i)
                        {
                            out_.emplace_back(Coord{0, 0});
                            out_.emplace_back(Coord{1, 0});
                        }
                        auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
                        auto midle = RAH2_NS::ranges::next(out.begin(), 10000 * RELEASE_MULTIPLIER);
                        STD::nth_element(out, midle, comp_64, &Coord::x);
                        CHECK((out.front() == Coord{1, 0}));
                    })));
        }
    }
    static constexpr bool do_test = RAH2_NS::derived_from<Tag, RAH2_NS::random_access_iterator_tag>;
};
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

    foreach_range_combination<test_algo<test_nth_element_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_lower_bound_
{
    static bool comp_64(intptr_t a, intptr_t b)
    {
        return b < a;
    }

    template <bool = true>
    void test()
    {
        testSuite.test_case("noproj");
        testSuite.test_case("iter");

        {
            testSuite.test_case("empty");
            RAH2_STD::vector<Coord> in;
            auto r1 = make_test_view_adapter<CS, Tag, Sized>(in);
            auto const iter = RAH2_NS::ranges::lower_bound(r1.begin(), r1.end(), Coord{3, 0});
            CHECK(iter == r1.end());
        }
        {
            testSuite.test_case("not_found");
            RAH2_STD::vector<Coord> in{{1, 0}, {2, 0}, {4, 0}};
            auto r1 = make_test_view_adapter<CS, Tag, Sized>(in);
            auto const iter = RAH2_NS::ranges::lower_bound(r1.begin(), r1.end(), Coord{3, 0});
            CHECK(iter == RAH2_NS::ranges::next(r1.begin(), 2)); // Point on {4, 0}
        }
        {
            testSuite.test_case("found");
            RAH2_STD::vector<Coord> in{{1, 0}, {2, 0}, {3, 0}, {3, 0}, {3, 0}, {4, 0}};
            auto r1 = make_test_view_adapter<CS, Tag, Sized>(in);
            auto const iter = RAH2_NS::ranges::lower_bound(r1.begin(), r1.end(), Coord{3, 0});
            CHECK(iter == RAH2_NS::ranges::next(r1.begin(), 2)); // Point on the first {3, 0}
        }

        testSuite.test_case("range");
        testSuite.test_case("proj");
        {
            testSuite.test_case("empty");
            RAH2_STD::vector<Coord> in;
            auto r1 = make_test_view_adapter<CS, Tag, Sized>(in);
            auto const iter = RAH2_NS::ranges::lower_bound(r1, 3, comp_64, &Coord::x);
            CHECK(iter == r1.end());
        }
        {
            testSuite.test_case("not_found");
            RAH2_STD::vector<Coord> in{{4, 0}, {2, 0}, {1, 0}};
            auto r1 = make_test_view_adapter<CS, Tag, Sized>(in);
            auto const iter = RAH2_NS::ranges::lower_bound(r1, 3, comp_64, &Coord::x);
            CHECK(iter == RAH2_NS::ranges::next(r1.begin(), 1)); // Point on {4, 0}
        }
        {
            testSuite.test_case("found");
            RAH2_STD::vector<Coord> in{{4, 0}, {3, 0}, {3, 0}, {3, 0}, {2, 0}, {1, 0}};
            auto r1 = make_test_view_adapter<CS, Tag, Sized>(in);
            auto const iter = RAH2_NS::ranges::lower_bound(r1, 3, comp_64, &Coord::x);
            CHECK(iter == RAH2_NS::ranges::next(r1.begin(), 1)); // Point on the first {3, 0}
        }
    }
    template <bool = true>
    void test_perf(char const* range_type)
    {
        RAH2_STD::vector<Coord> in(1000000 * RELEASE_MULTIPLIER, Coord{1, 2});
        in.push_back(Coord{3, 4});
        in.push_back(Coord{3, 4});
        in.push_back(Coord{3, 6});
        auto r1 = make_test_view_adapter<CS, Tag, Sized>(in);

        RAH2_STD::vector<Coord> in2(1000000 * RELEASE_MULTIPLIER, Coord{3, 4});
        in2.push_back(Coord{1, 4});
        in2.push_back(Coord{1, 2});
        in2.push_back(Coord{1, 3});
        auto r2 = make_test_view_adapter<CS, Tag, Sized>(in2);

        auto const RangeTypeMultiplier =
            RAH2_NS::derived_from<Tag, RAH2_NS::random_access_iterator_tag> ? 100 : 1;

        auto const RangeSizedMultiplier = Sized ? 10 : 1;

        COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(
            CS == Common,
            "lower_bound_iter",
            range_type,
            [&]
            {
                for (auto i = 0; i < RangeTypeMultiplier * RangeSizedMultiplier; ++i)
                {
                    auto iter = STD::lower_bound(fwd(r1.begin()), r1.end(), Coord{3, 4});
                    assert((*iter == Coord{3, 4}));
                }
            });
        COMPARE_DURATION_TO_STD_RANGES(
            "lower_bound_range_proj",
            range_type,
            [&]
            {
                for (auto i = 0; i < RangeTypeMultiplier * RangeSizedMultiplier; ++i)
                {
                    auto iter = STD::lower_bound(r2, 1, comp_64, &Coord::x);
                    assert((*iter == Coord{1, 4}));
                }
            });
    }
    static constexpr bool do_test = RAH2_NS::derived_from<Tag, RAH2_NS::forward_iterator_tag>;
};
void test_lower_bound()
{
    testSuite.test_case("sample");
    testSuite.test_case("return");
    /// [rah2::ranges::lower_bound]
    RAH2_STD::vector<int> data{1, 2, 2, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 5};
    auto const lower = RAH2_NS::ranges::lower_bound(data, 4);
    assert(lower == data.begin() + 6);
    /// [rah2::ranges::lower_bound]

    foreach_range_combination<test_algo<test_lower_bound_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_upper_bound_
{
    static bool comp_64(intptr_t a, intptr_t b)
    {
        return b < a;
    }

    template <bool = true>
    void test()
    {
        testSuite.test_case("noproj");
        testSuite.test_case("iter");

        {
            testSuite.test_case("empty");
            RAH2_STD::vector<Coord> in;
            auto r1 = make_test_view_adapter<CS, Tag, Sized>(in);
            auto const iter = RAH2_NS::ranges::upper_bound(r1.begin(), r1.end(), Coord{3, 0});
            CHECK(iter == r1.end());
        }
        {
            testSuite.test_case("not_found");
            RAH2_STD::vector<Coord> in{{1, 0}, {2, 0}, {4, 0}};
            auto r1 = make_test_view_adapter<CS, Tag, Sized>(in);
            auto const iter = RAH2_NS::ranges::upper_bound(r1.begin(), r1.end(), Coord{3, 0});
            CHECK(iter == RAH2_NS::ranges::next(r1.begin(), 2)); // Point on {4, 0}
        }
        {
            testSuite.test_case("found");
            RAH2_STD::vector<Coord> in{{1, 0}, {2, 0}, {3, 0}, {3, 0}, {3, 0}, {4, 0}};
            auto r1 = make_test_view_adapter<CS, Tag, Sized>(in);
            auto const iter = RAH2_NS::ranges::upper_bound(r1.begin(), r1.end(), Coord{3, 0});
            CHECK(iter == RAH2_NS::ranges::next(r1.begin(), 5)); // Point on the first {4, 0}
        }

        testSuite.test_case("range");
        testSuite.test_case("proj");
        {
            testSuite.test_case("empty");
            RAH2_STD::vector<Coord> in;
            auto r1 = make_test_view_adapter<CS, Tag, Sized>(in);
            auto const iter = RAH2_NS::ranges::upper_bound(r1, 3, comp_64, &Coord::x);
            CHECK(iter == r1.end());
        }
        {
            testSuite.test_case("not_found");
            RAH2_STD::vector<Coord> in{{4, 0}, {2, 0}, {1, 0}};
            auto r1 = make_test_view_adapter<CS, Tag, Sized>(in);
            auto const iter = RAH2_NS::ranges::upper_bound(r1, 3, comp_64, &Coord::x);
            CHECK(iter == RAH2_NS::ranges::next(r1.begin(), 1)); // Point on {4, 0}
        }
        {
            testSuite.test_case("found");
            RAH2_STD::vector<Coord> in{{4, 0}, {3, 0}, {3, 0}, {3, 0}, {2, 0}, {1, 0}};
            auto r1 = make_test_view_adapter<CS, Tag, Sized>(in);
            auto const iter = RAH2_NS::ranges::upper_bound(r1, 3, comp_64, &Coord::x);
            CHECK(iter == RAH2_NS::ranges::next(r1.begin(), 4)); // Point on {2, 0}
        }
    }
    template <bool = true>
    void test_perf(char const* range_type)
    {
        RAH2_STD::vector<Coord> in(1000000 * RELEASE_MULTIPLIER, Coord{1, 2});
        in.push_back(Coord{3, 4});
        in.push_back(Coord{3, 4});
        in.push_back(Coord{3, 6});
        auto r1 = make_test_view_adapter<CS, Tag, Sized>(in);

        RAH2_STD::vector<Coord> in2(1000000 * RELEASE_MULTIPLIER, Coord{3, 4});
        in2.push_back(Coord{1, 4});
        in2.push_back(Coord{1, 2});
        in2.push_back(Coord{1, 3});
        auto r2 = make_test_view_adapter<CS, Tag, Sized>(in2);

        auto const RangeTypeMultiplier =
            RAH2_NS::derived_from<Tag, RAH2_NS::random_access_iterator_tag> ? 100 : 1;

        auto const RangeSizedMultiplier = Sized ? 10 : 1;

        COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(
            CS == Common,
            "upper_bound_iter",
            range_type,
            [&]
            {
                for (auto i = 0; i < RangeTypeMultiplier * RangeSizedMultiplier; ++i)
                {
                    auto iter = STD::upper_bound(fwd(r1.begin()), r1.end(), Coord{3, 4});
                    assert((*iter == Coord{3, 6}));
                }
            });
        COMPARE_DURATION_TO_STD_RANGES(
            "upper_bound_range_proj",
            range_type,
            [&]
            {
                for (auto i = 0; i < RangeTypeMultiplier * RangeSizedMultiplier; ++i)
                {
                    auto iter = STD::upper_bound(r2, 1, comp_64, &Coord::x);
                    assert((iter == r2.end()));
                }
            });
    }
    static constexpr bool do_test = RAH2_NS::derived_from<Tag, RAH2_NS::forward_iterator_tag>;
};
void test_upper_bound()
{
    testSuite.test_case("sample");
    testSuite.test_case("return");
    /// [rah2::ranges::upper_bound]
    RAH2_STD::vector<int> data{1, 2, 2, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 5};
    auto const upper = RAH2_NS::ranges::upper_bound(data, 4);
    assert(upper == data.begin() + 10);
    /// [rah2::ranges::upper_bound]

    foreach_range_combination<test_algo<test_upper_bound_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_binary_search_
{
    static bool comp_64(intptr_t a, intptr_t b)
    {
        return b < a;
    }

    template <bool = true>
    void test()
    {
        testSuite.test_case("noproj");
        testSuite.test_case("iter");

        {
            testSuite.test_case("empty");
            RAH2_STD::vector<Coord> in;
            auto r1 = make_test_view_adapter<CS, Tag, Sized>(in);
            auto const found = RAH2_NS::ranges::binary_search(r1.begin(), r1.end(), Coord{3, 0});
            CHECK(not found);
        }
        {
            testSuite.test_case("not_found");
            RAH2_STD::vector<Coord> in{{1, 0}, {2, 0}, {4, 0}};
            auto r1 = make_test_view_adapter<CS, Tag, Sized>(in);
            auto const found = RAH2_NS::ranges::binary_search(r1.begin(), r1.end(), Coord{3, 0});
            CHECK(not found);
        }
        {
            testSuite.test_case("found");
            RAH2_STD::vector<Coord> in{{1, 0}, {2, 0}, {3, 0}, {3, 0}, {3, 0}, {4, 0}};
            auto r1 = make_test_view_adapter<CS, Tag, Sized>(in);
            auto const found = RAH2_NS::ranges::binary_search(r1.begin(), r1.end(), Coord{3, 0});
            CHECK(found);
        }

        testSuite.test_case("range");
        testSuite.test_case("proj");
        {
            testSuite.test_case("empty");
            RAH2_STD::vector<Coord> in;
            auto r1 = make_test_view_adapter<CS, Tag, Sized>(in);
            auto const found = RAH2_NS::ranges::binary_search(r1, 3, comp_64, &Coord::x);
            CHECK(not found);
        }
        {
            testSuite.test_case("not_found");
            RAH2_STD::vector<Coord> in{{4, 0}, {2, 0}, {1, 0}};
            auto r1 = make_test_view_adapter<CS, Tag, Sized>(in);
            auto const found = RAH2_NS::ranges::binary_search(r1, 3, comp_64, &Coord::x);
            CHECK(not found);
        }
        {
            testSuite.test_case("found");
            RAH2_STD::vector<Coord> in{{4, 0}, {3, 0}, {3, 0}, {3, 0}, {2, 0}, {1, 0}};
            auto r1 = make_test_view_adapter<CS, Tag, Sized>(in);
            auto const found = RAH2_NS::ranges::binary_search(r1, 3, comp_64, &Coord::x);
            CHECK(found);
        }
    }
    template <bool = true>
    void test_perf(char const* range_type)
    {
        RAH2_STD::vector<Coord> in(1000000 * RELEASE_MULTIPLIER, Coord{1, 2});
        in.push_back(Coord{3, 4});
        in.push_back(Coord{3, 4});
        in.push_back(Coord{3, 6});
        for (size_t i = 0; i < 1000000 * RELEASE_MULTIPLIER; ++i)
        {
            in.push_back(Coord{10, 10});
        }
        auto r1 = make_test_view_adapter<CS, Tag, Sized>(in);

        RAH2_STD::vector<Coord> in2(1000000 * RELEASE_MULTIPLIER, Coord{3, 4});
        in2.push_back(Coord{1, 4});
        in2.push_back(Coord{1, 2});
        in2.push_back(Coord{1, 3});
        for (size_t i = 0; i < 1000000 * RELEASE_MULTIPLIER; ++i)
        {
            in2.push_back(Coord{10, 10});
        }
        auto r2 = make_test_view_adapter<CS, Tag, Sized>(in2);

        auto const RangeMultiplier = Sized ? 50 * RELEASE_MULTIPLIER : 1;

        auto const IterMultiplier =
            (Sized
             and (CS == CommonOrSent::Sentinel or RAH2_NS::derived_from<Tag, RAH2_NS::random_access_iterator_tag>)) ?
                50 * (RELEASE_MULTIPLIER > 1 ? 10 : 1) :
                1;

        COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(
            CS == Common,
            "binary_search_iter",
            range_type,
            [&]
            {
                for (auto i = 0; i < IterMultiplier; ++i)
                {
                    auto found = STD::binary_search(fwd(r1.begin()), r1.end(), Coord{3, 4});
                    CHECK(found);
                }
            });
        COMPARE_DURATION_TO_STD_RANGES(
            "binary_search_range_proj",
            range_type,
            [&]
            {
                for (auto i = 0; i < RangeMultiplier; ++i)
                {
                    auto found = STD::binary_search(r2, 1, comp_64, &Coord::x);
                    CHECK(found);
                }
            });
    }
    static constexpr bool do_test = RAH2_NS::derived_from<Tag, RAH2_NS::forward_iterator_tag>;
};
void test_binary_search()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::binary_search]
    RAH2_STD::vector<int> vecIn1{1, 2, 2, 3, 4};
    assert(not RAH2_NS::ranges::binary_search(vecIn1, 0));
    assert(RAH2_NS::ranges::binary_search(vecIn1, 1));
    assert(RAH2_NS::ranges::binary_search(vecIn1, 2));
    /// [rah2::ranges::binary_search]

    foreach_range_combination<test_algo<test_binary_search_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_equal_range_
{
    static bool comp_64(intptr_t a, intptr_t b)
    {
        return b < a;
    }

    template <bool = true>
    void test()
    {
        testSuite.test_case("noproj");
        testSuite.test_case("iter");

        {
            testSuite.test_case("empty");
            RAH2_STD::vector<Coord> in;
            auto r1 = make_test_view_adapter<CS, Tag, Sized>(in);
            auto const sub = RAH2_NS::ranges::equal_range(r1.begin(), r1.end(), Coord{3, 0});
            CHECK(RAH2_NS::ranges::empty(sub));
        }
        {
            testSuite.test_case("not_found");
            RAH2_STD::vector<Coord> in{{1, 0}, {2, 0}, {4, 0}};
            auto r1 = make_test_view_adapter<CS, Tag, Sized>(in);
            auto const sub = RAH2_NS::ranges::equal_range(r1.begin(), r1.end(), Coord{3, 0});
            CHECK(RAH2_NS::ranges::empty(sub));
        }
        {
            testSuite.test_case("found");
            RAH2_STD::vector<Coord> in{{1, 0}, {2, 0}, {3, 0}, {3, 0}, {3, 0}, {4, 0}};
            auto r1 = make_test_view_adapter<CS, Tag, Sized>(in);
            auto const sub = RAH2_NS::ranges::equal_range(r1.begin(), r1.end(), Coord{3, 0});
            // WhatIs<decltype(sub)> tutu;
            CHECK(RAH2_NS::ranges::distance(sub.begin(), sub.end()) == 3);
            CHECK((*sub.begin() == Coord{3, 0}));
            CHECK((*sub.end() == Coord{4, 0}));
        }

        testSuite.test_case("range");
        testSuite.test_case("proj");
        {
            testSuite.test_case("empty");
            RAH2_STD::vector<Coord> in;
            auto r1 = make_test_view_adapter<CS, Tag, Sized>(in);
            auto const sub = RAH2_NS::ranges::equal_range(r1, 3, comp_64, &Coord::x);
            CHECK(RAH2_NS::ranges::empty(sub));
        }
        {
            testSuite.test_case("not_found");
            RAH2_STD::vector<Coord> in{{4, 0}, {2, 0}, {1, 0}};
            auto r1 = make_test_view_adapter<CS, Tag, Sized>(in);
            auto const sub = RAH2_NS::ranges::equal_range(r1, 3, comp_64, &Coord::x);
            CHECK(RAH2_NS::ranges::empty(sub));
        }
        {
            testSuite.test_case("found");
            RAH2_STD::vector<Coord> in{{4, 0}, {3, 0}, {3, 0}, {3, 0}, {2, 0}, {1, 0}};
            auto r1 = make_test_view_adapter<CS, Tag, Sized>(in);
            auto const sub = RAH2_NS::ranges::equal_range(r1, 3, comp_64, &Coord::x);
            CHECK(RAH2_NS::ranges::distance(sub.begin(), sub.end()) == 3);
            CHECK((*sub.begin() == Coord{3, 0}));
            CHECK((*sub.end() == Coord{2, 0}));
        }
    }
    template <bool = true>
    void test_perf(char const* range_type)
    {
        RAH2_STD::vector<Coord> in(1000000 * RELEASE_MULTIPLIER, Coord{1, 2});
        in.push_back(Coord{3, 4});
        in.push_back(Coord{3, 4});
        in.push_back(Coord{3, 6});
        auto r1 = make_test_view_adapter<CS, Tag, Sized>(in);

        RAH2_STD::vector<Coord> in2(1000000 * RELEASE_MULTIPLIER, Coord{3, 4});
        in2.push_back(Coord{1, 4});
        in2.push_back(Coord{1, 2});
        in2.push_back(Coord{1, 3});
        auto r2 = make_test_view_adapter<CS, Tag, Sized>(in2);

        auto const RangeTypeMultiplier =
            RAH2_NS::derived_from<Tag, RAH2_NS::random_access_iterator_tag> ? 100 : 1;

        auto const RangeSizedMultiplier = Sized ? 10 : 1;

        COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(
            CS == Common,
            "equal_range_iter",
            range_type,
            [&]
            {
                for (auto i = 0; i < RangeTypeMultiplier * RangeSizedMultiplier; ++i)
                {
                    auto sub = STD::equal_range(fwd(r1.begin()), r1.end(), Coord{3, 4});
                    DONT_OPTIM(sub);
                }
            });
        COMPARE_DURATION_TO_STD_RANGES(
            "equal_range_range_proj",
            range_type,
            [&]
            {
                for (auto i = 0; i < RangeTypeMultiplier * RangeSizedMultiplier; ++i)
                {
                    auto sub = STD::equal_range(r2, 1, comp_64, &Coord::x);
                    DONT_OPTIM(sub);
                }
            });
    }
    static constexpr bool do_test = RAH2_NS::derived_from<Tag, RAH2_NS::forward_iterator_tag>;
};
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

    foreach_range_combination<test_algo<test_equal_range_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_merge_
{
    static bool comp_64(intptr_t a, intptr_t b)
    {
        return a < b;
    }

    template <bool = true>
    void test()
    {
        testSuite.test_case("noproj");
        testSuite.test_case("iter");

        {
            testSuite.test_case("empty");
            RAH2_STD::vector<int> in1;
            RAH2_STD::vector<int> in2;
            auto i1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto i2 = make_test_view_adapter<CS, Tag, Sized>(in2);
            RAH2_STD::vector<int> o(10);
            auto const result =
                RAH2_NS::ranges::merge(i1.begin(), i1.end(), i2.begin(), i2.end(), o.begin());
            CHECK(result.in1 == i1.end());
            CHECK(result.in2 == i2.end());
            CHECK(result.out == o.begin());
        }
        {
            RAH2_STD::vector<int> in1{1, 3, 3, 5};
            RAH2_STD::vector<int> in2{2, 3, 4, 6};
            auto i1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto i2 = make_test_view_adapter<CS, Tag, Sized>(in2);
            RAH2_STD::vector<int> o(9);
            auto const result =
                RAH2_NS::ranges::merge(i1.begin(), i1.end(), i2.begin(), i2.end(), o.begin());
            CHECK(result.in1 == i1.end());
            CHECK(result.in2 == i2.end());
            CHECK(result.out == RAH2_NS::ranges::next(o.begin(), 8));
            CHECK_EQUAL(o, (RAH2_STD::vector<int>{1, 2, 3, 3, 3, 4, 5, 6, 0}));
        }

        testSuite.test_case("range");
        testSuite.test_case("proj");
        {
            testSuite.test_case("empty");
            RAH2_STD::vector<Coord> in1;
            RAH2_STD::vector<Coord> in2;
            auto i1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto i2 = make_test_view_adapter<CS, Tag, Sized>(in2);
            RAH2_STD::vector<Coord> o(10);
            auto const result =
                RAH2_NS::ranges::merge(i1, i2, o.begin(), comp_64, &Coord::x, &Coord::x);
            CHECK(result.in1 == i1.end());
            CHECK(result.in2 == i2.end());
            CHECK(result.out == o.begin());
        }
        {
            RAH2_STD::vector<Coord> in1{{1, 0}, {3, 0}, {3, 0}, {5, 0}};
            RAH2_STD::vector<Coord> in2{{2, 0}, {3, 0}, {4, 0}, {6, 0}};
            auto i1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto i2 = make_test_view_adapter<CS, Tag, Sized>(in2);
            RAH2_STD::vector<Coord> o(9);
            auto const result =
                RAH2_NS::ranges::merge(i1, i2, o.begin(), comp_64, &Coord::x, &Coord::x);
            CHECK(result.in1 == i1.end());
            CHECK(result.in2 == i2.end());
            CHECK(result.out == RAH2_NS::ranges::next(o.begin(), 8));
            CHECK_EQUAL(
                o,
                (RAH2_STD::vector<Coord>{
                    {1, 0}, {2, 0}, {3, 0}, {3, 0}, {3, 0}, {4, 0}, {5, 0}, {6, 0}, {0, 0}}));
        }
    }
    template <bool = true>
    void test_perf(char const* range_type)
    {
        RAH2_STD::vector<Coord> in1;
        RAH2_STD::vector<Coord> in2;
        for (int i = 0; i < 100000 * RELEASE_MULTIPLIER; ++i)
        {
            in1.push_back(Coord{i, 0});
            in2.push_back(Coord{i + 1, 0});
        }

        auto r1 = make_test_view_adapter<CS, Tag, Sized>(in1);
        auto r2 = make_test_view_adapter<CS, Tag, Sized>(in2);

        RAH2_STD::vector<Coord> out(100000 * RELEASE_MULTIPLIER * 2);
        auto rout = make_test_view_adapter<CS, Tag, Sized>(out);

        COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(
            CS == Common,
            "merge_iter",
            range_type,
            [&]
            {
                for (auto i = 0; i < 1; ++i)
                {
                    auto result =
                        STD::merge(fwd(r1.begin()), r1.end(), r2.begin(), r2.end(), rout.begin());
                    DONT_OPTIM(result);
                }
            });
        COMPARE_DURATION_TO_STD_RANGES(
            "merge_range_proj",
            range_type,
            [&]
            {
                for (auto i = 0; i < 1; ++i)
                {
                    auto result = STD::merge(r1, r2, rout.begin(), comp_64, &Coord::x, &Coord::x);
                    DONT_OPTIM(result);
                }
            });
    }
    static constexpr bool do_test = RAH2_NS::derived_from<Tag, RAH2_NS::forward_iterator_tag>;
};
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

    foreach_range_combination<test_algo<test_merge_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_inplace_merge_
{
    static bool comp_64(intptr_t a, intptr_t b)
    {
        return a < b;
    }

    template <bool = true>
    void test()
    {
        testSuite.test_case("noproj");
        testSuite.test_case("iter");

        {
            testSuite.test_case("empty");
            RAH2_STD::vector<int> in1;
            auto i1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto result = RAH2_NS::ranges::inplace_merge(i1.begin(), i1.begin(), i1.end());
            CHECK(result == i1.end());
        }
        {
            RAH2_STD::vector<int> in1{1, 3, 3, 5, 2, 3, 4, 6};
            auto i1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto const result = RAH2_NS::ranges::inplace_merge(
                i1.begin(), RAH2_NS::ranges::next(i1.begin(), 4), i1.end());
            CHECK(result == i1.end());
            CHECK_EQUAL(in1, (RAH2_STD::vector<int>{1, 2, 3, 3, 3, 4, 5, 6}));
        }

        testSuite.test_case("range");
        testSuite.test_case("proj");
        {
            testSuite.test_case("empty");
            RAH2_STD::vector<Coord> in1;
            auto i1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto const result = RAH2_NS::ranges::inplace_merge(i1, i1.begin(), comp_64, &Coord::x);
            CHECK(result == i1.end());
        }
        {
            RAH2_STD::vector<Coord> in1{
                {1, 0}, {3, 0}, {3, 0}, {5, 0}, {2, 0}, {3, 0}, {4, 0}, {6, 0}};
            auto i1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto const result = RAH2_NS::ranges::inplace_merge(
                i1, RAH2_NS::ranges::next(i1.begin(), 4), comp_64, &Coord::x);
            CHECK(result == i1.end());
            CHECK_EQUAL(
                i1,
                (RAH2_STD::vector<Coord>{
                    {1, 0}, {2, 0}, {3, 0}, {3, 0}, {3, 0}, {4, 0}, {5, 0}, {6, 0}}));
        }
    }
    template <bool = true>
    void test_perf(char const* range_type)
    {
        RAH2_STD::vector<Coord> in1;
        for (int i = 0; i < 100000 * RELEASE_MULTIPLIER; ++i)
        {
            in1.push_back(Coord{i, 0});
        }
        for (int i = 0; i < 100000 * RELEASE_MULTIPLIER; ++i)
        {
            in1.push_back(Coord{i + 1, 0});
        }
#ifndef RAH2_USE_EASTL
        COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(
            CS == Common,
            "inplace_merge_iter",
            range_type,
            (
                [in1, fwd]() mutable
                {
                    auto r1 = make_test_view_adapter<CS, Tag, Sized>(in1);
                    auto middle = RAH2_NS::ranges::next(r1.begin(), 100000 * RELEASE_MULTIPLIER);
                    STD::inplace_merge(fwd(r1.begin()), middle, r1.end());
                    CHECK_EQUAL(in1[0], (Coord{0, 0}));
                }));
#endif
        COMPARE_DURATION_TO_STD_RANGES(
            "inplace_merge_range_proj",
            range_type,
            (
                [in1]() mutable
                {
                    auto r1 = make_test_view_adapter<CS, Tag, Sized>(in1);
                    auto middle = RAH2_NS::ranges::next(r1.begin(), 100000 * RELEASE_MULTIPLIER);
                    STD::inplace_merge(r1, middle, comp_64, &Coord::x);
                    CHECK_EQUAL(in1[0], (Coord{0, 0}));
                }));
    }
    static constexpr bool do_test = RAH2_NS::derived_from<Tag, RAH2_NS::bidirectional_iterator_tag>;
};
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

    foreach_range_combination<test_algo<test_inplace_merge_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_includes_
{
    static bool comp_64(intptr_t a, intptr_t b)
    {
        return a > b;
    }

    template <bool = true>
    void test()
    {
        testSuite.test_case("noproj");
        testSuite.test_case("iter");

        {
            testSuite.test_case("empty");
            RAH2_STD::vector<int> in1, in2;
            auto i1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto i2 = make_test_view_adapter<CS, Tag, Sized>(in2);
            auto const result = RAH2_NS::ranges::includes(i1.begin(), i1.end(), i2.begin(), i2.end());
            CHECK(result);
        }
        {
            RAH2_STD::vector<int> in1{1, 3, 3, 5}, in2{};
            auto i1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto i2 = make_test_view_adapter<CS, Tag, Sized>(in2);
            auto const result = RAH2_NS::ranges::includes(i1.begin(), i1.end(), i2.begin(), i2.end());
            CHECK(result);
        }
        {
            RAH2_STD::vector<int> in1{1, 3, 3, 5}, in2{4, 5};
            auto i1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto i2 = make_test_view_adapter<CS, Tag, Sized>(in2);
            auto const result = RAH2_NS::ranges::includes(i1.begin(), i1.end(), i2.begin(), i2.end());
            CHECK(result == false);
        }
        {
            RAH2_STD::vector<int> in1{1, 3, 3, 5}, in2{1, 3, 3, 5, 6};
            auto i1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto i2 = make_test_view_adapter<CS, Tag, Sized>(in2);
            auto const result = RAH2_NS::ranges::includes(i1.begin(), i1.end(), i2.begin(), i2.end());
            CHECK(result == false);
        }
        {
            RAH2_STD::vector<int> in1{1, 3, 3, 5}, in2{3, 5};
            auto i1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto i2 = make_test_view_adapter<CS, Tag, Sized>(in2);
            auto const result = RAH2_NS::ranges::includes(i1.begin(), i1.end(), i2.begin(), i2.end());
            CHECK(result);
        }

        testSuite.test_case("range");
        testSuite.test_case("proj");
        {
            testSuite.test_case("empty");
            RAH2_STD::vector<Coord> in1, in2;
            auto i1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto i2 = make_test_view_adapter<CS, Tag, Sized>(in2);
            auto const result = RAH2_NS::ranges::includes(i1, i2, comp_64, &Coord::x, &Coord::x);
            CHECK(result);
        }
        {
            RAH2_STD::vector<Coord> in1{{5, 0}, {3, 0}, {3, 0}, {1, 0}}, in2{};
            auto i1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto i2 = make_test_view_adapter<CS, Tag, Sized>(in2);
            auto const result = RAH2_NS::ranges::includes(i1, i2, comp_64, &Coord::x, &Coord::x);
            CHECK(result);
        }
        {
            RAH2_STD::vector<Coord> in1{{5, 0}, {3, 0}, {3, 0}, {1, 0}}, in2{{2, 0}, {1, 0}};
            auto i1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto i2 = make_test_view_adapter<CS, Tag, Sized>(in2);
            auto const result = RAH2_NS::ranges::includes(i1, i2, comp_64, &Coord::x, &Coord::x);
            CHECK(result == false);
        }
        {
            RAH2_STD::vector<Coord> in1{{5, 0}, {3, 0}, {3, 0}, {1, 0}};
            RAH2_STD::vector<Coord> in2{{5, 0}, {3, 0}, {3, 0}, {1, 0}, {0, 0}};
            auto i1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto i2 = make_test_view_adapter<CS, Tag, Sized>(in2);
            auto const result = RAH2_NS::ranges::includes(i1, i2, comp_64, &Coord::x, &Coord::x);
            CHECK(result == false);
        }
        {
            RAH2_STD::vector<Coord> in1{{5, 0}, {3, 0}, {3, 0}, {2, 0}, {1, 0}, {-2, 0}};
            RAH2_STD::vector<Coord> in2{{3, 0}, {1, 0}};
            auto i1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto i2 = make_test_view_adapter<CS, Tag, Sized>(in2);
            auto const result = RAH2_NS::ranges::includes(i1, i2, comp_64, &Coord::x, &Coord::x);
            CHECK(result);
        }
    }
    template <bool = true>
    void test_perf(char const* range_type)
    {
#ifndef RAH2_USE_EASTL
        {
            RAH2_STD::vector<Coord> in1;
            RAH2_STD::vector<Coord> in2;
            in1.push_back(Coord{0, 0});
            for (int i = 1; i < 100000 * RELEASE_MULTIPLIER; ++i)
            {
                in1.push_back(Coord{i, 0});
                in2.push_back(Coord{i, 0});
            }
            in1.push_back(Coord{100000, 0});

            auto r1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto r2 = make_test_view_adapter<CS, Tag, Sized>(in2);

            COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(
                CS == Common,
                "includes_iter",
                range_type,
                [&]
                {
                    for (auto i = 0; i < 1; ++i)
                    {
                        auto result = STD::includes(fwd(r1.begin()), r1.end(), r2.begin(), r2.end());
                        CHECK(result);
                    }
                });
        }
#endif
        {
            RAH2_STD::vector<Coord> in1;
            RAH2_STD::vector<Coord> in2;
            in1.push_back(Coord{100000 * RELEASE_MULTIPLIER, 0});
            for (int i = 1; i < 100000 * RELEASE_MULTIPLIER; ++i)
            {
                in1.push_back(Coord{100000 * RELEASE_MULTIPLIER - i, 0});
                in2.push_back(Coord{100000 * RELEASE_MULTIPLIER - i, 0});
            }
            in1.push_back(Coord{100000 * RELEASE_MULTIPLIER - 100000, 0});

            auto r1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto r2 = make_test_view_adapter<CS, Tag, Sized>(in2);

            COMPARE_DURATION_TO_STD_RANGES(
                "includes_range_proj",
                range_type,
                [&]
                {
                    for (auto i = 0; i < 1; ++i)
                    {
                        auto result = STD::includes(r1, r2, comp_64, &Coord::x, &Coord::x);
                        CHECK(result);
                    }
                });
        }
    }
    static constexpr bool do_test = RAH2_NS::derived_from<Tag, RAH2_NS::input_iterator_tag>;
};
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

    foreach_range_combination<test_algo<test_includes_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_set_difference_
{
    static bool comp_64(intptr_t a, intptr_t b)
    {
        return a > b;
    }

    template <bool = true>
    void test()
    {
        testSuite.test_case("noproj");
        testSuite.test_case("iter");
        {
            // No commun elements : No element are removed
            RAH2_STD::vector<int> in1{1, 3, 3, 5}, in2{2, 4, 18};
            auto i1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto i2 = make_test_view_adapter<CS, Tag, Sized>(in2);
            RAH2_STD::vector<int> o{0, 0, 0, 0, 0};
            auto const result = RAH2_NS::ranges::set_difference(
                i1.begin(), i1.end(), i2.begin(), i2.end(), o.begin());
            CHECK(result.in == i1.end());
            CHECK(result.out == o.begin() + 4);
            CHECK(o == (RAH2_STD::vector<int>{1, 3, 3, 5, 0}));
        }
        {
            // Intersection : Some element are removed
            RAH2_STD::vector<int> in1{1, 3, 3, 5}, in2{3, 4, 5};
            auto i1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto i2 = make_test_view_adapter<CS, Tag, Sized>(in2);
            RAH2_STD::vector<int> o{0, 0, 0, 0, 0};
            auto const result = RAH2_NS::ranges::set_difference(
                i1.begin(), i1.end(), i2.begin(), i2.end(), o.begin());
            CHECK(result.in == i1.end());
            CHECK(result.out == o.begin() + 2);
            CHECK(o == (RAH2_STD::vector<int>{1, 3, 0, 0, 0}));
        }
        {
            // R1 is empty : No element is removed
            RAH2_STD::vector<int> in1{}, in2{1, 3, 3, 5};
            auto i1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto i2 = make_test_view_adapter<CS, Tag, Sized>(in2);
            RAH2_STD::vector<int> o{0, 0, 0, 0, 0};
            auto const result = RAH2_NS::ranges::set_difference(
                i1.begin(), i1.end(), i2.begin(), i2.end(), o.begin());
            CHECK(result.in == i1.end());
            CHECK(result.out == o.begin());
            CHECK(o == (RAH2_STD::vector<int>{0, 0, 0, 0, 0}));
        }
        {
            // R2 is empty : No element is removed
            RAH2_STD::vector<int> in1{1, 3, 3, 5}, in2{};
            auto i1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto i2 = make_test_view_adapter<CS, Tag, Sized>(in2);
            RAH2_STD::vector<int> o{0, 0, 0, 0, 0};
            auto const result = RAH2_NS::ranges::set_difference(
                i1.begin(), i1.end(), i2.begin(), i2.end(), o.begin());
            CHECK(result.in == i1.end());
            CHECK(result.out == o.begin() + 4);
            CHECK(o == (RAH2_STD::vector<int>{1, 3, 3, 5, 0}));
        }
        {
            // R1 and R2 are equal. All elements are removed.
            RAH2_STD::vector<int> in1{1, 3, 3, 5}, in2{1, 3, 3, 5};
            auto i1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto i2 = make_test_view_adapter<CS, Tag, Sized>(in2);
            RAH2_STD::vector<int> o{0, 0, 0, 0, 0};
            auto const result = RAH2_NS::ranges::set_difference(
                i1.begin(), i1.end(), i2.begin(), i2.end(), o.begin());
            CHECK(result.in == i1.end());
            CHECK(result.out == o.begin());
            CHECK(o == (RAH2_STD::vector<int>{0, 0, 0, 0, 0}));
        }
        {
            // R1 and R2 are empty
            testSuite.test_case("empty");
            RAH2_STD::vector<int> in1, in2;
            auto i1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto i2 = make_test_view_adapter<CS, Tag, Sized>(in2);
            RAH2_STD::vector<int> o{0, 0, 0, 0, 0};
            auto const result = RAH2_NS::ranges::set_difference(
                i1.begin(), i1.end(), i2.begin(), i2.end(), o.begin());
            CHECK(result.in == i1.end());
            CHECK(result.out == o.begin());
            CHECK(o == (RAH2_STD::vector<int>{0, 0, 0, 0, 0}));
        }
        {
            // All elements are remove
            RAH2_STD::vector<int> in1{1, 3, 3, 5}, in2{1, 3, 3, 3, 5, 6};
            auto i1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto i2 = make_test_view_adapter<CS, Tag, Sized>(in2);
            RAH2_STD::vector<int> o{0, 0, 0, 0, 0};
            auto const result = RAH2_NS::ranges::set_difference(
                i1.begin(), i1.end(), i2.begin(), i2.end(), o.begin());
            CHECK(result.in == i1.end());
            CHECK(result.out == o.begin());
            CHECK(o == (RAH2_STD::vector<int>(5)));
        }

        testSuite.test_case("range");
        testSuite.test_case("proj");
        {
            // No commun elements : No element are removed
            RAH2_STD::vector<Coord> in1{{5, 0}, {3, 0}, {3, 0}, {1, 0}}, in2{{18, 0}, {4, 0}, {2, 0}};
            auto i1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto i2 = make_test_view_adapter<CS, Tag, Sized>(in2);
            RAH2_STD::vector<Coord> o(5);
            auto const result = RAH2_NS::ranges::set_difference(
                i1.begin(), i1.end(), i2.begin(), i2.end(), o.begin(), &comp_64, &Coord::x, &Coord::x);
            CHECK(result.in == i1.end());
            CHECK(result.out == o.begin() + 4);
            CHECK(o == (RAH2_STD::vector<Coord>{{5, 0}, {3, 0}, {3, 0}, {1, 0}, {0, 0}}));
        }
        {
            // Intersection : Some element are removed
            RAH2_STD::vector<Coord> in1{{5, 0}, {3, 0}, {3, 0}, {1, 0}}, in2{{5, 0}, {4, 0}, {3, 0}};
            auto i1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto i2 = make_test_view_adapter<CS, Tag, Sized>(in2);
            RAH2_STD::vector<Coord> o(5);
            auto const result = RAH2_NS::ranges::set_difference(
                i1.begin(), i1.end(), i2.begin(), i2.end(), o.begin(), &comp_64, &Coord::x, &Coord::x);
            CHECK(result.in == i1.end());
            CHECK(result.out == o.begin() + 2);
            CHECK(o == (RAH2_STD::vector<Coord>{{3, 0}, {1, 0}, {0, 0}, {0, 0}, {0, 0}}));
        }
        {
            // R1 is empty : No element is removed
            RAH2_STD::vector<Coord> in1{}, in2{{5, 0}, {3, 0}, {3, 0}, {1, 0}};
            auto i1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto i2 = make_test_view_adapter<CS, Tag, Sized>(in2);
            RAH2_STD::vector<Coord> o(5);
            auto const result = RAH2_NS::ranges::set_difference(
                i1.begin(), i1.end(), i2.begin(), i2.end(), o.begin(), &comp_64, &Coord::x, &Coord::x);
            CHECK(result.in == i1.end());
            CHECK(result.out == o.begin());
            CHECK(o == (RAH2_STD::vector<Coord>(5)));
        }
        {
            // R2 is empty : No element is removed
            RAH2_STD::vector<Coord> in1{{5, 0}, {3, 0}, {3, 0}, {1, 0}}, in2{};
            auto i1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto i2 = make_test_view_adapter<CS, Tag, Sized>(in2);
            RAH2_STD::vector<Coord> o(5);
            auto const result = RAH2_NS::ranges::set_difference(
                i1.begin(), i1.end(), i2.begin(), i2.end(), o.begin(), &comp_64, &Coord::x, &Coord::x);
            CHECK(result.in == i1.end());
            CHECK(result.out == o.begin() + 4);
            CHECK(o == (RAH2_STD::vector<Coord>{{5, 0}, {3, 0}, {3, 0}, {1, 0}, {0, 0}}));
        }
        {
            // R1 and R2 are equal. All elements are removed.
            RAH2_STD::vector<Coord> in1{{5, 0}, {3, 0}, {3, 0}, {1, 0}},
                in2{{5, 0}, {3, 0}, {3, 0}, {1, 0}};
            auto i1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto i2 = make_test_view_adapter<CS, Tag, Sized>(in2);
            RAH2_STD::vector<Coord> o(5);
            auto const result = RAH2_NS::ranges::set_difference(
                i1.begin(), i1.end(), i2.begin(), i2.end(), o.begin(), &comp_64, &Coord::x, &Coord::x);
            CHECK(result.in == i1.end());
            CHECK(result.out == o.begin());
            CHECK(o == (RAH2_STD::vector<Coord>(5)));
        }
        {
            // R1 and R2 are empty
            testSuite.test_case("empty");
            RAH2_STD::vector<Coord> in1, in2;
            auto i1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto i2 = make_test_view_adapter<CS, Tag, Sized>(in2);
            RAH2_STD::vector<Coord> o(5);
            auto const result = RAH2_NS::ranges::set_difference(
                i1.begin(), i1.end(), i2.begin(), i2.end(), o.begin(), &comp_64, &Coord::x, &Coord::x);
            CHECK(result.in == i1.end());
            CHECK(result.out == o.begin());
            CHECK(o == (RAH2_STD::vector<Coord>(5)));
        }
        {
            // All elements are remove
            RAH2_STD::vector<Coord> in1{{5, 0}, {3, 0}, {3, 0}, {1, 0}},
                in2{{6, 0}, {5, 0}, {3, 0}, {3, 0}, {3, 0}, {1, 0}};
            auto i1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto i2 = make_test_view_adapter<CS, Tag, Sized>(in2);
            RAH2_STD::vector<Coord> o(5);
            auto const result = RAH2_NS::ranges::set_difference(
                i1.begin(), i1.end(), i2.begin(), i2.end(), o.begin(), &comp_64, &Coord::x, &Coord::x);
            CHECK(result.in == i1.end());
            CHECK(result.out == o.begin());
            CHECK(o == (RAH2_STD::vector<Coord>(5)));
        }
    }
    template <bool = true>
    void test_perf(char const* range_type)
    {
        {
            RAH2_STD::vector<Coord> in1;
            RAH2_STD::vector<Coord> in2;
            in1.push_back(Coord{0, 0});
            for (int i = 1; i < 100000 * RELEASE_MULTIPLIER; ++i)
            {
                in1.push_back(Coord{i, 0});
                if (i % 2 == 1)
                    in2.push_back(Coord{i, 0});
            }

            auto r1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto r2 = make_test_view_adapter<CS, Tag, Sized>(in2);

            RAH2_STD::vector<Coord> out(in1.size());

            COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(
                CS == Common,
                "set_difference_iter",
                range_type,
                [&]
                {
                    for (auto i = 0; i < 1; ++i)
                    {
                        auto result = STD::set_difference(
                            fwd(r1.begin()), r1.end(), r2.begin(), r2.end(), out.begin());
                        DONT_OPTIM(result);
                    }
                });
        }

        {
            RAH2_STD::vector<Coord> in1;
            RAH2_STD::vector<Coord> in2;
            in1.push_back(Coord{100000 * RELEASE_MULTIPLIER, 0});
            for (int i = 1; i < 100000 * RELEASE_MULTIPLIER; ++i)
            {
                in1.push_back(Coord{100000 * RELEASE_MULTIPLIER - i, 0});
                if (i % 2 == 1)
                    in2.push_back(Coord{100000 * RELEASE_MULTIPLIER - i, 0});
            }

            auto r1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto r2 = make_test_view_adapter<CS, Tag, Sized>(in2);

            RAH2_STD::vector<Coord> out(in1.size());

            COMPARE_DURATION_TO_STD_RANGES(
                "set_difference_range_proj",
                range_type,
                [&]
                {
                    for (auto i = 0; i < 1; ++i)
                    {
                        auto result =
                            STD::set_difference(r1, r2, out.begin(), &comp_64, &Coord::x, &Coord::x);
                        DONT_OPTIM(result);
                    }
                });
        }
    }
    static constexpr bool do_test = RAH2_NS::derived_from<Tag, RAH2_NS::input_iterator_tag>;
};
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

    foreach_range_combination<test_algo<test_set_difference_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_set_intersection_
{
    static bool comp_64(intptr_t a, intptr_t b)
    {
        return a > b;
    }

    template <bool = true>
    void test()
    {
        testSuite.test_case("noproj");
        testSuite.test_case("iter");
        {
            // No common elements : No elements are removed
            RAH2_STD::vector<int> in1{1, 3, 3, 5}, in2{2, 4, 18};
            auto i1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto i2 = make_test_view_adapter<CS, Tag, Sized>(in2);
            RAH2_STD::vector<int> o(5);
            auto const result = RAH2_NS::ranges::set_intersection(
                i1.begin(), i1.end(), i2.begin(), i2.end(), o.begin());
            CHECK(result.in1 == i1.end());
            CHECK(result.in2 == i2.end());
            CHECK(result.out == o.begin());
            CHECK(o == (RAH2_STD::vector<int>(5)));
        }
        {
            // Intersection : Some elements are removed
            RAH2_STD::vector<int> in1{1, 3, 3, 5}, in2{3, 4, 5};
            auto i1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto i2 = make_test_view_adapter<CS, Tag, Sized>(in2);
            RAH2_STD::vector<int> o(5);
            auto const result = RAH2_NS::ranges::set_intersection(
                i1.begin(), i1.end(), i2.begin(), i2.end(), o.begin());
            CHECK(result.in1 == i1.end());
            CHECK(result.in2 == i2.end());
            CHECK(result.out == o.begin() + 2);
            CHECK(o == (RAH2_STD::vector<int>{3, 5, 0, 0, 0}));
        }
        {
            // R1 is empty : No element is removed
            RAH2_STD::vector<int> in1{}, in2{1, 3, 3, 5};
            auto i1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto i2 = make_test_view_adapter<CS, Tag, Sized>(in2);
            RAH2_STD::vector<int> o(5);
            auto const result = RAH2_NS::ranges::set_intersection(
                i1.begin(), i1.end(), i2.begin(), i2.end(), o.begin());
            CHECK(result.in1 == i1.end());
            CHECK(result.in2 == i2.end());
            CHECK(result.out == o.begin());
            CHECK(o == (RAH2_STD::vector<int>(5)));
        }
        {
            // R2 is empty : No element is kept
            RAH2_STD::vector<int> in1{1, 3, 3, 5}, in2{};
            auto i1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto i2 = make_test_view_adapter<CS, Tag, Sized>(in2);
            RAH2_STD::vector<int> o(5);
            auto const result = RAH2_NS::ranges::set_intersection(
                i1.begin(), i1.end(), i2.begin(), i2.end(), o.begin());
            CHECK(result.in1 == i1.end());
            CHECK(result.in2 == i2.end());
            CHECK(result.out == o.begin());
            CHECK(o == (RAH2_STD::vector<int>(5)));
        }
        {
            // R1 and R2 are equal. All elements are kept.
            RAH2_STD::vector<int> in1{1, 3, 3, 5}, in2{1, 3, 3, 5};
            auto i1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto i2 = make_test_view_adapter<CS, Tag, Sized>(in2);
            RAH2_STD::vector<int> o(5);
            auto const result = RAH2_NS::ranges::set_intersection(
                i1.begin(), i1.end(), i2.begin(), i2.end(), o.begin());
            CHECK(result.in1 == i1.end());
            CHECK(result.in2 == i2.end());
            CHECK(result.out == o.begin() + 4);
            CHECK(o == (RAH2_STD::vector<int>{1, 3, 3, 5, 0}));
        }
        {
            // R1 and R2 are empty
            testSuite.test_case("empty");
            RAH2_STD::vector<int> in1, in2;
            auto i1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto i2 = make_test_view_adapter<CS, Tag, Sized>(in2);
            RAH2_STD::vector<int> o(5);
            auto const result = RAH2_NS::ranges::set_intersection(
                i1.begin(), i1.end(), i2.begin(), i2.end(), o.begin());
            CHECK(result.in1 == i1.end());
            CHECK(result.in2 == i2.end());
            CHECK(result.out == o.begin());
            CHECK(o == (RAH2_STD::vector<int>(5)));
        }
        {
            // All elements are kept
            RAH2_STD::vector<int> in1{1, 3, 3, 5}, in2{1, 3, 3, 3, 5, 6};
            auto i1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto i2 = make_test_view_adapter<CS, Tag, Sized>(in2);
            RAH2_STD::vector<int> o(5);
            auto const result = RAH2_NS::ranges::set_intersection(
                i1.begin(), i1.end(), i2.begin(), i2.end(), o.begin());
            CHECK(result.in1 == i1.end());
            CHECK(result.in2 == i2.end());
            CHECK(result.out == o.begin() + 4);
            CHECK(o == (RAH2_STD::vector<int>{1, 3, 3, 5, 0}));
        }

        testSuite.test_case("range");
        testSuite.test_case("proj");
        {
            // No common elements : All elements are removed
            RAH2_STD::vector<Coord> in1{{5, 0}, {3, 0}, {3, 0}, {1, 0}}, in2{{18, 0}, {4, 0}, {2, 0}};
            auto i1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto i2 = make_test_view_adapter<CS, Tag, Sized>(in2);
            RAH2_STD::vector<Coord> o(5);
            auto const result = RAH2_NS::ranges::set_intersection(
                i1.begin(), i1.end(), i2.begin(), i2.end(), o.begin(), &comp_64, &Coord::x, &Coord::x);
            CHECK(result.in1 == i1.end());
            CHECK(result.in2 == i2.end());
            CHECK(result.out == o.begin());
            CHECK(o == (RAH2_STD::vector<Coord>(5)));
        }
        {
            // Intersection : Some element are removed
            RAH2_STD::vector<Coord> in1{{5, 0}, {3, 0}, {3, 0}, {1, 0}}, in2{{5, 0}, {4, 0}, {3, 0}};
            auto i1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto i2 = make_test_view_adapter<CS, Tag, Sized>(in2);
            RAH2_STD::vector<Coord> o(5);
            auto const result = RAH2_NS::ranges::set_intersection(
                i1.begin(), i1.end(), i2.begin(), i2.end(), o.begin(), &comp_64, &Coord::x, &Coord::x);
            CHECK(result.in1 == i1.end());
            CHECK(result.in2 == i2.end());
            CHECK(result.out == o.begin() + 2);
            CHECK(o == (RAH2_STD::vector<Coord>{{5, 0}, {3, 0}, {0, 0}, {0, 0}, {0, 0}}));
        }
        {
            // R1 is empty : All elements are removed
            RAH2_STD::vector<Coord> in1{}, in2{{5, 0}, {3, 0}, {3, 0}, {1, 0}};
            auto i1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto i2 = make_test_view_adapter<CS, Tag, Sized>(in2);
            RAH2_STD::vector<Coord> o(5);
            auto const result = RAH2_NS::ranges::set_intersection(
                i1.begin(), i1.end(), i2.begin(), i2.end(), o.begin(), &comp_64, &Coord::x, &Coord::x);
            CHECK(result.in1 == i1.end());
            CHECK(result.in2 == i2.end());
            CHECK(result.out == o.begin());
            CHECK(o == (RAH2_STD::vector<Coord>(5)));
        }
        {
            // R2 is empty : All elements are removed
            RAH2_STD::vector<Coord> in1{{5, 0}, {3, 0}, {3, 0}, {1, 0}}, in2{};
            auto i1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto i2 = make_test_view_adapter<CS, Tag, Sized>(in2);
            RAH2_STD::vector<Coord> o(5);
            auto const result = RAH2_NS::ranges::set_intersection(
                i1.begin(), i1.end(), i2.begin(), i2.end(), o.begin(), &comp_64, &Coord::x, &Coord::x);
            CHECK(result.in1 == i1.end());
            CHECK(result.in2 == i2.end());
            CHECK(result.out == o.begin());
            CHECK(o == (RAH2_STD::vector<Coord>(5)));
        }
        {
            // R1 and R2 are equal. No elements are removed
            RAH2_STD::vector<Coord> in1{{5, 0}, {3, 0}, {3, 0}, {1, 0}},
                in2{{5, 0}, {3, 0}, {3, 0}, {1, 0}};
            auto i1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto i2 = make_test_view_adapter<CS, Tag, Sized>(in2);
            RAH2_STD::vector<Coord> o(5);
            auto const result = RAH2_NS::ranges::set_intersection(
                i1.begin(), i1.end(), i2.begin(), i2.end(), o.begin(), &comp_64, &Coord::x, &Coord::x);
            CHECK(result.in1 == i1.end());
            CHECK(result.in2 == i2.end());
            CHECK(result.out == o.begin() + 4);
            CHECK(o == (RAH2_STD::vector<Coord>{{5, 0}, {3, 0}, {3, 0}, {1, 0}, {0, 0}}));
        }
        {
            // R1 and R2 are empty
            testSuite.test_case("empty");
            RAH2_STD::vector<Coord> in1, in2;
            auto i1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto i2 = make_test_view_adapter<CS, Tag, Sized>(in2);
            RAH2_STD::vector<Coord> o(5);
            auto const result = RAH2_NS::ranges::set_intersection(
                i1.begin(), i1.end(), i2.begin(), i2.end(), o.begin(), &comp_64, &Coord::x, &Coord::x);
            CHECK(result.in1 == i1.end());
            CHECK(result.in2 == i2.end());
            CHECK(result.out == o.begin());
            CHECK(o == (RAH2_STD::vector<Coord>(5)));
        }
        {
            // R2 is biger but no R1 elements are removed
            RAH2_STD::vector<Coord> in1{{5, 0}, {3, 0}, {3, 0}, {1, 0}},
                in2{{6, 0}, {5, 0}, {3, 0}, {3, 0}, {3, 0}, {1, 0}};
            auto i1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto i2 = make_test_view_adapter<CS, Tag, Sized>(in2);
            RAH2_STD::vector<Coord> o(5);
            auto const result = RAH2_NS::ranges::set_intersection(
                i1.begin(), i1.end(), i2.begin(), i2.end(), o.begin(), &comp_64, &Coord::x, &Coord::x);
            CHECK(result.in1 == i1.end());
            CHECK(result.in2 == i2.end());
            CHECK(result.out == o.begin() + 4);
            CHECK(o == (RAH2_STD::vector<Coord>{{5, 0}, {3, 0}, {3, 0}, {1, 0}, {0, 0}}));
        }
    }
    template <bool = true>
    void test_perf(char const* range_type)
    {
        {
            RAH2_STD::vector<Coord> in1;
            RAH2_STD::vector<Coord> in2;
            in1.push_back(Coord{0, 0});
            for (int i = 1; i < 100000 * RELEASE_MULTIPLIER; ++i)
            {
                in1.push_back(Coord{i, 0});
                if (i % 2 == 1)
                    in2.push_back(Coord{i, 0});
            }

            auto r1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto r2 = make_test_view_adapter<CS, Tag, Sized>(in2);

            RAH2_STD::vector<Coord> out(in1.size());

            COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(
                CS == Common,
                "set_intersection_iter",
                range_type,
                [&]
                {
                    for (auto i = 0; i < 1; ++i)
                    {
                        auto result = STD::set_intersection(
                            fwd(r1.begin()), r1.end(), r2.begin(), r2.end(), out.begin());
                        DONT_OPTIM(result);
                    }
                });
        }

        {
            RAH2_STD::vector<Coord> in1;
            RAH2_STD::vector<Coord> in2;
            in1.push_back(Coord{100000 * RELEASE_MULTIPLIER, 0});
            for (int i = 1; i < 100000 * RELEASE_MULTIPLIER; ++i)
            {
                in1.push_back(Coord{100000 * RELEASE_MULTIPLIER - i, 0});
                if (i % 2 == 1)
                    in2.push_back(Coord{100000 * RELEASE_MULTIPLIER - i, 0});
            }

            auto r1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto r2 = make_test_view_adapter<CS, Tag, Sized>(in2);

            RAH2_STD::vector<Coord> out(in1.size());

            COMPARE_DURATION_TO_STD_RANGES(
                "set_intersection_range_proj",
                range_type,
                [&]
                {
                    for (auto i = 0; i < 1; ++i)
                    {
                        auto result = STD::set_intersection(
                            r1, r2, out.begin(), comp_64, &Coord::x, &Coord::x);
                        DONT_OPTIM(result);
                    }
                });
        }
    }
    static constexpr bool do_test = RAH2_NS::derived_from<Tag, RAH2_NS::input_iterator_tag>;
};
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

    foreach_range_combination<test_algo<test_set_intersection_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_set_symmetric_difference_
{
    static bool comp_64(intptr_t a, intptr_t b)
    {
        return a > b;
    }

    template <bool = true>
    void test()
    {
        testSuite.test_case("noproj");
        testSuite.test_case("iter");
        {
            // No common elements : Keep all
            RAH2_STD::vector<int> in1{1, 3, 3, 5}, in2{2, 4, 18};
            auto i1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto i2 = make_test_view_adapter<CS, Tag, Sized>(in2);
            RAH2_STD::vector<int> o(10);
            auto const result = RAH2_NS::ranges::set_symmetric_difference(
                i1.begin(), i1.end(), i2.begin(), i2.end(), o.begin());
            CHECK(result.in1 == i1.end());
            CHECK(result.in2 == i2.end());
            CHECK(result.out == o.begin() + 7);
            CHECK(o == (RAH2_STD::vector<int>{1, 2, 3, 3, 4, 5, 18, 0, 0, 0}));
        }
        {
            // Intersection : Some elements are removed
            RAH2_STD::vector<int> in1{1, 3, 3, 5}, in2{3, 4, 5};
            auto i1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto i2 = make_test_view_adapter<CS, Tag, Sized>(in2);
            RAH2_STD::vector<int> o(10);
            auto const result = RAH2_NS::ranges::set_symmetric_difference(
                i1.begin(), i1.end(), i2.begin(), i2.end(), o.begin());
            CHECK(result.in1 == i1.end());
            CHECK(result.in2 == i2.end());
            CHECK(result.out == o.begin() + 3);
            CHECK(o == (RAH2_STD::vector<int>{1, 3, 4, 0, 0, 0, 0, 0, 0, 0}));
        }
        {
            // R1 is empty : Keep R2
            RAH2_STD::vector<int> in1{}, in2{1, 3, 3, 5};
            auto i1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto i2 = make_test_view_adapter<CS, Tag, Sized>(in2);
            RAH2_STD::vector<int> o(10);
            auto const result = RAH2_NS::ranges::set_symmetric_difference(
                i1.begin(), i1.end(), i2.begin(), i2.end(), o.begin());
            CHECK(result.in1 == i1.end());
            CHECK(result.in2 == i2.end());
            CHECK(result.out == o.begin() + 4);
            CHECK(o == (RAH2_STD::vector<int>{1, 3, 3, 5, 0, 0, 0, 0, 0, 0}));
        }
        {
            // R2 is empty : Keep R1
            RAH2_STD::vector<int> in1{1, 3, 3, 5}, in2{};
            auto i1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto i2 = make_test_view_adapter<CS, Tag, Sized>(in2);
            RAH2_STD::vector<int> o(10);
            auto const result = RAH2_NS::ranges::set_symmetric_difference(
                i1.begin(), i1.end(), i2.begin(), i2.end(), o.begin());
            CHECK(result.in1 == i1.end());
            CHECK(result.in2 == i2.end());
            CHECK(result.out == o.begin() + 4);
            CHECK(o == (RAH2_STD::vector<int>{1, 3, 3, 5, 0, 0, 0, 0, 0, 0}));
        }
        {
            // R1 and R2 are equal. All elements are removed.
            RAH2_STD::vector<int> in1{1, 3, 3, 5}, in2{1, 3, 3, 5};
            auto i1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto i2 = make_test_view_adapter<CS, Tag, Sized>(in2);
            RAH2_STD::vector<int> o(10);
            auto const result = RAH2_NS::ranges::set_symmetric_difference(
                i1.begin(), i1.end(), i2.begin(), i2.end(), o.begin());
            CHECK(result.in1 == i1.end());
            CHECK(result.in2 == i2.end());
            CHECK(result.out == o.begin());
            CHECK(o == (RAH2_STD::vector<int>(10)));
        }
        {
            // R1 and R2 are empty
            testSuite.test_case("empty");
            RAH2_STD::vector<int> in1, in2;
            auto i1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto i2 = make_test_view_adapter<CS, Tag, Sized>(in2);
            RAH2_STD::vector<int> o(10);
            auto const result = RAH2_NS::ranges::set_symmetric_difference(
                i1.begin(), i1.end(), i2.begin(), i2.end(), o.begin());
            CHECK(result.in1 == i1.end());
            CHECK(result.in2 == i2.end());
            CHECK(result.out == o.begin());
            CHECK(o == (RAH2_STD::vector<int>(10)));
        }
        {
            // More elements in R2
            RAH2_STD::vector<int> in1{1, 3, 3, 5}, in2{1, 3, 3, 3, 5, 6};
            auto i1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto i2 = make_test_view_adapter<CS, Tag, Sized>(in2);
            RAH2_STD::vector<int> o(10);
            auto const result = RAH2_NS::ranges::set_symmetric_difference(
                i1.begin(), i1.end(), i2.begin(), i2.end(), o.begin());
            CHECK(result.in1 == i1.end());
            CHECK(result.in2 == i2.end());
            CHECK(result.out == o.begin() + 2);
            CHECK(o == (RAH2_STD::vector<int>{3, 6, 0, 0, 0, 0, 0, 0, 0, 0}));
        }

        testSuite.test_case("range");
        testSuite.test_case("proj");
        {
            // No common elements : Keep all
            RAH2_STD::vector<Coord> in1{{5, 0}, {3, 0}, {3, 0}, {1, 0}}, in2{{18, 0}, {4, 0}, {2, 0}};
            auto i1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto i2 = make_test_view_adapter<CS, Tag, Sized>(in2);
            RAH2_STD::vector<Coord> o(10);
            auto const result = RAH2_NS::ranges::set_symmetric_difference(
                i1.begin(), i1.end(), i2.begin(), i2.end(), o.begin(), &comp_64, &Coord::x, &Coord::x);
            CHECK(result.in1 == i1.end());
            CHECK(result.in2 == i2.end());
            CHECK(result.out == o.begin() + 7);
            CHECK(
                o
                == (RAH2_STD::vector<Coord>{
                    {18, 0}, {5, 0}, {4, 0}, {3, 0}, {3, 0}, {2, 0}, {1, 0}, {0, 0}, {0, 0}, {0, 0}}));
        }
        {
            // Intersection : Some elements are removed
            RAH2_STD::vector<Coord> in1{{5, 0}, {3, 0}, {3, 0}, {1, 0}}, in2{{5, 0}, {4, 0}, {3, 0}};
            auto i1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto i2 = make_test_view_adapter<CS, Tag, Sized>(in2);
            RAH2_STD::vector<Coord> o(10);
            auto const result = RAH2_NS::ranges::set_symmetric_difference(
                i1.begin(), i1.end(), i2.begin(), i2.end(), o.begin(), &comp_64, &Coord::x, &Coord::x);
            CHECK(result.in1 == i1.end());
            CHECK(result.in2 == i2.end());
            CHECK(result.out == o.begin() + 3);
            CHECK(
                o
                == (RAH2_STD::vector<Coord>{
                    {4, 0}, {3, 0}, {1, 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}}));
        }
        {
            // R1 is empty : Keep R2
            RAH2_STD::vector<Coord> in1{}, in2{{5, 0}, {3, 0}, {3, 0}, {1, 0}};
            auto i1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto i2 = make_test_view_adapter<CS, Tag, Sized>(in2);
            RAH2_STD::vector<Coord> o(10);
            auto const result = RAH2_NS::ranges::set_symmetric_difference(
                i1.begin(), i1.end(), i2.begin(), i2.end(), o.begin(), &comp_64, &Coord::x, &Coord::x);
            CHECK(result.in1 == i1.end());
            CHECK(result.in2 == i2.end());
            CHECK(result.out == o.begin() + 4);
            CHECK(
                o
                == (RAH2_STD::vector<Coord>{
                    {5, 0}, {3, 0}, {3, 0}, {1, 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}}));
        }
        {
            // R2 is empty : Keep R1
            RAH2_STD::vector<Coord> in1{{5, 0}, {3, 0}, {3, 0}, {1, 0}}, in2{};
            auto i1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto i2 = make_test_view_adapter<CS, Tag, Sized>(in2);
            RAH2_STD::vector<Coord> o(10);
            auto const result = RAH2_NS::ranges::set_symmetric_difference(
                i1.begin(), i1.end(), i2.begin(), i2.end(), o.begin(), &comp_64, &Coord::x, &Coord::x);
            CHECK(result.in1 == i1.end());
            CHECK(result.in2 == i2.end());
            CHECK(result.out == o.begin() + 4);
            CHECK(
                o
                == (RAH2_STD::vector<Coord>{
                    {5, 0}, {3, 0}, {3, 0}, {1, 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}}));
        }
        {
            // R1 and R2 are equal. All elements are removed.
            RAH2_STD::vector<Coord> in1{{5, 0}, {3, 0}, {3, 0}, {1, 0}},
                in2{{5, 0}, {3, 0}, {3, 0}, {1, 0}};
            auto i1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto i2 = make_test_view_adapter<CS, Tag, Sized>(in2);
            RAH2_STD::vector<Coord> o(10);
            auto const result = RAH2_NS::ranges::set_symmetric_difference(
                i1.begin(), i1.end(), i2.begin(), i2.end(), o.begin(), &comp_64, &Coord::x, &Coord::x);
            CHECK(result.in1 == i1.end());
            CHECK(result.in2 == i2.end());
            CHECK(result.out == o.begin());
            CHECK(o == (RAH2_STD::vector<Coord>(10)));
        }
        {
            // R1 and R2 are empty
            testSuite.test_case("empty");
            RAH2_STD::vector<Coord> in1, in2;
            auto i1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto i2 = make_test_view_adapter<CS, Tag, Sized>(in2);
            RAH2_STD::vector<Coord> o(10);
            auto const result = RAH2_NS::ranges::set_symmetric_difference(
                i1.begin(), i1.end(), i2.begin(), i2.end(), o.begin(), &comp_64, &Coord::x, &Coord::x);
            CHECK(result.in1 == i1.end());
            CHECK(result.in2 == i2.end());
            CHECK(result.out == o.begin());
            CHECK(o == (RAH2_STD::vector<Coord>(10)));
        }
        {
            // More elements in R2
            RAH2_STD::vector<Coord> in1{{5, 0}, {3, 0}, {3, 0}, {1, 0}},
                in2{{6, 0}, {5, 0}, {3, 0}, {3, 0}, {3, 0}, {1, 0}};
            auto i1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto i2 = make_test_view_adapter<CS, Tag, Sized>(in2);
            RAH2_STD::vector<Coord> o(10);
            auto const result = RAH2_NS::ranges::set_symmetric_difference(
                i1.begin(), i1.end(), i2.begin(), i2.end(), o.begin(), &comp_64, &Coord::x, &Coord::x);
            CHECK(result.in1 == i1.end());
            CHECK(result.in2 == i2.end());
            CHECK(result.out == o.begin() + 2);
            CHECK(
                o
                == (RAH2_STD::vector<Coord>{
                    {6, 0}, {3, 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}}));
        }
    }
    template <bool = true>
    void test_perf(char const* range_type)
    {
        {
            RAH2_STD::vector<Coord> in1;
            RAH2_STD::vector<Coord> in2;
            for (int i = 0; i < 100000 * RELEASE_MULTIPLIER; ++i)
            {
                if (i % 3 == 0)
                {
                    in1.push_back(Coord{i, 0});
                    in2.push_back(Coord{i, 0});
                }
                else if (i % 3 == 1)
                    in1.push_back(Coord{i, 0});
                else if (i % 3 == 2)
                    in2.push_back(Coord{i, 0});
            }

            auto r1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto r2 = make_test_view_adapter<CS, Tag, Sized>(in2);

            RAH2_STD::vector<Coord> out(in1.size());

            COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(
                CS == Common,
                "set_symmetric_difference_iter",
                range_type,
                [&]
                {
                    for (auto i = 0; i < 1; ++i)
                    {
                        auto result = STD::set_symmetric_difference(
                            fwd(r1.begin()), r1.end(), r2.begin(), r2.end(), out.begin());
                        DONT_OPTIM(result);
                    }
                });
        }

        {
            RAH2_STD::vector<Coord> in1;
            RAH2_STD::vector<Coord> in2;
            in1.push_back(Coord{100000 * RELEASE_MULTIPLIER, 0});
            for (int i = 100000 * RELEASE_MULTIPLIER; i > 0; --i)
            {
                if (i % 3 == 0)
                {
                    in1.push_back(Coord{i, 0});
                    in2.push_back(Coord{i, 0});
                }
                else if (i % 3 == 1)
                    in1.push_back(Coord{i, 0});
                else if (i % 3 == 2)
                    in2.push_back(Coord{i, 0});
            }

            auto r1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto r2 = make_test_view_adapter<CS, Tag, Sized>(in2);

            RAH2_STD::vector<Coord> out(in1.size());

            COMPARE_DURATION_TO_STD_RANGES(
                "set_symmetric_difference_range_proj",
                range_type,
                [&]
                {
                    for (auto i = 0; i < 1; ++i)
                    {
                        auto result = STD::set_symmetric_difference(
                            r1, r2, out.begin(), comp_64, &Coord::x, &Coord::x);
                        DONT_OPTIM(result);
                    }
                });
        }
    }
    static constexpr bool do_test = RAH2_NS::derived_from<Tag, RAH2_NS::input_iterator_tag>;
};
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

    foreach_range_combination<test_algo<test_set_symmetric_difference_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_set_union_
{
    static bool comp_64(intptr_t a, intptr_t b)
    {
        return a > b;
    }

    template <bool = true>
    void test()
    {
        testSuite.test_case("noproj");
        testSuite.test_case("iter");
        {
            // No common elements : Keep all
            RAH2_STD::vector<int> in1{1, 3, 3, 5}, in2{2, 4, 18};
            auto i1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto i2 = make_test_view_adapter<CS, Tag, Sized>(in2);
            RAH2_STD::vector<int> o(10);
            auto const result =
                RAH2_NS::ranges::set_union(i1.begin(), i1.end(), i2.begin(), i2.end(), o.begin());
            CHECK(result.in1 == i1.end());
            CHECK(result.in2 == i2.end());
            CHECK(result.out == o.begin() + 7);
            CHECK(o == (RAH2_STD::vector<int>{1, 2, 3, 3, 4, 5, 18, 0, 0, 0}));
        }
        {
            // Intersection : Keep all but only 2 "3", and one "5"
            RAH2_STD::vector<int> in1{1, 3, 3, 5}, in2{3, 4, 5};
            auto i1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto i2 = make_test_view_adapter<CS, Tag, Sized>(in2);
            RAH2_STD::vector<int> o(10);
            auto const result =
                RAH2_NS::ranges::set_union(i1.begin(), i1.end(), i2.begin(), i2.end(), o.begin());
            CHECK(result.in1 == i1.end());
            CHECK(result.in2 == i2.end());
            CHECK(result.out == o.begin() + 5);
            CHECK(o == (RAH2_STD::vector<int>{1, 3, 3, 4, 5, 0, 0, 0, 0, 0}));
        }
        {
            // R1 is empty : Keep R2
            RAH2_STD::vector<int> in1{}, in2{1, 3, 3, 5};
            auto i1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto i2 = make_test_view_adapter<CS, Tag, Sized>(in2);
            RAH2_STD::vector<int> o(10);
            auto const result =
                RAH2_NS::ranges::set_union(i1.begin(), i1.end(), i2.begin(), i2.end(), o.begin());
            CHECK(result.in1 == i1.end());
            CHECK(result.in2 == i2.end());
            CHECK(result.out == o.begin() + 4);
            CHECK(o == (RAH2_STD::vector<int>{1, 3, 3, 5, 0, 0, 0, 0, 0, 0}));
        }
        {
            // R2 is empty : Keep R1
            RAH2_STD::vector<int> in1{1, 3, 3, 5}, in2{};
            auto i1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto i2 = make_test_view_adapter<CS, Tag, Sized>(in2);
            RAH2_STD::vector<int> o(10);
            auto const result =
                RAH2_NS::ranges::set_union(i1.begin(), i1.end(), i2.begin(), i2.end(), o.begin());
            CHECK(result.in1 == i1.end());
            CHECK(result.in2 == i2.end());
            CHECK(result.out == o.begin() + 4);
            CHECK(o == (RAH2_STD::vector<int>{1, 3, 3, 5, 0, 0, 0, 0, 0, 0}));
        }
        {
            // R1 and R2 are equal. Keep R1
            RAH2_STD::vector<int> in1{1, 3, 3, 5}, in2{1, 3, 3, 5};
            auto i1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto i2 = make_test_view_adapter<CS, Tag, Sized>(in2);
            RAH2_STD::vector<int> o(10);
            auto const result =
                RAH2_NS::ranges::set_union(i1.begin(), i1.end(), i2.begin(), i2.end(), o.begin());
            CHECK(result.in1 == i1.end());
            CHECK(result.in2 == i2.end());
            CHECK(result.out == o.begin() + 4);
            CHECK(o == (RAH2_STD::vector<int>{1, 3, 3, 5, 0, 0, 0, 0, 0, 0}));
        }
        {
            // R1 and R2 are empty
            testSuite.test_case("empty");
            RAH2_STD::vector<int> in1, in2;
            auto i1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto i2 = make_test_view_adapter<CS, Tag, Sized>(in2);
            RAH2_STD::vector<int> o(10);
            auto const result =
                RAH2_NS::ranges::set_union(i1.begin(), i1.end(), i2.begin(), i2.end(), o.begin());
            CHECK(result.in1 == i1.end());
            CHECK(result.in2 == i2.end());
            CHECK(result.out == o.begin());
            CHECK(o == (RAH2_STD::vector<int>(10)));
        }
        {
            // More elements in R2. Keep R2
            RAH2_STD::vector<int> in1{1, 3, 3, 5}, in2{1, 3, 3, 3, 5, 6};
            auto i1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto i2 = make_test_view_adapter<CS, Tag, Sized>(in2);
            RAH2_STD::vector<int> o(10);
            auto const result =
                RAH2_NS::ranges::set_union(i1.begin(), i1.end(), i2.begin(), i2.end(), o.begin());
            CHECK(result.in1 == i1.end());
            CHECK(result.in2 == i2.end());
            CHECK(result.out == o.begin() + 6);
            CHECK(o == (RAH2_STD::vector<int>{1, 3, 3, 3, 5, 6, 0, 0, 0, 0}));
        }

        testSuite.test_case("range");
        testSuite.test_case("proj");
        {
            // No common elements : Keep all
            RAH2_STD::vector<Coord> in1{{5, 0}, {3, 0}, {3, 0}, {1, 0}}, in2{{18, 0}, {4, 0}, {2, 0}};
            auto i1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto i2 = make_test_view_adapter<CS, Tag, Sized>(in2);
            RAH2_STD::vector<Coord> o(10);
            auto const result = RAH2_NS::ranges::set_union(
                i1.begin(), i1.end(), i2.begin(), i2.end(), o.begin(), &comp_64, &Coord::x, &Coord::x);
            CHECK(result.in1 == i1.end());
            CHECK(result.in2 == i2.end());
            CHECK(result.out == o.begin() + 7);
            CHECK(
                o
                == (RAH2_STD::vector<Coord>{
                    {18, 0}, {5, 0}, {4, 0}, {3, 0}, {3, 0}, {2, 0}, {1, 0}, {0, 0}, {0, 0}, {0, 0}}));
        }
        {
            // Intersection : Keep all but onlt 2 "3", and one "5"
            RAH2_STD::vector<Coord> in1{{5, 0}, {3, 0}, {3, 0}, {1, 0}}, in2{{5, 0}, {4, 0}, {3, 0}};
            auto i1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto i2 = make_test_view_adapter<CS, Tag, Sized>(in2);
            RAH2_STD::vector<Coord> o(10);
            auto const result = RAH2_NS::ranges::set_union(
                i1.begin(), i1.end(), i2.begin(), i2.end(), o.begin(), &comp_64, &Coord::x, &Coord::x);
            CHECK(result.in1 == i1.end());
            CHECK(result.in2 == i2.end());
            CHECK(result.out == o.begin() + 5);
            CHECK(
                o
                == (RAH2_STD::vector<Coord>{
                    {5, 0}, {4, 0}, {3, 0}, {3, 0}, {1, 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}}));
        }
        {
            // R1 is empty : Keep R2
            RAH2_STD::vector<Coord> in1{}, in2{{5, 0}, {3, 0}, {3, 0}, {1, 0}};
            auto i1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto i2 = make_test_view_adapter<CS, Tag, Sized>(in2);
            RAH2_STD::vector<Coord> o(10);
            auto const result = RAH2_NS::ranges::set_union(
                i1.begin(), i1.end(), i2.begin(), i2.end(), o.begin(), &comp_64, &Coord::x, &Coord::x);
            CHECK(result.in1 == i1.end());
            CHECK(result.in2 == i2.end());
            CHECK(result.out == o.begin() + 4);
            CHECK(
                o
                == (RAH2_STD::vector<Coord>{
                    {5, 0}, {3, 0}, {3, 0}, {1, 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}}));
        }
        {
            // R2 is empty : Keep R1
            RAH2_STD::vector<Coord> in1{{5, 0}, {3, 0}, {3, 0}, {1, 0}}, in2{};
            auto i1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto i2 = make_test_view_adapter<CS, Tag, Sized>(in2);
            RAH2_STD::vector<Coord> o(10);
            auto const result = RAH2_NS::ranges::set_union(
                i1.begin(), i1.end(), i2.begin(), i2.end(), o.begin(), &comp_64, &Coord::x, &Coord::x);
            CHECK(result.in1 == i1.end());
            CHECK(result.in2 == i2.end());
            CHECK(result.out == o.begin() + 4);
            CHECK(
                o
                == (RAH2_STD::vector<Coord>{
                    {5, 0}, {3, 0}, {3, 0}, {1, 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}}));
        }
        {
            // R1 and R2 are equal. Keep R1
            RAH2_STD::vector<Coord> in1{{5, 0}, {3, 0}, {3, 0}, {1, 0}},
                in2{{5, 0}, {3, 0}, {3, 0}, {1, 0}};
            auto i1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto i2 = make_test_view_adapter<CS, Tag, Sized>(in2);
            RAH2_STD::vector<Coord> o(10);
            auto const result = RAH2_NS::ranges::set_union(
                i1.begin(), i1.end(), i2.begin(), i2.end(), o.begin(), &comp_64, &Coord::x, &Coord::x);
            CHECK(result.in1 == i1.end());
            CHECK(result.in2 == i2.end());
            CHECK(result.out == o.begin() + 4);
            CHECK(
                o
                == (RAH2_STD::vector<Coord>{
                    {5, 0}, {3, 0}, {3, 0}, {1, 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}}));
        }
        {
            // R1 and R2 are empty
            testSuite.test_case("empty");
            RAH2_STD::vector<Coord> in1, in2;
            auto i1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto i2 = make_test_view_adapter<CS, Tag, Sized>(in2);
            RAH2_STD::vector<Coord> o(10);
            auto const result = RAH2_NS::ranges::set_union(
                i1.begin(), i1.end(), i2.begin(), i2.end(), o.begin(), &comp_64, &Coord::x, &Coord::x);
            CHECK(result.in1 == i1.end());
            CHECK(result.in2 == i2.end());
            CHECK(result.out == o.begin());
            CHECK(o == (RAH2_STD::vector<Coord>(10)));
        }
        {
            // More elements in R2. Keep R2
            RAH2_STD::vector<Coord> in1{{5, 0}, {3, 0}, {3, 0}, {1, 0}},
                in2{{6, 0}, {5, 0}, {3, 0}, {3, 0}, {3, 0}, {1, 0}};
            auto i1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto i2 = make_test_view_adapter<CS, Tag, Sized>(in2);
            RAH2_STD::vector<Coord> o(10);
            auto const result = RAH2_NS::ranges::set_union(
                i1.begin(), i1.end(), i2.begin(), i2.end(), o.begin(), &comp_64, &Coord::x, &Coord::x);
            CHECK(result.in1 == i1.end());
            CHECK(result.in2 == i2.end());
            CHECK(result.out == o.begin() + 6);
            CHECK(
                o
                == (RAH2_STD::vector<Coord>{
                    {6, 0}, {5, 0}, {3, 0}, {3, 0}, {3, 0}, {1, 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}}));
        }
    }
    template <bool = true>
    void test_perf(char const* range_type)
    {
        {
            RAH2_STD::vector<Coord> in1;
            RAH2_STD::vector<Coord> in2;
            for (int i = 0; i < 100000 * RELEASE_MULTIPLIER; ++i)
            {
                if (i % 3 == 0)
                {
                    in1.push_back(Coord{i, 0});
                    in2.push_back(Coord{i, 0});
                }
                else if (i % 3 == 1)
                    in1.push_back(Coord{i, 0});
                else if (i % 3 == 2)
                    in2.push_back(Coord{i, 0});
            }

            auto r1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto r2 = make_test_view_adapter<CS, Tag, Sized>(in2);

            RAH2_STD::vector<Coord> out(in1.size() + in2.size());

            COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(
                CS == Common,
                "set_union_iter",
                range_type,
                [&]
                {
                    for (auto i = 0; i < 1; ++i)
                    {
                        auto result = STD::set_union(
                            fwd(r1.begin()), r1.end(), r2.begin(), r2.end(), out.begin());
                        DONT_OPTIM(result);
                    }
                });
        }

        {
            RAH2_STD::vector<Coord> in1;
            RAH2_STD::vector<Coord> in2;
            in1.push_back(Coord{100000 * RELEASE_MULTIPLIER, 0});
            for (int i = 100000 * RELEASE_MULTIPLIER; i > 0; --i)
            {
                if (i % 3 == 0)
                {
                    in1.push_back(Coord{i, 0});
                    in2.push_back(Coord{i, 0});
                }
                else if (i % 3 == 1)
                    in1.push_back(Coord{i, 0});
                else if (i % 3 == 2)
                    in2.push_back(Coord{i, 0});
            }

            auto r1 = make_test_view_adapter<CS, Tag, Sized>(in1);
            auto r2 = make_test_view_adapter<CS, Tag, Sized>(in2);

            RAH2_STD::vector<Coord> out(in1.size() + in2.size());

            COMPARE_DURATION_TO_STD_RANGES(
                "set_union_range_proj",
                range_type,
                [&]
                {
                    for (auto i = 0; i < 1; ++i)
                    {
                        auto result =
                            STD::set_union(r1, r2, out.begin(), comp_64, &Coord::x, &Coord::x);
                        DONT_OPTIM(result);
                    }
                });
        }
    }
    static constexpr bool do_test = RAH2_NS::derived_from<Tag, RAH2_NS::input_iterator_tag>;
};
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

    foreach_range_combination<test_algo<test_set_union_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_is_heap_
{
    static bool descending(int a, int b)
    {
        return b < a;
    }

    static bool descending_64(intptr_t a, intptr_t b)
    {
        return b < a;
    }

    static bool descending_coord(Coord a, Coord b)
    {
        return b.x < a.x;
    }

    template <bool = true>
    void test()
    {
        testSuite.test_case("iter");
        {
            RAH2_STD::vector<int> max_heap_ = {9, 5, 6, 4, 3, 2, 1};
            auto max_heap = make_test_view_adapter<CS, Tag, Sized>(max_heap_);
            CHECK(RAH2_NS::ranges::is_heap(max_heap.begin(), max_heap.end()));
        }
        {
            RAH2_STD::vector<int> not_heap_ = {3, 9, 6, 4, 5, 2, 1};
            auto not_heap = make_test_view_adapter<CS, Tag, Sized>(not_heap_);
            CHECK(!RAH2_NS::ranges::is_heap(not_heap.begin(), not_heap.end()));
        }
        {
            RAH2_STD::vector<int> single_element_ = {10};
            auto single_element = make_test_view_adapter<CS, Tag, Sized>(single_element_);
            CHECK(RAH2_NS::ranges::is_heap(single_element.begin(), single_element.end()));
        }
        {
            RAH2_STD::vector<int> empty_;
            auto empty = make_test_view_adapter<CS, Tag, Sized>(empty_);
            CHECK(RAH2_NS::ranges::is_heap(empty.begin(), empty.end()));
        }
        {
            RAH2_STD::vector<int> equal_elements_ = {5, 5, 5, 5, 5};
            auto equal_elements = make_test_view_adapter<CS, Tag, Sized>(equal_elements_);
            CHECK(RAH2_NS::ranges::is_heap(equal_elements.begin(), equal_elements.end()));
        }
        {
            RAH2_STD::vector<int> partial_heap_ = {9, 5, 6, 4, 3, 7, 1};
            auto partial_heap = make_test_view_adapter<CS, Tag, Sized>(partial_heap_);
            CHECK(!RAH2_NS::ranges::is_heap(partial_heap.begin(), partial_heap.end()));
        }

        testSuite.test_case("range");
        {
            RAH2_STD::vector<Coord> max_heap_ = {
                {1, 0}, {2, 0}, {3, 0}, {4, 0}, {6, 0}, {5, 0}, {9, 0}};
            auto max_heap = make_test_view_adapter<CS, Tag, Sized>(max_heap_);
            CHECK(RAH2_NS::ranges::is_heap(max_heap, descending_64, &Coord::x));
        }
        {
            RAH2_STD::vector<Coord> not_heap_ = {
                {1, 0}, {2, 0}, {5, 0}, {4, 0}, {6, 0}, {9, 0}, {3, 0}};
            auto not_heap = make_test_view_adapter<CS, Tag, Sized>(not_heap_);
            CHECK(!RAH2_NS::ranges::is_heap(not_heap, descending_64, &Coord::x));
        }
        {
            RAH2_STD::vector<Coord> single_element_ = {{10, 0}};
            auto single_element = make_test_view_adapter<CS, Tag, Sized>(single_element_);
            CHECK(RAH2_NS::ranges::is_heap(single_element, descending_64, &Coord::x));
        }
        {
            RAH2_STD::vector<Coord> empty_;
            auto empty = make_test_view_adapter<CS, Tag, Sized>(empty_);
            CHECK(RAH2_NS::ranges::is_heap(empty, descending_64, &Coord::x));
        }
        {
            RAH2_STD::vector<Coord> equal_elements_ = {{5, 0}, {5, 0}, {5, 0}, {5, 0}, {5, 0}};
            auto equal_elements = make_test_view_adapter<CS, Tag, Sized>(equal_elements_);
            CHECK(RAH2_NS::ranges::is_heap(equal_elements, descending_64, &Coord::x));
        }
        {
            RAH2_STD::vector<Coord> partial_heap_ = {
                {1, 0}, {7, 0}, {3, 0}, {4, 0}, {6, 0}, {5, 0}, {9, 0}};
            auto partial_heap = make_test_view_adapter<CS, Tag, Sized>(partial_heap_);
            CHECK(!RAH2_NS::ranges::is_heap(partial_heap, descending_64, &Coord::x));
        }
    }
    template <bool = true>
    void test_perf(char const* range_type)
    {
        RAH2_STD::vector<int> perf_iter_(1000000 * RELEASE_MULTIPLIER, 1);
        auto perf_iter = make_test_view_adapter<CS, Tag, Sized>(perf_iter_);

        RAH2_STD::vector<Coord> perf_(1000000 * RELEASE_MULTIPLIER, {1, 0});
        auto perf = make_test_view_adapter<CS, Tag, Sized>(perf_);

        COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(
            CS == Common,
            "is_heap",
            range_type,
            [&]
            {
                auto result = STD::is_heap(fwd(perf_iter.begin()), perf_iter.end());
                CHECK(result);
            });
        COMPARE_DURATION_TO_STD_RANGES(
            "is_heap_proj",
            range_type,
            [&]
            {
                auto result = STD::is_heap(perf, descending_64, &Coord::x);
                CHECK(result);
            });
    }
    static constexpr bool do_test = RAH2_NS::derived_from<Tag, RAH2_NS::random_access_iterator_tag>;
};
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

    foreach_range_combination<test_algo<test_is_heap_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_is_heap_until_
{
    static bool descending(int a, int b)
    {
        return b < a;
    }

    static bool descending_64(intptr_t a, intptr_t b)
    {
        return b < a;
    }

    static bool descending_coord(Coord a, Coord b)
    {
        return b.x < a.x;
    }

    template <bool = true>
    void test()
    {
        testSuite.test_case("iter");
        {
            RAH2_STD::vector<int> max_heap_ = {9, 5, 6, 4, 3, 2, 1};
            auto max_heap = make_test_view_adapter<CS, Tag, Sized>(max_heap_);
            CHECK(
                RAH2_NS::ranges::is_heap_until(max_heap.begin(), max_heap.end())
                == RAH2_NS::ranges::next(max_heap.begin(), 7));
        }
        {
            RAH2_STD::vector<int> not_heap_ = {3, 9, 6, 4, 5, 2, 1};
            auto not_heap = make_test_view_adapter<CS, Tag, Sized>(not_heap_);
            CHECK(
                RAH2_NS::ranges::is_heap_until(not_heap.begin(), not_heap.end())
                == RAH2_NS::ranges::next(not_heap.begin(), 1));
        }
        {
            RAH2_STD::vector<int> single_element_ = {10};
            auto single_element = make_test_view_adapter<CS, Tag, Sized>(single_element_);
            CHECK(
                RAH2_NS::ranges::is_heap_until(single_element.begin(), single_element.end())
                == single_element.end());
        }
        {
            RAH2_STD::vector<int> empty_;
            auto empty = make_test_view_adapter<CS, Tag, Sized>(empty_);
            CHECK(RAH2_NS::ranges::is_heap_until(empty.begin(), empty.end()) == empty.end());
        }
        {
            RAH2_STD::vector<int> equal_elements_ = {5, 5, 5, 5, 5};
            auto equal_elements = make_test_view_adapter<CS, Tag, Sized>(equal_elements_);
            CHECK(
                RAH2_NS::ranges::is_heap_until(equal_elements.begin(), equal_elements.end())
                == equal_elements.end());
        }
        {
            RAH2_STD::vector<int> partial_heap_ = {9, 5, 6, 4, 3, 7, 1};
            auto partial_heap = make_test_view_adapter<CS, Tag, Sized>(partial_heap_);
            CHECK(
                RAH2_NS::ranges::is_heap_until(partial_heap.begin(), partial_heap.end())
                == RAH2_NS::ranges::next(partial_heap.begin(), 5));
        }

        testSuite.test_case("range");
        {
            RAH2_STD::vector<Coord> max_heap_ = {
                {1, 0}, {2, 0}, {3, 0}, {4, 0}, {6, 0}, {5, 0}, {9, 0}};
            auto max_heap = make_test_view_adapter<CS, Tag, Sized>(max_heap_);
            CHECK(RAH2_NS::ranges::is_heap_until(max_heap, descending_64, &Coord::x) == max_heap.end());
        }
        {
            RAH2_STD::vector<Coord> not_heap_ = {
                {1, 0}, {2, 0}, {5, 0}, {4, 0}, {6, 0}, {9, 0}, {3, 0}};
            auto not_heap = make_test_view_adapter<CS, Tag, Sized>(not_heap_);
            CHECK(
                RAH2_NS::ranges::is_heap_until(not_heap, descending_64, &Coord::x)
                == RAH2_NS::ranges::next(not_heap.begin(), 6));
        }
        {
            RAH2_STD::vector<Coord> single_element_ = {{10, 0}};
            auto single_element = make_test_view_adapter<CS, Tag, Sized>(single_element_);
            CHECK(
                RAH2_NS::ranges::is_heap_until(single_element, descending_64, &Coord::x)
                == single_element.end());
        }
        {
            RAH2_STD::vector<Coord> empty_;
            auto empty = make_test_view_adapter<CS, Tag, Sized>(empty_);
            CHECK(RAH2_NS::ranges::is_heap_until(empty, descending_64, &Coord::x) == empty.end());
        }
        {
            RAH2_STD::vector<Coord> equal_elements_ = {{5, 0}, {5, 0}, {5, 0}, {5, 0}, {5, 0}};
            auto equal_elements = make_test_view_adapter<CS, Tag, Sized>(equal_elements_);
            CHECK(
                RAH2_NS::ranges::is_heap_until(equal_elements, descending_64, &Coord::x)
                == equal_elements.end());
        }
        {
            RAH2_STD::vector<Coord> partial_heap_ = {
                {1, 0}, {7, 0}, {3, 0}, {4, 0}, {6, 0}, {5, 0}, {9, 0}};
            auto partial_heap = make_test_view_adapter<CS, Tag, Sized>(partial_heap_);
            CHECK(
                RAH2_NS::ranges::is_heap_until(partial_heap, descending_64, &Coord::x)
                == RAH2_NS::ranges::next(partial_heap.begin(), 3));
        }
    }
    template <bool = true>
    void test_perf(char const* range_type)
    {
        RAH2_STD::vector<int> perf_iter_(1000000 * RELEASE_MULTIPLIER, 1);
        auto perf_iter = make_test_view_adapter<CS, Tag, Sized>(perf_iter_);

        RAH2_STD::vector<Coord> perf_(1000000 * RELEASE_MULTIPLIER, {1, 0});
        auto perf = make_test_view_adapter<CS, Tag, Sized>(perf_);

        COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(
            CS == Common,
            "is_heap_until",
            range_type,
            [&]
            {
                auto result = STD::is_heap_until(fwd(perf_iter.begin()), perf_iter.end());
                CHECK(result == perf_iter.end());
            });
        COMPARE_DURATION_TO_STD_RANGES(
            "is_heap_until_proj",
            range_type,
            [&]
            {
                auto result = STD::is_heap_until(perf, descending_64, &Coord::x);
                CHECK(result == perf.end());
            });
    }
    static constexpr bool do_test = RAH2_NS::derived_from<Tag, RAH2_NS::random_access_iterator_tag>;
};
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

    foreach_range_combination<test_algo<test_is_heap_until_>>();
}

static bool is_max_heap(const RAH2_STD::vector<int>& vec)
{
    auto n = vec.size();
    for (size_t i = 0; i < n / 2; ++i)
    {
        auto left = 2 * i + 1;
        auto right = 2 * i + 2;

        if (left < n && vec[i] < vec[left])
            return false;
        if (right < n && vec[i] < vec[right])
            return false;
    }
    return true;
}

static bool is_max_heap(const RAH2_STD::vector<Coord>& vec)
{
    auto n = vec.size();
    for (size_t i = 0; i < n / 2; ++i)
    {
        auto left = 2 * i + 1;
        auto right = 2 * i + 2;

        if (left < n && vec[i].x > vec[left].x)
            return false;
        if (right < n && vec[i].x > vec[right].x)
            return false;
    }
    return true;
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_make_heap_
{
    static bool descending(int a, int b)
    {
        return b < a;
    }

    static bool descending_64(intptr_t a, intptr_t b)
    {
        return b < a;
    }

    static bool descending_coord(Coord a, Coord b)
    {
        return b.x < a.x;
    }

    template <bool = true>
    void test()
    {
        testSuite.test_case("iter");
        {
            RAH2_STD::vector<int> max_heap_ = {9, 5, 6, 4, 3, 2, 1};
            auto max_heap = make_test_view_adapter<CS, Tag, Sized>(max_heap_);
            CHECK(RAH2_NS::ranges::make_heap(max_heap.begin(), max_heap.end()) == max_heap.end());
            CHECK(is_max_heap(max_heap_));
        }
        {
            RAH2_STD::vector<int> not_heap_ = {3, 9, 6, 4, 5, 2, 1};
            auto not_heap = make_test_view_adapter<CS, Tag, Sized>(not_heap_);
            CHECK(RAH2_NS::ranges::make_heap(not_heap.begin(), not_heap.end()) == not_heap.end());
            CHECK(is_max_heap(not_heap_));
        }
        {
            RAH2_STD::vector<int> single_element_ = {10};
            auto single_element = make_test_view_adapter<CS, Tag, Sized>(single_element_);
            CHECK(
                RAH2_NS::ranges::make_heap(single_element.begin(), single_element.end())
                == single_element.end());
            CHECK(is_max_heap(single_element_));
        }
        {
            RAH2_STD::vector<int> empty_;
            auto empty = make_test_view_adapter<CS, Tag, Sized>(empty_);
            CHECK(RAH2_NS::ranges::make_heap(empty.begin(), empty.end()) == empty.end());
            CHECK(is_max_heap(empty_));
        }
        {
            RAH2_STD::vector<int> equal_elements_ = {5, 5, 5, 5, 5};
            auto equal_elements = make_test_view_adapter<CS, Tag, Sized>(equal_elements_);
            CHECK(
                RAH2_NS::ranges::make_heap(equal_elements.begin(), equal_elements.end())
                == equal_elements.end());
            CHECK(is_max_heap(equal_elements_));
        }
        {
            RAH2_STD::vector<int> partial_heap_ = {9, 5, 6, 4, 3, 7, 1};
            auto partial_heap = make_test_view_adapter<CS, Tag, Sized>(partial_heap_);
            CHECK(
                RAH2_NS::ranges::make_heap(partial_heap.begin(), partial_heap.end())
                == partial_heap.end());
            CHECK(is_max_heap(partial_heap_));
        }

        testSuite.test_case("range");
        {
            RAH2_STD::vector<Coord> max_heap_ = {
                {1, 0}, {2, 0}, {3, 0}, {4, 0}, {6, 0}, {5, 0}, {9, 0}};
            auto max_heap = make_test_view_adapter<CS, Tag, Sized>(max_heap_);
            CHECK(RAH2_NS::ranges::make_heap(max_heap, descending_64, &Coord::x) == max_heap.end());
            CHECK(is_max_heap(max_heap_));
        }
        {
            RAH2_STD::vector<Coord> not_heap_ = {
                {1, 0}, {2, 0}, {5, 0}, {4, 0}, {6, 0}, {9, 0}, {3, 0}};
            auto not_heap = make_test_view_adapter<CS, Tag, Sized>(not_heap_);
            CHECK(RAH2_NS::ranges::make_heap(not_heap, descending_64, &Coord::x) == not_heap.end());
            CHECK(is_max_heap(not_heap_));
        }
        {
            RAH2_STD::vector<Coord> single_element_ = {{10, 0}};
            auto single_element = make_test_view_adapter<CS, Tag, Sized>(single_element_);
            CHECK(
                RAH2_NS::ranges::make_heap(single_element, descending_64, &Coord::x)
                == single_element.end());
            CHECK(is_max_heap(single_element_));
        }
        {
            RAH2_STD::vector<Coord> empty_;
            auto empty = make_test_view_adapter<CS, Tag, Sized>(empty_);
            CHECK(RAH2_NS::ranges::make_heap(empty, descending_64, &Coord::x) == empty.end());
            CHECK(is_max_heap(empty_));
        }
        {
            RAH2_STD::vector<Coord> equal_elements_ = {{5, 0}, {5, 0}, {5, 0}, {5, 0}, {5, 0}};
            auto equal_elements = make_test_view_adapter<CS, Tag, Sized>(equal_elements_);
            CHECK(
                RAH2_NS::ranges::make_heap(equal_elements, descending_64, &Coord::x)
                == equal_elements.end());
            CHECK(is_max_heap(equal_elements_));
        }
        {
            RAH2_STD::vector<Coord> partial_heap_ = {
                {1, 0}, {7, 0}, {3, 0}, {4, 0}, {6, 0}, {5, 0}, {9, 0}};
            auto partial_heap = make_test_view_adapter<CS, Tag, Sized>(partial_heap_);
            CHECK(
                RAH2_NS::ranges::make_heap(partial_heap, descending_64, &Coord::x)
                == partial_heap.end());
            CHECK(is_max_heap(partial_heap_));
        }
    }
    template <bool = true>
    void test_perf(char const* range_type)
    {
        RAH2_STD::vector<int> perf_iter_(1000000 * RELEASE_MULTIPLIER, 1);
        perf_iter_.push_back(2);
        auto perf_iter = make_test_view_adapter<CS, Tag, Sized>(perf_iter_);

        RAH2_STD::vector<Coord> perf_(1000000 * RELEASE_MULTIPLIER, {2, 0});
        perf_.push_back({1, 0});
        auto perf = make_test_view_adapter<CS, Tag, Sized>(perf_);

        COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(
            CS == Common,
            "make_heap",
            range_type,
            [&]
            {
                STD::make_heap(fwd(perf_iter.begin()), perf_iter.end());
                CHECK(*perf_iter.begin() == 2);
            });
        COMPARE_DURATION_TO_STD_RANGES(
            "make_heap_proj",
            range_type,
            [&]
            {
                STD::make_heap(perf, descending_64, &Coord::x);
                CHECK(perf.begin()->x == 1);
            });
    }
    static constexpr bool do_test = RAH2_NS::derived_from<Tag, RAH2_NS::random_access_iterator_tag>;
};
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

    foreach_range_combination<test_algo<test_make_heap_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_push_heap_
{
    static bool descending(int a, int b)
    {
        return b < a;
    }

    static bool descending_64(intptr_t a, intptr_t b)
    {
        return b < a;
    }

    static bool descending_coord(Coord a, Coord b)
    {
        return b.x < a.x;
    }

    template <bool = true>
    void test()
    {
        // Empty,
        // single => Greater, lower, equal
        // many => Greater, lower, inside not equal, inside equal
        // Many equal => Greater, lower, equal

        testSuite.test_case("iter");
        auto test = [](RAH2_STD::vector<int> range, int value)
        {
            CHECK(is_max_heap(range));
            range.push_back(value);
            auto view = make_test_view_adapter<CS, Tag, Sized>(range);
            auto last = RAH2_NS::ranges::push_heap(view.begin(), view.end());
            CHECK(last == view.end());
            CHECK(is_max_heap(range));
        };
        test({}, 5);
        test({10}, 2);
        test({10}, 10);
        test({10}, 45);
        test({9, 5, 6, 4, 3, 2, 1}, -2);
        test({9, 5, 6, 4, 3, 2, 1}, 4);
        test({9, 5, 6, 3, 2, 1}, 4);
        test({9, 5, 6, 4, 4, 3, 2, 1}, 4);
        test({5, 5, 5, 5, 5}, 2);
        test({5, 5, 5, 5, 5}, 5);
        test({5, 5, 5, 5, 5}, 15);

        testSuite.test_case("range");
        auto test_proj = [](RAH2_STD::vector<Coord> range, Coord value)
        {
            CHECK(is_max_heap(range));
            range.push_back(value);
            auto view = make_test_view_adapter<CS, Tag, Sized>(range);
            auto last = RAH2_NS::ranges::push_heap(view, &descending_64, &Coord::x);
            CHECK(last == view.end());
            CHECK(is_max_heap(range));
        };
        test_proj({}, {5, 0});
        test_proj({{10, 0}}, {2, 0});
        test_proj({{10, 0}}, {10, 0});
        test_proj({{10, 0}}, {45, 0});
        test_proj({{1, 0}, {2, 0}, {3, 0}, {4, 0}, {6, 0}, {5, 0}, {9, 0}}, {-2, 0});
        test_proj({{1, 0}, {2, 0}, {3, 0}, {4, 0}, {6, 0}, {5, 0}, {9, 0}}, {72, 0});
        test_proj({{1, 0}, {2, 0}, {3, 0}, {4, 0}, {6, 0}, {5, 0}, {9, 0}}, {4, 0});
        test_proj({{1, 0}, {2, 0}, {3, 0}, {4, 0}, {4, 0}, {6, 0}, {5, 0}, {9, 0}}, {4, 0});
        test_proj({{1, 0}, {2, 0}, {3, 0}, {6, 0}, {5, 0}, {9, 0}}, {4, 0});
        test_proj({{5, 0}, {5, 0}, {5, 0}, {5, 0}, {5, 0}}, {2, 0});
        test_proj({{5, 0}, {5, 0}, {5, 0}, {5, 0}, {5, 0}}, {5, 0});
        test_proj({{5, 0}, {5, 0}, {5, 0}, {5, 0}, {5, 0}}, {15, 0});
    }
    template <bool = true>
    void test_perf(char const* range_type)
    {
        RAH2_STD::vector<int> perf_iter_(1000000 * RELEASE_MULTIPLIER, 2);
        perf_iter_.reserve(perf_iter_.size() * 2);
        auto perf_iter = make_test_view_adapter<CS, Tag, Sized>(perf_iter_);

        RAH2_STD::vector<Coord> perf_(1000000 * RELEASE_MULTIPLIER, {2, 0});
        perf_.reserve(perf_.size() * 2);
        auto perf = make_test_view_adapter<CS, Tag, Sized>(perf_);
        static constexpr auto UnsizedMult = Sized ? 1000 : 1;

        COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(
            CS == Common,
            "push_heap",
            range_type,
            [&]
            {
                for (int i = 0; i < 1 * UnsizedMult; ++i)
                {
                    perf_iter_.push_back(3);
                    STD::push_heap(fwd(perf_iter.begin()), perf_iter.end());
                    CHECK(*perf_iter.begin() == 3);
                }
            });
        COMPARE_DURATION_TO_STD_RANGES(
            "push_heap_proj",
            range_type,
            [&]
            {
                for (int i = 0; i < 1 * UnsizedMult; ++i)
                {
                    perf_.push_back(Coord{1, 0});
                    STD::push_heap(perf, descending_64, &Coord::x);
                    CHECK(perf.begin()->x == 1);
                }
            });
    }
    static constexpr bool do_test = RAH2_NS::derived_from<Tag, RAH2_NS::random_access_iterator_tag>;
};
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

    foreach_range_combination<test_algo<test_push_heap_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_pop_heap_
{
    static bool descending(int a, int b)
    {
        return b < a;
    }

    static bool descending_64(intptr_t a, intptr_t b)
    {
        return b < a;
    }

    static bool descending_coord(Coord a, Coord b)
    {
        return b.x < a.x;
    }

    template <bool = true>
    void test()
    {
        // Empty,
        // single => Greater, lower, equal
        // many => Greater, lower, inside not equal, inside equal
        // Many equal => Greater, lower, equal

        testSuite.test_case("iter");
        auto test = [](RAH2_STD::vector<int> range, int expected)
        {
            CHECK(is_max_heap(range));
            // range.push_back(value);
            auto view = make_test_view_adapter<CS, Tag, Sized>(range);
            auto last = RAH2_NS::ranges::pop_heap(view.begin(), view.end());
            CHECK_EQUAL(last, view.end());
            CHECK(range.back() == expected);
            range.pop_back();
            CHECK(is_max_heap(range));
        };
        // test({}, 5);
        test({10}, 10);
        test({9, 5, 6, 4, 3, 2, 1}, 9);
        test({9, 9, 5, 6, 4, 3, 2, 1}, 9);
        test({5, 5, 5, 5, 5}, 5);

        testSuite.test_case("range");
        auto test_proj = [](RAH2_STD::vector<Coord> range, Coord expected)
        {
            CHECK(is_max_heap(range));
            // range.push_back(value);
            auto view = make_test_view_adapter<CS, Tag, Sized>(range);
            auto last = RAH2_NS::ranges::pop_heap(view, &descending_64, &Coord::x);
            CHECK_EQUAL(last, view.end());
            CHECK(range.back() == expected);
            range.pop_back();
            CHECK(is_max_heap(range));
        };
        // test_proj({}, {5, 0});
        test_proj({{10, 0}}, {10, 0});
        test_proj({{1, 0}, {2, 0}, {3, 0}, {4, 0}, {6, 0}, {5, 0}, {9, 0}}, {1, 0});
        test_proj({{1, 0}, {1, 0}, {2, 0}, {3, 0}, {4, 0}, {6, 0}, {5, 0}, {9, 0}}, {1, 0});
        test_proj({{5, 0}, {5, 0}, {5, 0}, {5, 0}, {5, 0}}, {5, 0});
    }
    template <bool = true>
    void test_perf(char const* range_type)
    {
        RAH2_STD::vector<int> perf_iter_(1000000 * RELEASE_MULTIPLIER, 2);
        auto perf_iter = make_test_view_adapter<CS, Tag, Sized>(perf_iter_);

        RAH2_STD::vector<Coord> perf_(1000000 * RELEASE_MULTIPLIER, {2, 0});
        auto perf = make_test_view_adapter<CS, Tag, Sized>(perf_);
        static constexpr auto UnsizedMult = Sized ? 1000 : 1;

        COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(
            CS == Common,
            "pop_heap",
            range_type,
            [&]
            {
                for (int i = 0; i < 1 * UnsizedMult; ++i)
                {
                    perf_iter_.front() = 3;
                    perf_iter_.back() = 2;
                    STD::pop_heap(fwd(perf_iter.begin()), perf_iter.end());
                    CHECK(perf_iter_.back() == 3);
                }
            });
        COMPARE_DURATION_TO_STD_RANGES(
            "pop_heap_proj",
            range_type,
            [&]
            {
                for (int i = 0; i < 1 * UnsizedMult; ++i)
                {
                    perf_.front() = (Coord{1, 0});
                    perf_.back() = (Coord{2, 0});
                    STD::pop_heap(perf, descending_64, &Coord::x);
                    CHECK_EQUAL(perf_.back().x, 1);
                }
            });
    }
    static constexpr bool do_test = RAH2_NS::derived_from<Tag, RAH2_NS::random_access_iterator_tag>;
};
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

    foreach_range_combination<test_algo<test_pop_heap_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_sort_heap_
{
    static bool comp_64(intptr_t a, intptr_t b)
    {
        return b < a;
    }

    template <bool = true>
    void test()
    {
        testSuite.test_case("iter");
        {
            // empty
            RAH2_STD::vector<int> out_;
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            CHECK(RAH2_NS::ranges::is_heap(out));
            auto result = RAH2_NS::ranges::sort_heap(out.begin(), out.end());
            CHECK_EQUAL(result, out.end());
            CHECK_EQUAL(out_, (RAH2_STD::vector<int>{}));
        }
        {
            // single
            RAH2_STD::vector<int> out_{2};
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            CHECK(RAH2_NS::ranges::is_heap(out));
            auto result = RAH2_NS::ranges::sort_heap(out.begin(), out.end());
            CHECK_EQUAL(result, out.end());
            CHECK_EQUAL(out_, (RAH2_STD::vector<int>{2}));
        }
        {
            // sorted
            RAH2_STD::vector<int> out_{8, 6, 2, 4};
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            CHECK(RAH2_NS::ranges::is_heap(out));
            auto result = RAH2_NS::ranges::sort_heap(out.begin(), out.end());
            CHECK_EQUAL(result, out.end());
            CHECK_EQUAL(out_, (RAH2_STD::vector<int>{2, 4, 6, 8}));
        }
        {
            // unsorted
            RAH2_STD::vector<int> out_{7, 3, 5, 1};
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            CHECK(RAH2_NS::ranges::is_heap(out));
            auto result = RAH2_NS::ranges::sort_heap(out.begin(), out.end());
            CHECK_EQUAL(result, out.end());
            CHECK_EQUAL(out_, (RAH2_STD::vector<int>{1, 3, 5, 7}));
        }

        testSuite.test_case("range");
        {
            // empty
            RAH2_STD::vector<Coord> out_;
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            CHECK(RAH2_NS::ranges::is_heap(out, comp_64, &Coord::x));
            auto result = RAH2_NS::ranges::sort_heap(out, comp_64, &Coord::x);
            CHECK_EQUAL(result, out.end());
            CHECK_EQUAL(out_, (RAH2_STD::vector<Coord>{}));
        }
        {
            // single
            RAH2_STD::vector<Coord> out_{Coord{2, 0}};
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            CHECK(RAH2_NS::ranges::is_heap(out, comp_64, &Coord::x));
            auto result = RAH2_NS::ranges::sort_heap(out, comp_64, &Coord::x);
            CHECK_EQUAL(result, out.end());
            CHECK_EQUAL(out_, (RAH2_STD::vector<Coord>{Coord{2, 0}}));
        }
        {
            // sorted
            RAH2_STD::vector<Coord> out_{Coord{2, 0}, Coord{4, 0}, Coord{8, 0}, Coord{6, 0}};
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            CHECK(RAH2_NS::ranges::is_heap(out, comp_64, &Coord::x));
            auto result = RAH2_NS::ranges::sort_heap(out, comp_64, &Coord::x);
            CHECK_EQUAL(result, out.end());
            CHECK_EQUAL(out_, (RAH2_STD::vector<Coord>{{8, 0}, {6, 0}, {4, 0}, {2, 0}}));
        }
        {
            // unsorted
            RAH2_STD::vector<Coord> out_{Coord{1, 0}, Coord{3, 0}, Coord{5, 0}, Coord{7, 0}};
            auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
            CHECK(RAH2_NS::ranges::is_heap(out, comp_64, &Coord::x));
            auto result = RAH2_NS::ranges::sort_heap(out, comp_64, &Coord::x);
            CHECK_EQUAL(result, out.end());
            CHECK_EQUAL(out_, (RAH2_STD::vector<Coord>{{7, 0}, {5, 0}, {3, 0}, {1, 0}}));
        }
    }

    template <bool = true>
    void test_perf(char const* range_type)
    {
        testSuite.test_case("perf");
        {
            COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(
                CS == CommonOrSent::Common,
                "sort_heap_iter",
                range_type,
                (
                    [&]
                    {
                        RAH2_STD::vector<int> out_(200000 * RELEASE_MULTIPLIER);
                        out_.front() = 1;
                        auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
                        STD::sort_heap(RAH2_NS::ranges::begin(fwd(out)), RAH2_NS::ranges::end(out));
                        CHECK(out.front() == 0);
                    }));
        }

        {
            COMPARE_DURATION_TO_STD_RANGES(
                "sort_heap_ranges",
                range_type,
                ((
                    [&]
                    {
                        RAH2_STD::vector<Coord> out_(20000 * RELEASE_MULTIPLIER, Coord{2, 0});
                        out_.front() = Coord{1, 0};
                        auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
                        STD::sort_heap(out, comp_64, &Coord::x);
                        CHECK((out.front() == Coord{2, 0}));
                    })));
        }
    }
    static constexpr bool do_test = RAH2_NS::derived_from<Tag, RAH2_NS::random_access_iterator_tag>;
};
void test_sort_heap()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::sort_heap]
    RAH2_STD::array<int, 6> v{3, 1, 4, 1, 5, 9};
    RAH2_STD::make_heap(v.begin(), v.end());
    RAH2_NS::ranges::sort_heap(v);
    assert(RAH2_STD::is_sorted(v.begin(), v.end()));
    /// [rah2::ranges::sort_heap]

    foreach_range_combination<test_algo<test_sort_heap_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_max_
{
    static bool comp_64(intptr_t a, intptr_t b)
    {
        return b < a;
    }

    static bool comp(int a, int b)
    {
        return a < b;
    }

    template <bool = true>
    void test()
    {
        // std::ranges::max only works on ranges (not on iterators)
        testSuite.test_case("range");
        {
            std::vector<int> several_max = {4, 4, 4, 4};
            CHECK_EQUAL(RAH2_NS::ranges::max(make_test_view_adapter<CS, Tag, Sized>(several_max)), 4);
            std::vector<int> one_max = {4, 4, 5, 4};
            CHECK_EQUAL(
                RAH2_NS::ranges::max(make_test_view_adapter<CS, Tag, Sized>(one_max), comp), 5);
            std::vector<int> one_value = {4};
            CHECK_EQUAL(RAH2_NS::ranges::max(make_test_view_adapter<CS, Tag, Sized>(one_value)), 4);
        }

        testSuite.test_case("pred");
        testSuite.test_case("proj");
        {
            std::vector<Coord> several_max{Coord{4, 0}, Coord{4, 0}, Coord{4, 0}, Coord{4, 0}};
            CHECK_EQUAL(
                (RAH2_NS::ranges::max(
                    make_test_view_adapter<CS, Tag, Sized>(several_max), comp_64, &Coord::x)),
                (Coord{4, 0}));
            std::vector<Coord> one_max{Coord{4, 0}, Coord{4, 0}, Coord{3, 0}, Coord{4, 0}};
            CHECK_EQUAL(
                (RAH2_NS::ranges::max(
                    make_test_view_adapter<CS, Tag, Sized>(one_max), comp_64, &Coord::x)),
                (Coord{3, 0}));
            std::vector<Coord> one_value{Coord{4, 0}};
            CHECK_EQUAL(
                (RAH2_NS::ranges::max(
                    make_test_view_adapter<CS, Tag, Sized>(one_value), comp_64, &Coord::x)),
                (Coord{4, 0}));
        }
    }
    template <bool = true>
    void test_perf(char const* range_type)
    {
        std::vector<Coord> perf_no_vec(1000000 * RELEASE_MULTIPLIER, {2, 0});
        perf_no_vec.back() = Coord{3, 0};
        perf_no_vec[perf_no_vec.size() - 2] = Coord{1, 0};
        auto perf_no = make_test_view_adapter<CS, Tag, Sized>(perf_no_vec);
        COMPARE_DURATION_TO_STD_RANGES(
            "max",
            range_type,
            [&]
            {
                auto result = STD::max(perf_no);
                CHECK_EQUAL(result, (Coord{3, 0}));
            });
        COMPARE_DURATION_TO_STD_RANGES(
            "max_proj",
            range_type,
            [&]
            {
                auto result = STD::max(perf_no, comp_64, &Coord::x);
                CHECK_EQUAL(result, (Coord{1, 0}));
            });
    }
    static constexpr bool do_test = true;
};
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

    foreach_range_combination<test_algo<test_max_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_max_element_
{
    static bool comp_64(intptr_t a, intptr_t b)
    {
        return b < a;
    }

    static bool comp(int a, int b)
    {
        return a < b;
    }

    template <bool = true>
    void test()
    {
        testSuite.test_case("iter");
        {
            auto several_max = make_test_view_adapter<CS, Tag, Sized>(std::vector<int>{4, 4, 4, 4});
            auto iter = RAH2_NS::ranges::max_element(several_max.begin(), several_max.end());
            CHECK_EQUAL(iter, several_max.begin());
            std::vector<int> one_max_ = {4, 4, 5, 4};
            auto one_max = make_test_view_adapter<CS, Tag, Sized>(one_max_);
            auto iter2 = RAH2_NS::ranges::max_element(one_max.begin(), one_max.end(), comp);
            CHECK_EQUAL(iter2, RAH2_NS::ranges::next(one_max.begin(), 2));
            std::vector<int> one_value_ = {4};
            auto one_value = make_test_view_adapter<CS, Tag, Sized>(one_value_);
            auto iter3 = RAH2_NS::ranges::max_element(one_value.begin(), one_value.end());
            CHECK_EQUAL(iter3, one_value.begin());
            auto empty_max = make_test_view_adapter<CS, Tag, Sized>(std::vector<int>{});
            auto iter4 = RAH2_NS::ranges::max_element(empty_max.begin(), empty_max.end());
            CHECK_EQUAL(iter4, empty_max.end());
        }

        testSuite.test_case("range");
        testSuite.test_case("pred");
        testSuite.test_case("proj");
        {
            auto several_max = make_test_view_adapter<CS, Tag, Sized>(
                std::vector<Coord>{{4, 0}, {4, 0}, {4, 0}, {4, 0}});
            auto iter = RAH2_NS::ranges::max_element(several_max, comp_64, &Coord::x);
            CHECK_EQUAL(iter, several_max.begin());
            std::vector<Coord> one_max_ = {{4, 0}, {4, 0}, {3, 0}, {4, 0}};
            auto one_max = make_test_view_adapter<CS, Tag, Sized>(one_max_);
            auto iter2 = RAH2_NS::ranges::max_element(one_max, comp_64, &Coord::x);
            CHECK_EQUAL(iter2, RAH2_NS::ranges::next(one_max.begin(), 2));
            std::vector<Coord> one_value_{{4, 0}};
            auto one_value = make_test_view_adapter<CS, Tag, Sized>(one_value_);
            auto iter3 = RAH2_NS::ranges::max_element(one_value, comp_64, &Coord::x);
            CHECK_EQUAL(iter3, one_value.begin());
            auto empty_max = make_test_view_adapter<CS, Tag, Sized>(std::vector<Coord>{});
            auto iter4 = RAH2_NS::ranges::max_element(empty_max, comp_64, &Coord::x);
            CHECK_EQUAL(iter4, empty_max.end());
        }
    }
    template <bool = true>
    void test_perf(char const* range_type)
    {
        std::vector<Coord> perf_no_vec(1000000 * RELEASE_MULTIPLIER, {2, 0});
        perf_no_vec.back() = Coord{3, 0};
        perf_no_vec[perf_no_vec.size() - 2] = Coord{1, 0};
        auto perf_no = make_test_view_adapter<CS, Tag, Sized>(perf_no_vec);
        COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(
            CS == CommonOrSent::Common,
            "max_element",
            range_type,
            [&]
            {
                auto result = STD::max_element(fwd(perf_no.begin()), perf_no.end());
                CHECK_EQUAL(*result, (Coord{3, 0}));
            });
        COMPARE_DURATION_TO_STD_RANGES(
            "max_element_proj",
            range_type,
            [&]
            {
                auto result = STD::max_element(perf_no, comp_64, &Coord::x);
                CHECK_EQUAL(*result, (Coord{1, 0}));
            });
    }
    static constexpr bool do_test = RAH2_NS::derived_from<Tag, RAH2_NS::forward_iterator_tag>;
};
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
    foreach_range_combination<test_algo<test_max_element_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_min_
{
    static bool comp_64(intptr_t a, intptr_t b)
    {
        return b < a;
    }

    static bool comp(int a, int b)
    {
        return a < b;
    }

    template <bool = true>
    void test()
    {
        // std::ranges::max only works on ranges (not on iterators)
        testSuite.test_case("range");
        {
            std::vector<int> several_max = {4, 4, 4, 4};
            CHECK_EQUAL(RAH2_NS::ranges::min(make_test_view_adapter<CS, Tag, Sized>(several_max)), 4);
            std::vector<int> one_max = {4, 4, 3, 4};
            CHECK_EQUAL(
                RAH2_NS::ranges::min(make_test_view_adapter<CS, Tag, Sized>(one_max), comp), 3);
            std::vector<int> one_value = {4};
            CHECK_EQUAL(RAH2_NS::ranges::min(make_test_view_adapter<CS, Tag, Sized>(one_value)), 4);
        }

        testSuite.test_case("pred");
        testSuite.test_case("proj");
        {
            std::vector<Coord> several_max{Coord{4, 0}, Coord{4, 0}, Coord{4, 0}, Coord{4, 0}};
            CHECK_EQUAL(
                (RAH2_NS::ranges::min(
                    make_test_view_adapter<CS, Tag, Sized>(several_max), comp_64, &Coord::x)),
                (Coord{4, 0}));
            std::vector<Coord> one_max{Coord{4, 0}, Coord{4, 0}, Coord{5, 0}, Coord{4, 0}};
            CHECK_EQUAL(
                (RAH2_NS::ranges::min(
                    make_test_view_adapter<CS, Tag, Sized>(one_max), comp_64, &Coord::x)),
                (Coord{5, 0}));
            std::vector<Coord> one_value{Coord{4, 0}};
            CHECK_EQUAL(
                (RAH2_NS::ranges::min(
                    make_test_view_adapter<CS, Tag, Sized>(one_value), comp_64, &Coord::x)),
                (Coord{4, 0}));
        }
    }
    template <bool = true>
    void test_perf(char const* range_type)
    {
        std::vector<Coord> perf_no_vec(1000000 * RELEASE_MULTIPLIER, {2, 0});
        perf_no_vec.back() = Coord{1, 0};
        perf_no_vec[perf_no_vec.size() - 2] = Coord{3, 0};
        auto perf_no = make_test_view_adapter<CS, Tag, Sized>(perf_no_vec);
        COMPARE_DURATION_TO_STD_RANGES(
            "min",
            range_type,
            [&]
            {
                auto result = STD::min(perf_no);
                CHECK_EQUAL(result, (Coord{1, 0}));
            });
        COMPARE_DURATION_TO_STD_RANGES(
            "min_proj",
            range_type,
            [&]
            {
                auto result = STD::min(perf_no, comp_64, &Coord::x);
                CHECK_EQUAL(result, (Coord{3, 0}));
            });
    }
    static constexpr bool do_test = true;
};
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

    foreach_range_combination<test_algo<test_min_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_min_element_
{
    static bool comp_64(intptr_t a, intptr_t b)
    {
        return b < a;
    }

    static bool comp(int a, int b)
    {
        return a < b;
    }

    template <bool = true>
    void test()
    {
        testSuite.test_case("iter");
        {
            auto several_max = make_test_view_adapter<CS, Tag, Sized>(std::vector<int>{4, 4, 4, 4});
            auto iter = RAH2_NS::ranges::min_element(several_max.begin(), several_max.end());
            CHECK_EQUAL(iter, several_max.begin());
            auto one_max = make_test_view_adapter<CS, Tag, Sized>(std::vector<int>{4, 4, 3, 4});
            auto iter2 = RAH2_NS::ranges::min_element(one_max.begin(), one_max.end(), comp);
            CHECK_EQUAL(iter2, RAH2_NS::ranges::next(one_max.begin(), 2));
            auto one_value = make_test_view_adapter<CS, Tag, Sized>(std::vector<int>{4});
            auto iter3 = RAH2_NS::ranges::min_element(one_value.begin(), one_value.end());
            CHECK_EQUAL(iter3, one_value.begin());
            auto empty_max = make_test_view_adapter<CS, Tag, Sized>(std::vector<int>{});
            auto iter4 = RAH2_NS::ranges::min_element(empty_max.begin(), empty_max.end());
            CHECK_EQUAL(iter4, empty_max.end());
        }

        testSuite.test_case("range");
        testSuite.test_case("pred");
        testSuite.test_case("proj");
        {
            auto several_max = make_test_view_adapter<CS, Tag, Sized>(
                std::vector<Coord>{{4, 0}, {4, 0}, {4, 0}, {4, 0}});
            auto iter = RAH2_NS::ranges::min_element(several_max, comp_64, &Coord::x);
            CHECK_EQUAL(iter, several_max.begin());
            std::vector<Coord> one_max_ = {{4, 0}, {4, 0}, {5, 0}, {4, 0}};
            auto one_max = make_test_view_adapter<CS, Tag, Sized>(one_max_);
            auto iter2 = RAH2_NS::ranges::min_element(one_max, comp_64, &Coord::x);
            CHECK_EQUAL(iter2, RAH2_NS::ranges::next(one_max.begin(), 2));
            std::vector<Coord> one_value_{{4, 0}};
            auto one_value = make_test_view_adapter<CS, Tag, Sized>(one_value_);
            auto iter3 = RAH2_NS::ranges::min_element(one_value, comp_64, &Coord::x);
            CHECK_EQUAL(iter3, one_value.begin());
            auto empty_max = make_test_view_adapter<CS, Tag, Sized>(std::vector<Coord>{});
            auto iter4 = RAH2_NS::ranges::min_element(empty_max, comp_64, &Coord::x);
            CHECK_EQUAL(iter4, empty_max.end());
        }
    }
    template <bool = true>
    void test_perf(char const* range_type)
    {
        std::vector<Coord> perf_no_vec(1000000 * RELEASE_MULTIPLIER, {2, 0});
        perf_no_vec.back() = Coord{1, 0};
        perf_no_vec[perf_no_vec.size() - 2] = Coord{3, 0};
        auto perf_no = make_test_view_adapter<CS, Tag, Sized>(perf_no_vec);
        COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(
            CS == CommonOrSent::Common,
            "min_element",
            range_type,
            [&]
            {
                auto result = STD::min_element(fwd(perf_no.begin()), perf_no.end());
                CHECK_EQUAL(*result, (Coord{1, 0}));
            });
        COMPARE_DURATION_TO_STD_RANGES(
            "min_element_proj",
            range_type,
            [&]
            {
                auto result = STD::min_element(perf_no, comp_64, &Coord::x);
                CHECK_EQUAL(*result, (Coord{3, 0}));
            });
    }
    static constexpr bool do_test = RAH2_NS::derived_from<Tag, RAH2_NS::forward_iterator_tag>;
};
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

    foreach_range_combination<test_algo<test_min_element_>>();
}

namespace RAH2_NS
{
    namespace ranges
    {
        template <typename U, typename V>
        bool operator==(min_max_result<U> const& A, min_max_result<V> const& B)
        {
            return A.min == B.min && A.max == B.max;
        }
        template <typename U>
        std::ostream& operator<<(std::ostream& os, min_max_result<U> const& A)
        {
            os << "(" << A.min << "," << A.max << ")";
            return os;
        }
    } // namespace ranges
} // namespace RAH2_NS

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_minmax_
{
    static bool comp_64(intptr_t a, intptr_t b)
    {
        return b < a;
    }

    static bool comp(int a, int b)
    {
        return a < b;
    }

    template <bool = true>
    void test()
    {
        // std::ranges::max only works on ranges (not on iterators)
        testSuite.test_case("range");
        {
            std::vector<int> several_max = {4, 4, 4, 4};
            using ResultType = decltype(RAH2_NS::ranges::minmax(
                make_test_view_adapter<CS, Tag, Sized>(several_max)));
            CHECK_EQUAL(
                RAH2_NS::ranges::minmax(make_test_view_adapter<CS, Tag, Sized>(several_max)),
                (ResultType{4, 4}));
            std::vector<int> one_max = {4, 5, 3, 4};
            CHECK_EQUAL(
                RAH2_NS::ranges::minmax(make_test_view_adapter<CS, Tag, Sized>(one_max), comp),
                (ResultType{3, 5}));
            std::vector<int> one_value = {4};
            CHECK_EQUAL(
                RAH2_NS::ranges::minmax(make_test_view_adapter<CS, Tag, Sized>(one_value)),
                (ResultType{4, 4}));
        }

        testSuite.test_case("pred");
        testSuite.test_case("proj");
        {
            std::vector<Coord> several_max{Coord{4, 0}, Coord{4, 0}, Coord{4, 0}, Coord{4, 0}};
            using ResultType = decltype(RAH2_NS::ranges::minmax(
                make_test_view_adapter<CS, Tag, Sized>(several_max), comp_64, &Coord::x));
            CHECK_EQUAL(
                (RAH2_NS::ranges::minmax(
                    make_test_view_adapter<CS, Tag, Sized>(several_max), comp_64, &Coord::x)),
                (ResultType{Coord{4, 0}, Coord{4, 0}}));
            std::vector<Coord> one_max{Coord{4, 0}, Coord{3, 0}, Coord{5, 0}, Coord{4, 0}};
            CHECK_EQUAL(
                (RAH2_NS::ranges::minmax(
                    make_test_view_adapter<CS, Tag, Sized>(one_max), comp_64, &Coord::x)),
                (ResultType{Coord{5, 0}, Coord{3, 0}}));
            std::vector<Coord> one_value{Coord{4, 0}};
            CHECK_EQUAL(
                (RAH2_NS::ranges::minmax(
                    make_test_view_adapter<CS, Tag, Sized>(one_value), comp_64, &Coord::x)),
                (ResultType{Coord{4, 0}, Coord{4, 0}}));
        }
    }
    template <bool = true>
    void test_perf(char const* range_type)
    {
        std::vector<Coord> perf_no_vec(1000000 * RELEASE_MULTIPLIER, {2, 0});
        perf_no_vec.back() = Coord{1, 0};
        perf_no_vec[perf_no_vec.size() - 2] = Coord{3, 0};
        auto perf_no = make_test_view_adapter<CS, Tag, Sized>(perf_no_vec);

        COMPARE_DURATION_TO_STD_RANGES(
            "minmax",
            range_type,
            [&]
            {
                auto result = STD::minmax(perf_no);
                CHECK_EQUAL(result.min, (Coord{1, 0}));
                CHECK_EQUAL(result.max, (Coord{3, 0}));
            });
        COMPARE_DURATION_TO_STD_RANGES(
            "minmax_proj",
            range_type,
            [&]
            {
                auto result = STD::minmax(perf_no, comp_64, &Coord::x);
                CHECK_EQUAL(result.min, (Coord{3, 0}));
                CHECK_EQUAL(result.max, (Coord{1, 0}));
            });
    }
    static constexpr bool do_test = true;
};
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

    foreach_range_combination<test_algo<test_minmax_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_minmax_element_
{
    static bool comp_64(intptr_t a, intptr_t b)
    {
        return b < a;
    }

    static bool comp(int a, int b)
    {
        return a < b;
    }

    template <bool = true>
    void test()
    {
        testSuite.test_case("iter");
        {
            auto several_max = make_test_view_adapter<CS, Tag, Sized>(std::vector<int>{4, 4, 4, 4});
            auto result = RAH2_NS::ranges::minmax_element(several_max.begin(), several_max.end());
            CHECK_EQUAL(*result.min, 4);
            CHECK_EQUAL(*result.max, 4);
            CHECK_EQUAL(result.min, several_max.begin());
            CHECK_EQUAL(result.max, RAH2_NS::ranges::next(several_max.begin(), 3));

            auto one_max = make_test_view_adapter<CS, Tag, Sized>(std::vector<int>{4, 5, 3, 4});
            auto result2 = RAH2_NS::ranges::minmax_element(one_max.begin(), one_max.end(), comp);
            CHECK_EQUAL(*result2.min, 3);
            CHECK_EQUAL(*result2.max, 5);
            CHECK_EQUAL(result2.min, RAH2_NS::ranges::next(one_max.begin(), 2));
            CHECK_EQUAL(result2.max, RAH2_NS::ranges::next(one_max.begin(), 1));

            auto one_value = make_test_view_adapter<CS, Tag, Sized>(std::vector<int>{4});
            auto result3 = RAH2_NS::ranges::minmax_element(one_value.begin(), one_value.end());
            CHECK_EQUAL(*result3.min, 4);
            CHECK_EQUAL(*result3.max, 4);
            CHECK_EQUAL(result3.min, RAH2_NS::ranges::next(one_value.begin(), 0));
            CHECK_EQUAL(result3.max, RAH2_NS::ranges::next(one_value.begin(), 0));

            auto empty_max = make_test_view_adapter<CS, Tag, Sized>(std::vector<int>{});
            auto result4 = RAH2_NS::ranges::minmax_element(empty_max.begin(), empty_max.end());
            CHECK_EQUAL(result4.min, empty_max.end());
            CHECK_EQUAL(result4.max, empty_max.end());
        }

        testSuite.test_case("range");
        testSuite.test_case("pred");
        testSuite.test_case("proj");
        {
            auto several_max = make_test_view_adapter<CS, Tag, Sized>(
                std::vector<Coord>{{4, 0}, {4, 0}, {4, 0}, {4, 0}});
            auto result = RAH2_NS::ranges::minmax_element(several_max, comp_64, &Coord::x);
            CHECK_EQUAL(result.min, several_max.begin());
            CHECK_EQUAL(result.max, RAH2_NS::ranges::next(several_max.begin(), 3));

            std::vector<Coord> one_max_ = {{4, 0}, {3, 0}, {5, 0}, {4, 0}};
            auto one_max = make_test_view_adapter<CS, Tag, Sized>(one_max_);
            auto result2 = RAH2_NS::ranges::minmax_element(one_max, comp_64, &Coord::x);
            CHECK_EQUAL(result2.min, RAH2_NS::ranges::next(one_max.begin(), 2));
            CHECK_EQUAL(result2.max, RAH2_NS::ranges::next(one_max.begin(), 1));

            std::vector<Coord> one_value_{{4, 0}};
            auto one_value = make_test_view_adapter<CS, Tag, Sized>(one_value_);
            auto result3 = RAH2_NS::ranges::minmax_element(one_value, comp_64, &Coord::x);
            CHECK_EQUAL(result3.min, RAH2_NS::ranges::next(one_value.begin(), 0));
            CHECK_EQUAL(result3.max, RAH2_NS::ranges::next(one_value.begin(), 0));

            auto empty_max = make_test_view_adapter<CS, Tag, Sized>(std::vector<Coord>{});
            auto result4 = RAH2_NS::ranges::minmax_element(empty_max, comp_64, &Coord::x);
            CHECK_EQUAL(result4.min, empty_max.end());
            CHECK_EQUAL(result4.max, empty_max.end());
        }
    }
    template <bool = true>
    void test_perf(char const* range_type)
    {
        std::vector<Coord> perf_no_vec(1000000 * RELEASE_MULTIPLIER, {2, 0});
        perf_no_vec.back() = Coord{1, 0};
        perf_no_vec[perf_no_vec.size() - 2] = Coord{3, 0};
        auto perf_no = make_test_view_adapter<CS, Tag, Sized>(perf_no_vec);
        COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(
            CS == CommonOrSent::Common,
            "minmax_element",
            range_type,
            [&]
            {
                auto result = STD::minmax_element(fwd(perf_no.begin()), perf_no.end());
                DONT_OPTIM(result);
            });
        COMPARE_DURATION_TO_STD_RANGES(
            "minmax_element_proj",
            range_type,
            [&]
            {
                auto result = STD::minmax_element(perf_no, comp_64, &Coord::x);
                DONT_OPTIM(result);
            });
    }
    static constexpr bool do_test = RAH2_NS::derived_from<Tag, RAH2_NS::forward_iterator_tag>;
};
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

    foreach_range_combination<test_algo<test_minmax_element_>>();
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

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_is_permutation_
{
    static bool equal_64(intptr_t a, intptr_t b)
    {
        return a == b;
    }

    static bool equal(int a, int b)
    {
        return a < b;
    }

    template <bool = true>
    void test()
    {
        testSuite.test_case("iter");
        {
            // Identical ranges
            auto rng1 = make_test_view_adapter<CS, Tag, Sized>(std::vector<int>{1, 2, 3});
            auto rng2 = make_test_view_adapter<CS, Tag, Sized>(std::vector<int>{1, 2, 3});
            CHECK(RAH2_NS::ranges::is_permutation(rng1.begin(), rng1.end(), rng2.begin(), rng2.end()));
        }
        {
            // Permuted ranges
            auto rng1 = make_test_view_adapter<CS, Tag, Sized>(std::vector<int>{1, 2, 3});
            auto rng2 = make_test_view_adapter<CS, Tag, Sized>(std::vector<int>{3, 1, 2});
            CHECK(RAH2_NS::ranges::is_permutation(rng1.begin(), rng1.end(), rng2.begin(), rng2.end()));
        }
        {
            // Different sizes
            auto rng1 = make_test_view_adapter<CS, Tag, Sized>(std::vector<int>{1, 2, 3});
            auto rng2 = make_test_view_adapter<CS, Tag, Sized>(std::vector<int>{4, 5, 6});
            CHECK(
                RAH2_NS::ranges::is_permutation(rng1.begin(), rng1.end(), rng2.begin(), rng2.end())
                == false);
        }
        {
            // Empty ranges
            auto rng1 = make_test_view_adapter<CS, Tag, Sized>(std::vector<int>{});
            auto rng2 = make_test_view_adapter<CS, Tag, Sized>(std::vector<int>{});
            CHECK(RAH2_NS::ranges::is_permutation(rng1.begin(), rng1.end(), rng2.begin(), rng2.end()));
        }
        {
            // One empty, one non-empty
            auto rng1 = make_test_view_adapter<CS, Tag, Sized>(std::vector<int>{});
            auto rng2 = make_test_view_adapter<CS, Tag, Sized>(std::vector<int>{1, 2, 3});
            CHECK(
                RAH2_NS::ranges::is_permutation(rng1.begin(), rng1.end(), rng2.begin(), rng2.end())
                == false);
        }
        {
            // Single element ranges
            auto rng1 = make_test_view_adapter<CS, Tag, Sized>(std::vector<int>{1});
            auto rng2 = make_test_view_adapter<CS, Tag, Sized>(std::vector<int>{1});
            CHECK(RAH2_NS::ranges::is_permutation(rng1.begin(), rng1.end(), rng2.begin(), rng2.end()));
        }
        {
            // Single element mismatch
            auto rng1 = make_test_view_adapter<CS, Tag, Sized>(std::vector<int>{1});
            auto rng2 = make_test_view_adapter<CS, Tag, Sized>(std::vector<int>{2});
            CHECK(
                RAH2_NS::ranges::is_permutation(rng1.begin(), rng1.end(), rng2.begin(), rng2.end())
                == false);
        }

        testSuite.test_case("range");
        testSuite.test_case("proj");
        {
            // Identical ranges
            auto rng1 = make_test_view_adapter<CS, Tag, Sized>(
                std::vector<Coord>{Coord{1, 0}, Coord{2, 0}, Coord{3, 0}});
            auto rng2 = make_test_view_adapter<CS, Tag, Sized>(
                std::vector<Coord>{Coord{1, 0}, Coord{2, 0}, Coord{3, 0}});
            CHECK(RAH2_NS::ranges::is_permutation(rng1, rng2, equal_64, &Coord::x, &Coord::x));
        }
        {
            // Permuted ranges
            auto rng1 = make_test_view_adapter<CS, Tag, Sized>(
                std::vector<Coord>{Coord{1, 0}, Coord{2, 0}, Coord{3, 0}});
            auto rng2 = make_test_view_adapter<CS, Tag, Sized>(
                std::vector<Coord>{Coord{3, 0}, Coord{1, 0}, Coord{2, 0}});
            CHECK(RAH2_NS::ranges::is_permutation(rng1, rng2, equal_64, &Coord::x, &Coord::x));
        }
        {
            // Different sizes
            auto rng1 = make_test_view_adapter<CS, Tag, Sized>(
                std::vector<Coord>{Coord{1, 0}, Coord{2, 0}, Coord{3, 0}});
            auto rng2 = make_test_view_adapter<CS, Tag, Sized>(
                std::vector<Coord>{Coord{4, 0}, Coord{5, 0}, Coord{6, 0}});
            CHECK(RAH2_NS::ranges::is_permutation(rng1, rng2, equal_64, &Coord::x, &Coord::x) == false);
        }
        {
            // Empty ranges
            auto rng1 = make_test_view_adapter<CS, Tag, Sized>(std::vector<Coord>{});
            auto rng2 = make_test_view_adapter<CS, Tag, Sized>(std::vector<Coord>{});
            CHECK(RAH2_NS::ranges::is_permutation(rng1, rng2, equal_64, &Coord::x, &Coord::x));
        }
        {
            // One empty, one non-empty
            auto rng1 = make_test_view_adapter<CS, Tag, Sized>(std::vector<Coord>{});
            auto rng2 = make_test_view_adapter<CS, Tag, Sized>(
                std::vector<Coord>{Coord{1, 0}, Coord{2, 0}, Coord{3, 0}});
            CHECK(RAH2_NS::ranges::is_permutation(rng1, rng2, equal_64, &Coord::x, &Coord::x) == false);
        }
        {
            // Single element ranges
            auto rng1 = make_test_view_adapter<CS, Tag, Sized>(std::vector<Coord>{Coord{1, 0}});
            auto rng2 = make_test_view_adapter<CS, Tag, Sized>(std::vector<Coord>{Coord{1, 0}});
            CHECK(RAH2_NS::ranges::is_permutation(rng1, rng2, equal_64, &Coord::x, &Coord::x));
        }
        {
            // Single element mismatch
            auto rng1 = make_test_view_adapter<CS, Tag, Sized>(std::vector<Coord>{Coord{1, 0}});
            auto rng2 = make_test_view_adapter<CS, Tag, Sized>(std::vector<Coord>{Coord{2, 0}});
            CHECK(RAH2_NS::ranges::is_permutation(rng1, rng2, equal_64, &Coord::x, &Coord::x) == false);
        }
    }

    template <bool = true>
    void test_perf(char const* range_type)
    {
        std::vector<Coord> vec1;
        std::vector<Coord> vec2;
        constexpr auto VecSize = 1000;
        for (intptr_t i = 0; i < VecSize; ++i)
        {
            vec1.push_back(Coord{i, 0});
            vec2.push_back(Coord{(VecSize - 1) - i, 0});
        }
        auto rng1 = make_test_view_adapter<CS, Tag, Sized>(vec1);
        auto rng2 = make_test_view_adapter<CS, Tag, Sized>(vec2);

        {
            COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(
                CS == Common,
                "is_permutation",
                range_type,
                [&]
                {
                    bool const result = RAH2_NS::ranges::is_permutation(
                        fwd(rng1.begin()), rng1.end(), rng2.begin(), rng2.end());
                    CHECK(result);
                });
        }
        {
            COMPARE_DURATION_TO_STD_RANGES(
                "is_permutation_proj",
                range_type,
                (
                    [&]
                    {
                        bool const result = RAH2_NS::ranges::is_permutation(
                            rng1, rng2, equal_64, &Coord::x, &Coord::x);
                        CHECK(result);
                    }));
        }
    }
    static constexpr bool do_test = RAH2_NS::derived_from<Tag, RAH2_NS::forward_iterator_tag>;
};
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

    foreach_range_combination<test_algo<test_is_permutation_>>();
}

size_t factorial(size_t n)
{
    size_t f = 1;
    for (size_t i = 1; i <= n; ++i)
        f *= i;
    return f;
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_next_permutation_
{
    static bool comp_64(intptr_t a, intptr_t b)
    {
        return b < a;
    }

    template <bool = true>
    void test()
    {
        testSuite.test_case("iter");
        {
            // Ascending order permutation
            std::vector<int> vec = {1, 2, 3};
            auto rng = make_test_view_adapter<CS, Tag, Sized>(vec);
            auto result = RAH2_NS::ranges::next_permutation(rng.begin(), rng.end());
            CHECK_EQUAL(result.in, rng.end());
            CHECK(result.found);
            CHECK_EQUAL(rng, (RAH2_STD::vector<int>{1, 3, 2}));
        }
        {
            // Ascending order permutation
            std::vector<int> vec = {3, 2, 1};
            auto rng = make_test_view_adapter<CS, Tag, Sized>(vec);
            auto result = RAH2_NS::ranges::next_permutation(rng.begin(), rng.end());
            CHECK_EQUAL(result.in, rng.end());
            CHECK(result.found == false);
            CHECK_EQUAL(rng, (RAH2_STD::vector<int>{1, 2, 3}));
        }
        {
            // Single element vector
            std::vector<int> vec = {1};
            auto rng = make_test_view_adapter<CS, Tag, Sized>(vec);
            auto result = RAH2_NS::ranges::next_permutation(rng.begin(), rng.end());
            CHECK_EQUAL(result.in, rng.end());
            CHECK(result.found == false);
            CHECK_EQUAL(rng, (RAH2_STD::vector<int>{1}));
        }
        {
            // Empty vector
            std::vector<int> vec = {};
            auto rng = make_test_view_adapter<CS, Tag, Sized>(vec);
            auto result = RAH2_NS::ranges::next_permutation(rng.begin(), rng.end());
            CHECK_EQUAL(result.in, rng.end());
            CHECK(result.found == false);
            CHECK_EQUAL(rng, (RAH2_STD::vector<int>{}));
        }
        {
            // Duplicate values
            std::vector<int> vec = {1, 1, 2, 2};
            auto rng = make_test_view_adapter<CS, Tag, Sized>(vec);
            auto result = RAH2_NS::ranges::next_permutation(rng.begin(), rng.end());
            CHECK_EQUAL(result.in, rng.end());
            CHECK(result.found == true);
            CHECK_EQUAL(rng, (RAH2_STD::vector<int>{1, 2, 1, 2}));
        }
        {
            // All elements identical
            std::vector<int> vec = {1, 1, 1};
            auto rng = make_test_view_adapter<CS, Tag, Sized>(vec);
            auto result = RAH2_NS::ranges::next_permutation(rng.begin(), rng.end());
            CHECK_EQUAL(result.in, rng.end());
            CHECK(result.found == false);
            CHECK_EQUAL(rng, (RAH2_STD::vector<int>{1, 1, 1}));
        }
        {
            // Large vector
            std::vector<int> vec = {1, 2, 3, 4, 5, 6, 7, 8};
            auto rng = make_test_view_adapter<CS, Tag, Sized>(vec);
            auto result = RAH2_NS::ranges::next_permutation(rng.begin(), rng.end());
            CHECK_EQUAL(result.in, rng.end());
            CHECK(result.found);
            CHECK_EQUAL(rng, (RAH2_STD::vector<int>{1, 2, 3, 4, 5, 6, 8, 7}));
        }

        testSuite.test_case("range");
        testSuite.test_case("proj");
        {
            // range/proj
            std::vector<Coord> vec = {
                Coord{8, 0},
                Coord{7, 0},
                Coord{6, 0},
                Coord{5, 0},
                Coord{4, 0},
                Coord{3, 0},
                Coord{2, 0},
                Coord{1, 0}};
            auto rng = make_test_view_adapter<CS, Tag, Sized>(vec);
            auto result = RAH2_NS::ranges::next_permutation(rng, comp_64, &Coord::x);
            CHECK_EQUAL(result.in, rng.end());
            CHECK(result.found);
            CHECK_EQUAL(
                rng,
                (RAH2_STD::vector<Coord>{
                    Coord{8, 0},
                    Coord{7, 0},
                    Coord{6, 0},
                    Coord{5, 0},
                    Coord{4, 0},
                    Coord{3, 0},
                    Coord{1, 0},
                    Coord{2, 0}}));
        }
    }

    template <bool = true>
    void test_perf(char const* range_type)
    {
        std::vector<Coord> vec1;
        constexpr auto VecSize = 1000000;
        for (intptr_t i = 0; i < VecSize; ++i)
        {
            vec1.push_back(Coord{i, 0});
        }
        RAH2_STD::swap(vec1[500000], vec1[1000000 - 1]);
        auto rng1 = make_test_view_adapter<CS, Tag, Sized>(vec1);

        {
            COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(
                CS == Common,
                "next_permutation",
                range_type,
                [&]
                {
                    for (int i = 0; i < 100; ++i)
                    {
                        auto const result =
                            RAH2_NS::ranges::next_permutation(fwd(rng1.begin()), rng1.end());
                        DONT_OPTIM(result);
                    }
                });
        }
        {
            COMPARE_DURATION_TO_STD_RANGES(
                "next_permutation_proj",
                range_type,
                (
                    [&]
                    {
                        for (int i = 0; i < 100; ++i)
                        {
                            auto const result =
                                RAH2_NS::ranges::next_permutation(rng1, comp_64, &Coord::x);
                            CHECK(result.found);
                        }
                    }));
        }
    }
    static constexpr bool do_test = RAH2_NS::derived_from<Tag, RAH2_NS::bidirectional_iterator_tag>;
};
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

    foreach_range_combination<test_algo<test_next_permutation_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_prev_permutation_
{
    static bool comp_64(intptr_t a, intptr_t b)
    {
        return b < a;
    }

    template <bool = true>
    void test()
    {
        testSuite.test_case("iter");
        {
            // Ascending order permutation
            std::vector<int> vec = {1, 3, 2};
            auto rng = make_test_view_adapter<CS, Tag, Sized>(vec);
            auto result = RAH2_NS::ranges::prev_permutation(rng.begin(), rng.end());
            CHECK_EQUAL(result.in, rng.end());
            CHECK(result.found);
            CHECK_EQUAL(rng, (RAH2_STD::vector<int>{1, 2, 3}));
        }
        {
            // Ascending order permutation
            std::vector<int> vec = {1, 2, 3};
            auto rng = make_test_view_adapter<CS, Tag, Sized>(vec);
            auto result = RAH2_NS::ranges::prev_permutation(rng.begin(), rng.end());
            CHECK_EQUAL(result.in, rng.end());
            CHECK(result.found == false);
            CHECK_EQUAL(rng, (RAH2_STD::vector<int>{3, 2, 1}));
        }
        {
            // Single element vector
            std::vector<int> vec = {1};
            auto rng = make_test_view_adapter<CS, Tag, Sized>(vec);
            auto result = RAH2_NS::ranges::prev_permutation(rng.begin(), rng.end());
            CHECK_EQUAL(result.in, rng.end());
            CHECK(result.found == false);
            CHECK_EQUAL(rng, (RAH2_STD::vector<int>{1}));
        }
        {
            // Empty vector
            std::vector<int> vec = {};
            auto rng = make_test_view_adapter<CS, Tag, Sized>(vec);
            auto result = RAH2_NS::ranges::prev_permutation(rng.begin(), rng.end());
            CHECK_EQUAL(result.in, rng.end());
            CHECK(result.found == false);
            CHECK_EQUAL(rng, (RAH2_STD::vector<int>{}));
        }
        {
            // Duplicate values
            std::vector<int> vec = {1, 2, 1, 2};
            auto rng = make_test_view_adapter<CS, Tag, Sized>(vec);
            auto result = RAH2_NS::ranges::prev_permutation(rng.begin(), rng.end());
            CHECK_EQUAL(result.in, rng.end());
            CHECK(result.found == true);
            CHECK_EQUAL(rng, (RAH2_STD::vector<int>{1, 1, 2, 2}));
        }
        {
            // All elements identical
            std::vector<int> vec = {1, 1, 1};
            auto rng = make_test_view_adapter<CS, Tag, Sized>(vec);
            auto result = RAH2_NS::ranges::prev_permutation(rng.begin(), rng.end());
            CHECK_EQUAL(result.in, rng.end());
            CHECK(result.found == false);
            CHECK_EQUAL(rng, (RAH2_STD::vector<int>{1, 1, 1}));
        }
        {
            // Large vector
            std::vector<int> vec = {1, 2, 3, 4, 5, 6, 8, 7};
            auto rng = make_test_view_adapter<CS, Tag, Sized>(vec);
            auto result = RAH2_NS::ranges::prev_permutation(rng.begin(), rng.end());
            CHECK_EQUAL(result.in, rng.end());
            CHECK(result.found);
            CHECK_EQUAL(rng, (RAH2_STD::vector<int>{1, 2, 3, 4, 5, 6, 7, 8}));
        }

        testSuite.test_case("range");
        testSuite.test_case("proj");
        {
            // range/proj
            std::vector<Coord> vec = {
                Coord{8, 0},
                Coord{7, 0},
                Coord{6, 0},
                Coord{5, 0},
                Coord{4, 0},
                Coord{3, 0},
                Coord{1, 0},
                Coord{2, 0}};
            auto rng = make_test_view_adapter<CS, Tag, Sized>(vec);
            auto result = RAH2_NS::ranges::prev_permutation(rng, comp_64, &Coord::x);
            CHECK_EQUAL(result.in, rng.end());
            CHECK(result.found);
            CHECK_EQUAL(
                rng,
                (RAH2_STD::vector<Coord>{
                    Coord{8, 0},
                    Coord{7, 0},
                    Coord{6, 0},
                    Coord{5, 0},
                    Coord{4, 0},
                    Coord{3, 0},
                    Coord{2, 0},
                    Coord{1, 0}}));
        }
    }

    template <bool = true>
    void test_perf(char const* range_type)
    {
        std::vector<Coord> vec1;
        constexpr auto VecSize = 1000000;
        for (intptr_t i = 0; i < VecSize; ++i)
        {
            vec1.push_back(Coord{i, 0});
        }
        RAH2_STD::swap(vec1[500000], vec1[1000000 - 1]);
        auto rng1 = make_test_view_adapter<CS, Tag, Sized>(vec1);

        {
            COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(
                CS == Common,
                "prev_permutation",
                range_type,
                [&]
                {
                    for (int i = 0; i < 100; ++i)
                    {

                        auto const result =
                            RAH2_NS::ranges::prev_permutation(fwd(rng1.begin()), rng1.end());
                        DONT_OPTIM(result);
                    }
                });
        }
        {
            COMPARE_DURATION_TO_STD_RANGES(
                "prev_permutation_proj",
                range_type,
                (
                    [&]
                    {
                        for (int i = 0; i < 100; ++i)
                        {

                            auto const result =
                                RAH2_NS::ranges::prev_permutation(rng1, comp_64, &Coord::x);
                            CHECK(result.found);
                        }
                    }));
        }
    }
    static constexpr bool do_test = RAH2_NS::derived_from<Tag, RAH2_NS::bidirectional_iterator_tag>;
};
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

    foreach_range_combination<test_algo<test_prev_permutation_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_iota_
{
    template <bool = true>
    void test()
    {
        RAH2_STD::vector<int> out_{0, 0, 0, 4, 5};
        auto out = make_test_view_adapter<CS, Tag, Sized>(out_);
        testSuite.test_case("iter");
        auto result = RAH2_NS::ranges::iota(out.begin(), out.end(), 12);
        CHECK(result.out == out.end());
        CHECK(out == (RAH2_STD::vector<int>{12, 13, 14, 15, 16}));

        testSuite.test_case("range");
        auto result2 = RAH2_NS::ranges::iota(out, 72);
        CHECK(result2.out == out.end());
        CHECK(out == (RAH2_STD::vector<int>{72, 73, 74, 75, 76}));

        testSuite.test_case("empty");
        {
            RAH2_STD::vector<int> empty_out_;
            auto empty_out = make_test_view_adapter<CS, Tag, Sized>(empty_out_);

            auto result3 = RAH2_NS::ranges::iota(empty_out.begin(), empty_out.end(), 169);
            CHECK(result3.out == empty_out.end());
            CHECK(empty_out.empty());
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
            COMPARE_DURATION_TO_STD_ALGO_11_AND_RANGES_23(
                CS == Common,
                "iota_iter",
                range_type,
                (
                    [&]
                    {
                        STD::iota(RAH2_NS::ranges::begin(fwd(out)), RAH2_NS::ranges::end(out), 16);
                        DONT_OPTIM(*out.begin());
                    }));
        }

        {
            COMPARE_DURATION_TO_STD_RANGES_23(
                "iota_ranges",
                range_type,
                (
                    [&]
                    {
                        auto result2 = STD::iota(out, 27);
                        CHECK(result2.out == out.end());
                    }));
        }
    }
    // Only use an output iterator, so no need to test several input iterator types
    static constexpr bool do_test = RAH2_NS::derived_from<Tag, RAH2_NS::contiguous_iterator_tag>;
};
void test_iota()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::iota]
    RAH2_STD::list<int> list(8);

    // Fill the list with ascending values: 0, 1, 2, ..., 7
    RAH2_NS::ranges::iota(list, 0);
    assert(list == (RAH2_STD::list<int>{0, 1, 2, 3, 4, 5, 6, 7}));
    /// [rah2::ranges::iota]

    foreach_range_combination<test_algo<test_iota_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_fold_left_
{
    struct divide
    {
        template <typename T, typename U>
        constexpr auto operator()(T const& _Left, U const& _Right) const
        {
            return _Left / _Right;
        }
    };

    template <bool = true>
    void test()
    {
        testSuite.test_case("range");
        RAH2_STD::vector<int> intsVec({4, 4, 4, 3});
        auto intsRange = make_test_view_adapter<CS, Tag, Sized>(intsVec);
        CHECK_EQUAL(RAH2_NS::ranges::fold_left(intsRange, 3, RAH2_STD::plus<int>()), 18);

        RAH2_STD::vector<int> intsVec2({2, 2, 2});
        auto intsRange2 = make_test_view_adapter<CS, Tag, Sized>(intsVec2);
        CHECK_EQUAL(RAH2_NS::ranges::fold_left(intsRange2, 16, divide()), 2);

        RAH2_STD::vector<RAH2_STD::string> stringRange_({"b", "c", "d"});
        auto stringRange = make_test_view_adapter<CS, Tag, Sized>(stringRange_);
        CHECK_EQUAL(
            RAH2_NS::ranges::fold_left(stringRange, "a", RAH2_STD::plus<RAH2_STD::string>()), "abcd");

        testSuite.test_case("iter");
        CHECK_EQUAL(
            RAH2_NS::ranges::fold_left(intsRange.begin(), intsRange.end(), 3, RAH2_STD::plus<int>()),
            18);

        testSuite.test_case("empty");
        {
            auto emptyRange = make_test_view_adapter<CS, Tag, Sized>(RAH2_STD::vector<int>());
            CHECK_EQUAL(RAH2_NS::ranges::fold_left(emptyRange, 3, RAH2_STD::plus<int>()), 3);
        }
    }
    template <bool = true>
    void test_perf(char const* range_type)
    {
        constexpr auto vecSize = 1000000;
        RAH2_STD::vector<int> intsVec(1, vecSize);
        auto intsRange = make_test_view_adapter<CS, Tag, Sized>(intsVec);
        testSuite.test_case("perf");
        COMPARE_DURATION_TO_STD_RANGES_23(
            "fold_left",
            range_type,
            [&]
            {
                for (size_t i = 0; i < 1000 * RELEASE_MULTIPLIER; ++i)
                {
                    const auto result = STD::fold_left(
                        fwd(intsRange.begin()), intsRange.end(), 2, RAH2_STD::plus<int>());
                    assert(result == vecSize + 2);
                    const auto result2 = STD::fold_left(intsRange, 2, RAH2_STD::plus<int>());
                    assert(result2 == vecSize + 2);
                }
            });
    }
    static constexpr bool do_test = true;
};
void test_fold_left()
{
    testSuite.test_case("sample");
    /// [rah2::ranges::fold_left]
    RAH2_STD::vector<int> vecIn1{1, 2, 3, 4};
    assert(RAH2_NS::ranges::fold_left(vecIn1, 0, [](auto a, auto b) { return a + b; }) == 10);
    /// [rah2::ranges::fold_left]

    foreach_range_combination<test_algo<test_fold_left_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_fold_left_first_
{
    struct divide
    {
        template <typename T, typename U>
        constexpr auto operator()(T const& _Left, U const& _Right) const
        {
            return _Left / _Right;
        }
    };

    template <bool = true>
    void test()
    {
        testSuite.test_case("range");
        RAH2_STD::vector<int> intsVec({4, 4, 4, 3});
        auto intsRange = make_test_view_adapter<CS, Tag, Sized>(intsVec);
        CHECK_EQUAL(*RAH2_NS::ranges::fold_left_first(intsRange, RAH2_STD::plus<int>()), 15);

        RAH2_STD::vector<int> intsVec2({16, 2, 2, 2});
        auto intsRange2 = make_test_view_adapter<CS, Tag, Sized>(intsVec2);
        CHECK_EQUAL(*RAH2_NS::ranges::fold_left_first(intsRange2, divide()), 2);

        RAH2_STD::vector<RAH2_STD::string> stringRange_({"a", "b", "c", "d"});
        auto stringRange = make_test_view_adapter<CS, Tag, Sized>(stringRange_);
        auto result =
            *RAH2_NS::ranges::fold_left_first(stringRange, RAH2_STD::plus<RAH2_STD::string>());
        CHECK_EQUAL(result, "abcd");
        auto result2 =
            *RAH2_NS::ranges::fold_left_first(stringRange, RAH2_STD::plus<RAH2_STD::string>());
        CHECK_EQUAL(result2, "abcd");

        testSuite.test_case("iter");
        CHECK_EQUAL(
            *RAH2_NS::ranges::fold_left_first(
                intsRange.begin(), intsRange.end(), RAH2_STD::plus<int>()),
            15);

        testSuite.test_case("empty");
        RAH2_STD::vector<int> emptyVec;
        auto emptyRange = make_test_view_adapter<CS, Tag, Sized>(emptyVec);
        CHECK(RAH2_NS::ranges::fold_left_first(emptyRange, RAH2_STD::plus<int>()) == nullptr);
    }
    template <bool = true>
    void test_perf(char const* range_type)
    {
        constexpr auto vecSize = 1000000;
        RAH2_STD::vector<int> intsVec(1, vecSize);
        auto intsRange = make_test_view_adapter<CS, Tag, Sized>(intsVec);
        testSuite.test_case("perf");
        COMPARE_DURATION_TO_STD_RANGES_23(
            "fold_left_first",
            range_type,
            [&]
            {
                for (size_t i = 0; i < 1000 * RELEASE_MULTIPLIER; ++i)
                {

                    const auto result = *STD::fold_left_first(
                        fwd(intsRange.begin()), intsRange.end(), RAH2_STD::plus<int>());
                    assert(result == vecSize);
                    const auto result2 = *STD::fold_left_first(intsRange, RAH2_STD::plus<int>());
                    assert(result2 == vecSize);
                }
            });
    }
    static constexpr bool do_test = true;
};
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

    foreach_range_combination<test_algo<test_fold_left_first_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_fold_right_
{
    struct divide
    {
        template <typename T, typename U>
        constexpr auto operator()(T const& _Left, U const& _Right) const
        {
            return _Left / _Right;
        }
    };

    template <bool = true>
    void test()
    {
        testSuite.test_case("range");
        RAH2_STD::vector<int> intsVec({4, 4, 4, 3});
        auto intsRange = make_test_view_adapter<CS, Tag, Sized>(intsVec);
        CHECK_EQUAL(RAH2_NS::ranges::fold_right(intsRange, 3, RAH2_STD::plus<int>()), 18);

        RAH2_STD::vector<int> intsVec2({4, 4, 4, 4, 4});
        auto intsRange2 = make_test_view_adapter<CS, Tag, Sized>(intsVec2);
        CHECK_EQUAL(RAH2_NS::ranges::fold_right(intsRange2, 2, divide()), 2);

        RAH2_STD::vector<RAH2_STD::string> stringRange_({"b", "c", "d"});
        auto stringRange = make_test_view_adapter<CS, Tag, Sized>(stringRange_);
        CHECK_EQUAL(
            RAH2_NS::ranges::fold_right(stringRange, "e", RAH2_STD::plus<RAH2_STD::string>()),
            "bcde");

        testSuite.test_case("iter");
        CHECK_EQUAL(
            RAH2_NS::ranges::fold_right(intsRange.begin(), intsRange.end(), 3, RAH2_STD::plus<int>()),
            18);

        testSuite.test_case("empty");
        auto emptyRange = make_test_view_adapter<CS, Tag, Sized>(RAH2_STD::vector<int>());
        CHECK_EQUAL(RAH2_NS::ranges::fold_right(emptyRange, 3, RAH2_STD::plus<int>()), 3);
    }
    template <bool = true>
    void test_perf(char const* range_type)
    {
        constexpr auto vecSize = 1000000;
        RAH2_STD::vector<int> intsVec(1, vecSize);
        auto intsRange = make_test_view_adapter<CS, Tag, Sized>(intsVec);
        testSuite.test_case("perf");
        COMPARE_DURATION_TO_STD_RANGES_23(
            "fold_right",
            range_type,
            [&]
            {
                for (size_t i = 0; i < 1000 * RELEASE_MULTIPLIER; ++i)
                {
                    const auto result = STD::fold_right(
                        fwd(intsRange.begin()), intsRange.end(), 2, RAH2_STD::plus<int>());
                    assert(result == vecSize + 2);
                    const auto result2 = STD::fold_right(intsRange, 2, RAH2_STD::plus<int>());
                    assert(result2 == vecSize + 2);
                }
            });
    }
    static constexpr bool do_test = RAH2_NS::derived_from<Tag, RAH2_NS::bidirectional_iterator_tag>;
};
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

    foreach_range_combination<test_algo<test_fold_right_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_fold_right_last_
{
    struct divide
    {
        template <typename T, typename U>
        constexpr auto operator()(T const& _Left, U const& _Right) const
        {
            return _Left / _Right;
        }
    };

    template <bool = true>
    void test()
    {
        testSuite.test_case("range");
        RAH2_STD::vector<int> intsVec({4, 4, 4, 3, 3});
        auto intsRange = make_test_view_adapter<CS, Tag, Sized>(intsVec);
        CHECK_EQUAL(*RAH2_NS::ranges::fold_right_last(intsRange, RAH2_STD::plus<int>()), 18);

        RAH2_STD::vector<int> intsVec2({4, 4, 4, 4, 4, 2});
        auto intsRange2 = make_test_view_adapter<CS, Tag, Sized>(intsVec2);
        CHECK_EQUAL(*RAH2_NS::ranges::fold_right_last(intsRange2, divide()), 2);

        RAH2_STD::vector<RAH2_STD::string> stringRange_({"b", "c", "d", "e"});
        auto stringRange = make_test_view_adapter<CS, Tag, Sized>(stringRange_);
        CHECK_EQUAL(
            *RAH2_NS::ranges::fold_right_last(stringRange, RAH2_STD::plus<RAH2_STD::string>()),
            "bcde");

        testSuite.test_case("iter");
        CHECK_EQUAL(
            *RAH2_NS::ranges::fold_right_last(
                intsRange.begin(), intsRange.end(), RAH2_STD::plus<int>()),
            18);

        testSuite.test_case("range");
        RAH2_STD::vector<int> emptyVec({3});
        auto emptyRange = make_test_view_adapter<CS, Tag, Sized>(emptyVec);
        CHECK_EQUAL(*RAH2_NS::ranges::fold_right_last(emptyRange, RAH2_STD::plus<int>()), 3);
    }
    template <bool = true>
    void test_perf(char const* range_type)
    {
        constexpr auto vecSize = 1000000;
        RAH2_STD::vector<int> intsVec(1, vecSize);
        auto intsRange = make_test_view_adapter<CS, Tag, Sized>(intsVec);
        testSuite.test_case("perf");
        COMPARE_DURATION_TO_STD_RANGES_23(
            "fold_right_last",
            range_type,
            [&]
            {
                for (size_t i = 0; i < 1000 * RELEASE_MULTIPLIER; ++i)
                {
                    const auto result = *STD::fold_right_last(
                        fwd(intsRange.begin()), intsRange.end(), RAH2_STD::plus<int>());
                    assert(result == vecSize);
                    const auto result2 = *STD::fold_right_last(intsRange, RAH2_STD::plus<int>());
                    assert(result2 == vecSize);
                }
            });
    }
    static constexpr bool do_test = RAH2_NS::derived_from<Tag, RAH2_NS::bidirectional_iterator_tag>;
};
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

    foreach_range_combination<test_algo<test_fold_right_last_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_fold_left_with_iter_
{
    struct divide
    {
        template <typename T, typename U>
        constexpr auto operator()(T const& _Left, U const& _Right) const
        {
            return _Left / _Right;
        }
    };

    template <bool = true>
    void test()
    {
        testSuite.test_case("range");
        RAH2_STD::vector<int> intsVec({4, 4, 4, 3});
        auto intsRange = make_test_view_adapter<CS, Tag, Sized>(intsVec);
        auto result = RAH2_NS::ranges::fold_left_with_iter(intsRange, 3, RAH2_STD::plus<int>());
        CHECK_EQUAL(result.value, 18);
        CHECK_EQUAL(result.in, intsRange.end());

        RAH2_STD::vector<int> intsVec2({2, 2, 2});
        auto intsRange2 = make_test_view_adapter<CS, Tag, Sized>(intsVec2);
        auto result2 = RAH2_NS::ranges::fold_left_with_iter(intsRange2, 16, divide());
        CHECK_EQUAL(result2.value, 2);
        CHECK_EQUAL(result2.in, intsRange2.end());

        RAH2_STD::vector<RAH2_STD::string> stringRange_({"b", "c", "d"});
        auto stringRange = make_test_view_adapter<CS, Tag, Sized>(stringRange_);
        auto result3 = RAH2_NS::ranges::fold_left_with_iter(
            stringRange, "a", RAH2_STD::plus<RAH2_STD::string>());
        CHECK_EQUAL(result3.value, "abcd");
        CHECK_EQUAL(result3.in, stringRange.end());

        testSuite.test_case("iter");
        auto result4 = RAH2_NS::ranges::fold_left_with_iter(
            intsRange.begin(), intsRange.end(), 3, RAH2_STD::plus<int>());
        CHECK_EQUAL(result4.value, 18);
        CHECK_EQUAL(result4.in, intsRange.end());

        testSuite.test_case("empty");
        auto emptyRange = make_test_view_adapter<CS, Tag, Sized>(RAH2_STD::vector<int>());
        auto result5 = RAH2_NS::ranges::fold_left_with_iter(emptyRange, 3, RAH2_STD::plus<int>());
        CHECK_EQUAL(result5.value, 3);
        CHECK_EQUAL(result5.in, emptyRange.end());
    }
    template <bool = true>
    void test_perf(char const* range_type)
    {
        constexpr auto vecSize = 1000000;
        RAH2_STD::vector<int> intsVec(1, vecSize);
        auto intsRange = make_test_view_adapter<CS, Tag, Sized>(intsVec);
        testSuite.test_case("perf");
        COMPARE_DURATION_TO_STD_RANGES_23(
            "fold_left_with_iter",
            range_type,
            [&]
            {
                for (size_t i = 0; i < 1000 * RELEASE_MULTIPLIER; ++i)
                {
                    const auto result = STD::fold_left_with_iter(
                        fwd(intsRange.begin()), intsRange.end(), 2, RAH2_STD::plus<int>());
                    assert(result.value == vecSize + 2);
                    const auto result2 =
                        STD::fold_left_with_iter(intsRange, 2, RAH2_STD::plus<int>());
                    assert(result2.value == vecSize + 2);
                }
            });
    }
    static constexpr bool do_test = true;
};
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

    foreach_range_combination<test_algo<test_fold_left_with_iter_>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct test_fold_left_first_with_iter_
{
    struct divide
    {
        template <typename T, typename U>
        constexpr auto operator()(T const& _Left, U const& _Right) const
        {
            return _Left / _Right;
        }
    };

    template <bool = true>
    void test()
    {
        testSuite.test_case("range");
        RAH2_STD::vector<int> intsVec({4, 4, 4, 3});
        auto intsRange = make_test_view_adapter<CS, Tag, Sized>(intsVec);
        auto result = RAH2_NS::ranges::fold_left_first_with_iter(intsRange, RAH2_STD::plus<int>());
        CHECK_EQUAL(*result.value, 15);
        CHECK_EQUAL(result.in, intsRange.end());

        RAH2_STD::vector<int> intsVec2({16, 2, 2, 2});
        auto intsRange2 = make_test_view_adapter<CS, Tag, Sized>(intsVec2);
        auto result2 = RAH2_NS::ranges::fold_left_first_with_iter(intsRange2, divide());
        CHECK_EQUAL(*result2.value, 2);
        CHECK_EQUAL(result2.in, intsRange2.end());

        RAH2_STD::vector<RAH2_STD::string> stringRange_({"a", "b", "c", "d"});
        auto stringRange = make_test_view_adapter<CS, Tag, Sized>(stringRange_);
        auto result3 = RAH2_NS::ranges::fold_left_first_with_iter(
            stringRange, RAH2_STD::plus<RAH2_STD::string>());
        CHECK_EQUAL(*result3.value, "abcd");
        CHECK_EQUAL(result3.in, stringRange.end());
        auto result4 = RAH2_NS::ranges::fold_left_first_with_iter(
            stringRange, RAH2_STD::plus<RAH2_STD::string>());
        CHECK_EQUAL(*result4.value, "abcd");
        CHECK_EQUAL(result4.in, stringRange.end());

        testSuite.test_case("iter");
        auto result5 = RAH2_NS::ranges::fold_left_first_with_iter(
            intsRange.begin(), intsRange.end(), RAH2_STD::plus<int>());
        CHECK_EQUAL(*result5.value, 15);
        CHECK_EQUAL(result5.in, intsRange.end());

        testSuite.test_case("empty");
        RAH2_STD::vector<int> emptyVec;
        auto emptyRange = make_test_view_adapter<CS, Tag, Sized>(emptyVec);
        auto result6 = RAH2_NS::ranges::fold_left_first_with_iter(emptyRange, RAH2_STD::plus<int>());
        CHECK(result6.value == nullptr);
        CHECK_EQUAL(result6.in, emptyRange.end());
    }
    template <bool = true>
    void test_perf(char const* range_type)
    {
        constexpr auto vecSize = 1000000;
        RAH2_STD::vector<int> intsVec(1, vecSize);
        auto intsRange = make_test_view_adapter<CS, Tag, Sized>(intsVec);
        testSuite.test_case("perf");
        COMPARE_DURATION_TO_STD_RANGES_23(
            "fold_left_first_with_iter",
            range_type,
            [&]
            {
                for (size_t i = 0; i < 1000 * RELEASE_MULTIPLIER; ++i)
                {

                    const auto result = STD::fold_left_first_with_iter(
                        fwd(intsRange.begin()), intsRange.end(), RAH2_STD::plus<int>());
                    CHECK_EQUAL(*result.value, vecSize);
                    const auto result2 =
                        STD::fold_left_first_with_iter(intsRange, RAH2_STD::plus<int>());
                    CHECK_EQUAL(*result2.value, vecSize);
                }
            });
    }
    static constexpr bool do_test = true;
};
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

    foreach_range_combination<test_algo<test_fold_left_first_with_iter_>>();
}
