#pragma once

#include <rah2/ranges.hpp>
#include <rah2/algorithm.hpp>

#include <iostream>
#include <chrono>
#include <functional>
#include <set>

#ifdef RAH2_USE_EASTL
#include <EASTL/map.h>
#include <EASTL/vector.h>
#include <EASTL/string.h>
#else
#include <map>
#include <vector>
#include <string>
#endif

#ifndef TEST_LEVEL
#define TEST_LEVEL 1 // medium
#endif

#define STATIC_ASSERT(PRED) static_assert(PRED, #PRED)

struct TestSuite
{
    struct TestResult
    {
        bool success = true;
        size_t testCount = 0;
    };

    RAH2_STD::map<RAH2_STD::string, std::function<void()>> testMap;
    RAH2_STD::map<RAH2_STD::string, TestResult> testResult;
    RAH2_STD::map<RAH2_STD::string, std::set<RAH2_STD::pair<RAH2_STD::string, RAH2_STD::string>>> testCases;

    bool current_test_status = true;
    size_t test_count = 0;
    bool all_success = true;
    volatile size_t avoid_optim = 1;

    void addTest(RAH2_STD::string const& group, RAH2_STD::string const& name, std::function<void()> test)
    {
        testMap.emplace(group + " - " + name, std::move(test));
    }

    char const* currentTest = nullptr;
    void test_case(char const* testcase, char const* extra = "")
    {
        testCases[currentTest].emplace(testcase, extra);
    }

    void report() const
    {
        for (auto const& name_test : testMap)
        {
            auto& name = name_test.first;
            std::cout << name_test.first.c_str() << " : ";
            auto iter = testResult.find(name);
            if (iter != testResult.end())
            {
                auto& result = iter->second;
                if (!result.success)
                    std::cout << "FAILED" << std::endl;
                else if (result.testCount == 0)
                    std::cout << "NO TESTS" << std::endl;
                else
                    std::cout << "OK" << std::endl;
            }
            else
                std::cout << "MISSING";

            if (testCases.count(name) != 0u)
            {
                for (auto& caseName_extra : testCases.at(name))
                {
                    std::cout << " / " << caseName_extra.first.c_str();
                }
            }
            std::cout << std::endl;
        }
        std::cout << (all_success ? "Tests passed" : "Tests failed") << std::endl;
    }

    void run()
    {
        for (auto const& name_test : testMap)
        {
            auto& name = name_test.first;
            currentTest = name.c_str();
            auto& test = name_test.second;
            current_test_status = true;
            test_count = 0;
            try
            {
                test();
            }
            catch (...)
            {
                testResult[name].success = false;
                return;
            }
            testResult[name].success = current_test_status;
            testResult[name].testCount = test_count;
            all_success = all_success && current_test_status;
        }
    }
};

// #define TEST_DISPLAY_ALL
#define TEST_DISPLAY_FAILED
// #define TEST_DISPLAY_NONE

extern TestSuite testSuite;
inline void assert_impl(char const* file, int line, char const* condition, bool value)
{
    ++testSuite.test_count;
#if defined(TEST_DISPLAY_ALL)
    std::cout << file << ":" << line << " assert : " << condition << std::endl;
#endif
    if (value)
    {
#if defined(TEST_DISPLAY_ALL)
        std::cout << "OK" << std::endl;
#endif
    }
    else
    {
#if defined(TEST_DISPLAY_FAILED) and not defined(TEST_DISPLAY_ALL)
        std::cout << file << ":" << line << " assert : " << condition << std::endl;
#endif
#if defined(TEST_DISPLAY_FAILED)
        std::cout << "NOT OK (" << file << ":" << line << ")" << std::endl;
#endif
        // abort();
        testSuite.current_test_status = false;
    }
}

#undef assert
#define assert(CONDITION) assert_impl(__FILE__, __LINE__, #CONDITION, (CONDITION))

template <bool A, bool B>
struct AssertEqual;

template <bool X>
struct AssertEqual<X, X>
{
};

template <typename A, typename B>
struct AssertSame;

template <typename X>
struct AssertSame<X, X>
{
};

enum CommonOrSent
{
    Common,
    Sentinel
};

template <CommonOrSent Sent, typename Cat, bool SizedRange>
class test_view : public RAH2_NS::ranges::view_interface<test_view<Sent, Cat, SizedRange>>
{
    RAH2_STD::vector<int> vec;

public:
    test_view()
    {
        vec.reserve(103);
        for (int i = 0; i < 103; ++i)
        {
            vec.push_back(i);
        }
    }

    class iterator;
    struct sentinel
    {
    };

    using ref_type =
        RAH2_STD::conditional_t<RAH2_NS::is_same_v<Cat, RAH2_NS::output_iterator_tag>, int&, int const&>;

    class iterator : public RAH2_NS::ranges::iterator_facade<iterator, sentinel, ref_type, Cat>
    {
        RAH2_STD::vector<int>::iterator iter_;
        RAH2_STD::vector<int>::iterator end_;

    public:
        iterator() = default;

        template <
            typename C = Cat,
            RAH2_STD::enable_if_t<
                RAH2_NS::derived_from<C, RAH2_NS::input_iterator_tag>
                || RAH2_NS::derived_from<C, RAH2_NS::output_iterator_tag>>* = nullptr>
        iterator(iterator&& other)
            : iter_(other.iter_)
        {
        }

        template <
            typename C = Cat,
            RAH2_STD::enable_if_t<
                RAH2_NS::derived_from<C, RAH2_NS::input_iterator_tag>
                || RAH2_NS::derived_from<C, RAH2_NS::output_iterator_tag>>* = nullptr>
        iterator& operator=(iterator&& other)
        {
            iter_ = other.iter_;
        }

        template <
            typename C = Cat,
            RAH2_STD::enable_if_t<
                RAH2_NS::derived_from<C, RAH2_NS::forward_iterator_tag>
                || RAH2_NS::derived_from<C, RAH2_NS::output_iterator_tag>>* = nullptr>
        iterator(iterator const& other)
            : iter_(other.iter_)
        {
        }

        template <
            typename C = Cat,
            RAH2_STD::enable_if_t<
                RAH2_NS::derived_from<C, RAH2_NS::forward_iterator_tag>
                || RAH2_NS::derived_from<C, RAH2_NS::output_iterator_tag>>* = nullptr>
        iterator& operator=(iterator const& other)
        {
            iter_ = other.iter_;
        }
        iterator(RAH2_STD::vector<int>::iterator iter, RAH2_STD::vector<int>::iterator end)
            : iter_(iter)
            , end_(end)
        {
        }

        iterator& operator++()
        {
            ++iter_;
            return *this;
        }
        RAH2_POST_INCR(Cat)
        template <
            typename C = Cat,
            RAH2_STD::enable_if_t<RAH2_NS::derived_from<C, RAH2_NS::random_access_iterator_tag>>* = nullptr>
        iterator& operator+=(intptr_t value)
        {
            iter_ += value;
            return *this;
        }

        template <
            typename C = Cat,
            RAH2_STD::enable_if_t<RAH2_NS::derived_from<C, RAH2_NS::bidirectional_iterator_tag>>* = nullptr>
        iterator& operator--()
        {
            --iter_;
            return *this;
        }
        template <
            typename C = Cat,
            RAH2_STD::enable_if_t<RAH2_NS::derived_from<C, RAH2_NS::bidirectional_iterator_tag>>* = nullptr>
        RAH2_POST_DECR;
        template <
            typename C = Cat,
            RAH2_STD::enable_if_t<RAH2_NS::derived_from<C, RAH2_NS::random_access_iterator_tag>>* = nullptr>
        auto operator-(iterator const& other) const
        {
            return iter_ - other.iter_;
        }
        template <
            typename C = Cat,
            RAH2_STD::enable_if_t<RAH2_NS::derived_from<C, RAH2_NS::random_access_iterator_tag>>* = nullptr>
        iterator& operator-=(intptr_t value)
        {
            iter_ -= value;
            return *this;
        }
        ref_type operator*() const
        {
            return *iter_;
        }
        template <
            typename C = Cat,
            RAH2_STD::enable_if_t<RAH2_NS::derived_from<C, RAH2_NS::forward_iterator_tag>>* = nullptr>
        friend constexpr bool operator==(iterator const& it1, iterator const& it2)
        {
            return it1.iter_ == it2.iter_;
        }
        friend constexpr bool operator==(RAH2_NS::default_sentinel_t const&, iterator const& it)
        {
            return it.iter_ == it.end_;
        }
        friend constexpr bool operator==(iterator const& it, RAH2_NS::default_sentinel_t const&)
        {
            return it.iter_ == it.end_;
        }
        template <
            typename C = Cat,
            RAH2_STD::enable_if_t<RAH2_NS::derived_from<C, RAH2_NS::random_access_iterator_tag>>* = nullptr>
        friend bool operator<(iterator const& it1, iterator const& it2)
        {
            return it1.iter_ < it2.iter_;
        }
    };

    template <bool IsSized = SizedRange, RAH2_STD::enable_if_t<IsSized>* = nullptr>
    auto size() const
    {
        return vec.size();
    }

    auto begin()
    {
        return iterator(vec.begin(), vec.end());
    }
    template <CommonOrSent S = Sent, RAH2_STD::enable_if_t<S == Sentinel>* = nullptr>
    auto end()
    {
        return RAH2_NS::default_sentinel_t{};
    }
    template <CommonOrSent S = Sent, RAH2_STD::enable_if_t<S == Common>* = nullptr>
    auto end()
    {
        return iterator(vec.end(), vec.end());
    }
    template <
        typename C = Cat,
        RAH2_STD::enable_if_t<RAH2_NS::is_same_v<C, RAH2_NS::contiguous_iterator_tag>>* = nullptr>
    RAH2_STD::remove_reference_t<ref_type>* data()
    {
        return vec.data();
    }

    template <
        typename C = Cat,
        RAH2_STD::enable_if_t<RAH2_NS::is_same_v<C, RAH2_NS::contiguous_iterator_tag>>* = nullptr>
    RAH2_STD::remove_reference_t<ref_type>* data() const
    {
        return vec.data();
    }
};

template <CommonOrSent Sent, typename Cat, bool SizedRange, typename Range>
class test_view_adapter : public RAH2_NS::ranges::view_interface<test_view<Sent, Cat, SizedRange>>
{
    Range base_;
    using base_iterator = RAH2_NS::ranges::iterator_t<Range>;
    using base_sentinel = RAH2_NS::ranges::sentinel_t<Range>;
    using ref = RAH2_NS::ranges::range_reference_t<Range>;

public:
    class iterator
        : public RAH2_NS::ranges::iterator_facade<iterator, RAH2_NS::ranges::sentinel_iterator, ref, Cat>
    {
        base_iterator iter_;
        base_iterator end_;

    public:
        iterator() = default;
        explicit iterator(base_iterator it, base_iterator end)
            : iter_(it)
            , end_(end)
        {
        }

        iterator& operator++()
        {
            ++iter_;
            return *this;
        }
        RAH2_POST_INCR(Cat)
        template <
            typename C = Cat,
            RAH2_STD::enable_if_t<RAH2_NS::derived_from<C, RAH2_NS::random_access_iterator_tag>>* = nullptr>
        iterator& operator+=(intptr_t value)
        {
            iter_ += value;
            return *this;
        }

        template <
            typename C = Cat,
            RAH2_STD::enable_if_t<RAH2_NS::derived_from<C, RAH2_NS::bidirectional_iterator_tag>>* = nullptr>
        iterator& operator--()
        {
            --iter_;
            return *this;
        }
        template <
            typename C = Cat,
            RAH2_STD::enable_if_t<RAH2_NS::derived_from<C, RAH2_NS::bidirectional_iterator_tag>>* = nullptr>
        RAH2_POST_DECR;
        template <
            typename C = Cat,
            RAH2_STD::enable_if_t<RAH2_NS::derived_from<C, RAH2_NS::random_access_iterator_tag>>* = nullptr>
        auto operator-(iterator const& other) const
        {
            return iter_ - other.iter_;
        }
        template <
            typename C = Cat,
            RAH2_STD::enable_if_t<RAH2_NS::derived_from<C, RAH2_NS::random_access_iterator_tag>>* = nullptr>
        iterator& operator-=(intptr_t value)
        {
            iter_ -= value;
            return *this;
        }
        RAH2_NS::iter_reference_t<base_iterator> operator*() const
        {
            return *iter_;
        }
        RAH2_NS::iter_reference_t<base_iterator> operator*()
        {
            return *iter_;
        }
        template <
            typename C = Cat,
            RAH2_STD::enable_if_t<RAH2_NS::derived_from<C, RAH2_STD::forward_iterator_tag>>* = nullptr>
        friend constexpr bool operator==(iterator const& it1, iterator const& it2)
        {
            return it1.iter_ == it2.iter_;
        }
        friend constexpr bool operator==(RAH2_NS::default_sentinel_t const&, iterator const& it)
        {
            return it.iter_ == it.end_;
        }
        friend constexpr bool operator==(iterator const& it, RAH2_NS::default_sentinel_t const&)
        {
            return it.iter_ == it.end_;
        }
        template <
            typename C = Cat,
            RAH2_STD::enable_if_t<RAH2_NS::derived_from<C, RAH2_NS::random_access_iterator_tag>>* = nullptr>
        friend bool operator<(iterator const& it1, iterator const& it2)
        {
            return it1.iter_ < it2.iter_;
        }
    };

    test_view_adapter() = default;
    explicit test_view_adapter(Range r)
        : base_(std::move(r))
    {
    }

    auto begin()
    {
        return iterator(RAH2_NS::ranges::begin(base_), RAH2_NS::ranges::end(base_));
    }
    template <CommonOrSent S = Sent, RAH2_STD::enable_if_t<S == Sentinel>* = nullptr>
    auto end()
    {
        return RAH2_NS::default_sentinel_t{};
    }
    template <CommonOrSent S = Sent, RAH2_STD::enable_if_t<S == Common>* = nullptr>
    auto end()
    {
        return iterator(RAH2_NS::ranges::end(base_), RAH2_NS::ranges::end(base_));
    }
    template <
        typename C = Cat,
        RAH2_STD::enable_if_t<RAH2_NS::is_same_v<C, RAH2_NS::contiguous_iterator_tag>>* = nullptr>
    RAH2_STD::remove_reference_t<ref>* data()
    {
        return RAH2_NS::ranges::data(base_);
    }

    template <
        typename C = Cat,
        RAH2_STD::enable_if_t<RAH2_NS::is_same_v<C, RAH2_NS::contiguous_iterator_tag>>* = nullptr>
    RAH2_STD::remove_reference_t<ref>* data() const
    {
        return RAH2_NS::ranges::data(base_);
    }
};

namespace RAH2_NS
{
    namespace ranges
    {
        template <CommonOrSent Sent, typename Cat, bool SizedRange, typename Range>
        constexpr bool enable_borrowed_range<test_view_adapter<Sent, Cat, SizedRange, Range>> =
            enable_borrowed_range<Range>;
    }

} // namespace RAH2_NS

template <CommonOrSent Sent, typename Cat, bool Sized>
auto make_test_view()
{
    return test_view<Sent, Cat, Sized>();
}

template <CommonOrSent Sent, typename Cat, bool Sized, typename Range>
auto make_test_view_adapter(Range&& r)
{
    return test_view_adapter<Sent, Cat, Sized, RAH2_NS::views::all_t<Range>>(RAH2_NS::views::all(r));
}

// output
using OutputCommonView = test_view<Common, RAH2_NS::output_iterator_tag, false>;
static_assert(
    RAH2_NS::input_or_output_iterator<RAH2_NS::ranges::iterator_t<OutputCommonView>>,
    "Should be input");
static_assert(not RAH2_NS::ranges::input_range<OutputCommonView>, "Should be input");
static_assert(RAH2_NS::ranges::output_range<OutputCommonView, int>, "Should be output");
static_assert(not RAH2_NS::input_or_output_iterator<OutputCommonView>, "Should be input");
static_assert(not RAH2_NS::ranges::forward_range<OutputCommonView>, "Should be Forward");
static_assert(not RAH2_NS::ranges::bidirectional_range<OutputCommonView>, "Should not be bidirectional");
static_assert(not RAH2_NS::ranges::random_access_range<OutputCommonView>, "Should not be random");
static_assert(not RAH2_NS::ranges::contiguous_range<OutputCommonView>, "Should not be contiguous");

// input
using InputCommonView = test_view<Common, RAH2_NS::input_iterator_tag, false>;
static_assert(
    RAH2_NS::input_or_output_iterator<RAH2_NS::ranges::iterator_t<InputCommonView>>,
    "Should be input");
static_assert(RAH2_NS::ranges::input_range<InputCommonView>, "Should be input");
static_assert(not RAH2_NS::ranges::output_range<InputCommonView, int>, "Should not be output");
static_assert(not RAH2_NS::ranges::forward_range<InputCommonView>, "Should be Forward");
static_assert(not RAH2_NS::ranges::bidirectional_range<InputCommonView>, "Should not be bidirectional");
static_assert(not RAH2_NS::ranges::random_access_range<InputCommonView>, "Should not be random");
static_assert(not RAH2_NS::ranges::contiguous_range<InputCommonView>, "Should not be contiguous");

// Forward
using ForwardCommonView = test_view<Common, RAH2_NS::forward_iterator_tag, false>;
static_assert(
    RAH2_NS::input_or_output_iterator<RAH2_NS::ranges::iterator_t<ForwardCommonView>>,
    "Should be input");
static_assert(RAH2_NS::ranges::input_range<ForwardCommonView>, "Should be input");
static_assert(not RAH2_NS::ranges::output_range<ForwardCommonView, int>, "Should not be output");
static_assert(
    RAH2_NS::ranges::details::forward_range_impl<ForwardCommonView, true>::value,
    "Should be Forward");
static_assert(!RAH2_NS::ranges::bidirectional_range<ForwardCommonView>, "Should not be bidirectional");
static_assert(!RAH2_NS::ranges::random_access_range<ForwardCommonView>, "Should not be random");
static_assert(not RAH2_NS::ranges::contiguous_range<ForwardCommonView>, "Should not be contiguous");

// bidirectional
using BidirCommonView = test_view<Common, RAH2_NS::bidirectional_iterator_tag, false>;
static_assert(
    RAH2_NS::input_or_output_iterator<RAH2_NS::ranges::iterator_t<BidirCommonView>>,
    "Should be input");
static_assert(RAH2_NS::ranges::input_range<BidirCommonView>, "Should be input");
static_assert(not RAH2_NS::ranges::output_range<BidirCommonView, int>, "Should not be outnput");
static_assert(RAH2_NS::ranges::forward_range<BidirCommonView>, "Should be Forward");
STATIC_ASSERT((RAH2_NS::ranges::details::bidirectional_range_impl<BidirCommonView, true>::value));
static_assert(!RAH2_NS::ranges::random_access_range<BidirCommonView>, "Should not be random");
static_assert(not RAH2_NS::ranges::contiguous_range<BidirCommonView>, "Should not be contiguous");

// random access
using RandomCommonView = test_view<Common, RAH2_NS::random_access_iterator_tag, true>;
static_assert(
    RAH2_NS::input_or_output_iterator<RAH2_NS::ranges::iterator_t<RandomCommonView>>,
    "Should be input");
static_assert(RAH2_NS::ranges::input_range<RandomCommonView>, "Should be input");
static_assert(not RAH2_NS::ranges::output_range<RandomCommonView, int>, "Should not be output");
static_assert(RAH2_NS::ranges::forward_range<RandomCommonView>, "Should be Forward");
using It = RAH2_NS::ranges::iterator_t<RandomCommonView>;
using cat = RAH2_NS::ranges::details::range_iter_categ_t<RandomCommonView>;
static_assert(RAH2_NS::forward_iterator<It>, "");
static_assert(RAH2_NS::is_same_v<decltype(--std::declval<It>()), It&>, "");
STATIC_ASSERT((RAH2_NS::ranges::details::bidirectional_range_impl<RandomCommonView, true>::value));
static_assert(RAH2_NS::ranges::bidirectional_range<RandomCommonView>, "Should be bidirectional");
using RandomCommonViewIter = RAH2_NS::ranges::iterator_t<RandomCommonView>;
STATIC_ASSERT((RAH2_NS::ranges::details::random_access_range_impl<RandomCommonView, true>::value));
static_assert(not RAH2_NS::ranges::contiguous_range<RandomCommonView>, "Should not be contiguous");

// contiguous
using ContiCommonView = test_view<Common, RAH2_NS::contiguous_iterator_tag, true>;
static_assert(
    RAH2_NS::input_or_output_iterator<RAH2_NS::ranges::iterator_t<ContiCommonView>>,
    "Should be input");
static_assert(RAH2_NS::ranges::input_range<ContiCommonView>, "Should be input");
static_assert(not RAH2_NS::ranges::output_range<ContiCommonView, int>, "Should not be output");
static_assert(RAH2_NS::ranges::forward_range<ContiCommonView>, "Should be Forward");
static_assert(RAH2_NS::ranges::bidirectional_range<ContiCommonView>, "Should be bidirectional");
static_assert(RAH2_NS::ranges::random_access_range<ContiCommonView>, "Should be random");
STATIC_ASSERT((RAH2_NS::ranges::details::contiguous_range_impl<ContiCommonView, true>::value));
static_assert(RAH2_NS::ranges::contiguous_range<ContiCommonView>, "Should be contiguous");

template <typename R>
constexpr bool is_input_common =
    RAH2_NS::ranges::input_range<R> && not RAH2_NS::ranges::forward_range<R>
    && RAH2_NS::ranges::common_range<R>;

template <typename R>
constexpr bool is_input_not_common =
    RAH2_NS::ranges::input_range<R> && not RAH2_NS::ranges::forward_range<R>
    && not RAH2_NS::ranges::common_range<R>;

template <typename R>
constexpr bool is_forward_common =
    RAH2_NS::ranges::forward_range<R> && not RAH2_NS::ranges::bidirectional_range<R>
    && RAH2_NS::ranges::common_range<R>;

template <typename R>
constexpr bool is_forward_not_common =
    RAH2_NS::ranges::forward_range<R> && not RAH2_NS::ranges::bidirectional_range<R>
    && not RAH2_NS::ranges::common_range<R>;

template <typename R>
constexpr bool is_bidirectional_common =
    RAH2_NS::ranges::bidirectional_range<R> && not RAH2_NS::ranges::random_access_range<R>
    && RAH2_NS::ranges::common_range<R>;

template <typename R>
constexpr bool is_bidirectional_not_common =
    RAH2_NS::ranges::bidirectional_range<R> && not RAH2_NS::ranges::random_access_range<R>
    && not RAH2_NS::ranges::common_range<R>;

template <typename R>
constexpr bool is_random_access_common =
    RAH2_NS::ranges::random_access_range<R> && not RAH2_NS::ranges::contiguous_range<R>
    && RAH2_NS::ranges::common_range<R>;

template <typename R>
constexpr bool is_random_access_not_common =
    RAH2_NS::ranges::random_access_range<R> && not RAH2_NS::ranges::contiguous_range<R>
    && not RAH2_NS::ranges::common_range<R>;

template <typename R>
constexpr bool is_contiguous_common =
    RAH2_NS::ranges::contiguous_range<R> && RAH2_NS::ranges::common_range<R>;

template <typename R>
constexpr bool is_contiguous_not_common =
    RAH2_NS::ranges::contiguous_range<R> && not RAH2_NS::ranges::common_range<R>;

template <typename C, typename I>
struct check_iterator_cat;

template <typename I>
struct check_iterator_cat<RAH2_STD::input_iterator_tag, I>
{
    STATIC_ASSERT(RAH2_NS::input_iterator<I>);
    STATIC_ASSERT(
        (RAH2_NS::is_same_v<RAH2_NS::details::iterator_concept<I>, RAH2_NS::input_iterator_tag>));
    STATIC_ASSERT(not RAH2_NS::forward_iterator<I>);
};

template <typename I>
struct check_iterator_cat<RAH2_STD::forward_iterator_tag, I>
{
    STATIC_ASSERT(RAH2_NS::forward_iterator<I>);
    STATIC_ASSERT(
        (RAH2_NS::is_same_v<RAH2_NS::details::iterator_concept<I>, RAH2_NS::forward_iterator_tag>));
    STATIC_ASSERT(not RAH2_NS::bidirectional_iterator<I>);
};

template <typename I>
struct check_iterator_cat<RAH2_NS::bidirectional_iterator_tag, I>
{
    STATIC_ASSERT(RAH2_NS::bidirectional_iterator<I>);
    STATIC_ASSERT((
        RAH2_NS::is_same_v<RAH2_NS::details::iterator_concept<I>, RAH2_NS::bidirectional_iterator_tag>));
    STATIC_ASSERT(not RAH2_NS::random_access_iterator<I>);
};

template <typename I>
struct check_iterator_cat<RAH2_NS::random_access_iterator_tag, I>
{
    STATIC_ASSERT(RAH2_NS::random_access_iterator<I>);
    STATIC_ASSERT((
        RAH2_NS::is_same_v<RAH2_NS::details::iterator_concept<I>, RAH2_NS::random_access_iterator_tag>));
    STATIC_ASSERT(not RAH2_NS::contiguous_iterator<I>);
};

template <typename I>
struct check_iterator_cat<RAH2_NS::contiguous_iterator_tag, I>
{
    STATIC_ASSERT(RAH2_NS::contiguous_iterator<I>);
    STATIC_ASSERT((
        RAH2_NS::is_same_v<RAH2_NS::details::iterator_concept<I>, RAH2_NS::contiguous_iterator_tag>));
};

template <bool sized>
struct check_size
{
    template <typename S, typename R>
    static void check(S size, R&& range)
    {
        if (size != S(RAH2_NS::ranges::size(range)))
        {
            std::cout << "real size :" << size
                      << " - expected size:" << RAH2_NS::ranges::size(range) << std::endl;
            assert(size == S(RAH2_NS::ranges::size(range)));
        }
    }
    static intptr_t get_stop_iter()
    {
        return std::numeric_limits<intptr_t>::max();
    }
};

template <>
struct check_size<false>
{
    template <typename S, typename R>
    static void check(S, R&&)
    {
    }
    static intptr_t get_stop_iter()
    {
        return 100;
    }
};

template <typename C, typename R>
struct check_range_cat;

template <typename R>
struct check_range_cat<RAH2_STD::input_iterator_tag, R>
{
    static void test(R& r)
    {
        STATIC_ASSERT((RAH2_NS::details::sentinel_for_impl<
                       RAH2_NS::ranges::sentinel_t<R>,
                       RAH2_NS::ranges::iterator_t<R>,
                       true>::value));
        auto e = RAH2_NS::ranges::end(r);
        assert(not RAH2_NS::ranges::empty(r));
        intptr_t const max_iter = check_size<RAH2_NS::ranges::sized_range<R>>::get_stop_iter();
        intptr_t counter = 0;
        for (auto i = RAH2_NS::ranges::begin(r); i != e && counter != max_iter; ++i, ++counter)
        {
            (void)*i;
        }
        check_size<RAH2_NS::ranges::sized_range<R>>::check(counter, r);
        STATIC_ASSERT(RAH2_NS::ranges::input_range<R>);
        AssertSame<RAH2_NS::ranges::details::range_iter_categ_t<R>, RAH2_NS::input_iterator_tag>();
        STATIC_ASSERT(not RAH2_NS::ranges::forward_range<R>);
    }
};

template <typename R>
struct check_range_cat<RAH2_STD::forward_iterator_tag, R>
{
    static void test(R& r)
    {
        STATIC_ASSERT((RAH2_NS::details::sentinel_for_impl<
                       RAH2_NS::ranges::sentinel_t<R>,
                       RAH2_NS::ranges::iterator_t<R>,
                       true>::value));
        auto e = RAH2_NS::ranges::end(r);
        assert(not RAH2_NS::ranges::empty(r));
        auto i = RAH2_NS::ranges::begin(r);
        auto u = i;
        assert(u == i);
        ++u;
        assert(u != i);
        intptr_t const max_iter = check_size<RAH2_NS::ranges::sized_range<R>>::get_stop_iter();
        intptr_t counter = 0;
        for (; i != e && counter != max_iter; ++i, ++counter)
        {
            (void)*i;
        }
        check_size<RAH2_NS::ranges::sized_range<R>>::check(counter, r);
        AssertSame<RAH2_NS::ranges::details::range_iter_categ_t<R>, RAH2_NS::forward_iterator_tag>();
        STATIC_ASSERT(
            (RAH2_NS::details::forward_iterator_impl<RAH2_NS::ranges::iterator_t<R>, true>::value));
        STATIC_ASSERT(RAH2_NS::ranges::forward_range<R>);
        STATIC_ASSERT(not RAH2_NS::ranges::bidirectional_range<R>);
    }
};

template <typename R>
struct check_range_cat<RAH2_NS::bidirectional_iterator_tag, R>
{
    static void test(R& r)
    {
        STATIC_ASSERT((RAH2_NS::details::sentinel_for_impl<
                       RAH2_NS::ranges::sentinel_t<R>,
                       RAH2_NS::ranges::iterator_t<R>,
                       true>::value));
        auto e = RAH2_NS::ranges::end(r);
        assert(not RAH2_NS::ranges::empty(r));
        auto i = RAH2_NS::ranges::begin(r);
        auto u = i;
        assert(u == i);
        intptr_t const max_iter = check_size<RAH2_NS::ranges::sized_range<R>>::get_stop_iter();
        intptr_t counter = 0;
        for (; i != e && counter != max_iter; ++i, ++counter)
        {
            (void)*i;
        }
        check_size<RAH2_NS::ranges::sized_range<R>>::check(counter, r);
        for (; i != RAH2_NS::ranges::begin(r); --i, --counter)
        {
            assert(counter >= 0);
            if (counter == 0)
            {
                assert(i == RAH2_NS::ranges::begin(r));
            }
        }
        STATIC_ASSERT((RAH2_NS::ranges::details::bidirectional_range_impl<R, true>::value));
        AssertSame<RAH2_NS::ranges::details::range_iter_categ_t<R>, RAH2_NS::bidirectional_iterator_tag>();
        STATIC_ASSERT(not RAH2_NS::ranges::random_access_range<R>);
    }
};

template <typename R>
struct check_range_cat<RAH2_NS::random_access_iterator_tag, R>
{
    static void test(R& r)
    {
        STATIC_ASSERT((RAH2_NS::details::sentinel_for_impl<
                       RAH2_NS::ranges::sentinel_t<R>,
                       RAH2_NS::ranges::iterator_t<R>,
                       true>::value));
        auto e = RAH2_NS::ranges::end(r);
        assert(not RAH2_NS::ranges::empty(r));
        auto i = RAH2_NS::ranges::begin(r);
        auto u = i;
        u += 2;
        decltype(i[2]) ref = i[2];
        AssertSame<decltype(ref), RAH2_NS::ranges::range_reference_t<R>>();
        assert(u > i);
        assert(not(u < i));
        assert(u != i);
        assert(not(u == i));
        assert((u - i) == 2);
        intptr_t const max_iter = check_size<RAH2_NS::ranges::sized_range<R>>::get_stop_iter();
        intptr_t counter = 0;
        for (; i != e && counter != max_iter; ++i, ++counter)
        {
            (void)*i;
        }
        check_size<RAH2_NS::ranges::sized_range<R>>::check(counter, r);
        for (; i != RAH2_NS::ranges::begin(r); --i, --counter)
        {
            assert(counter >= 0);
            assert(i >= RAH2_NS::ranges::begin(r));
        }
        STATIC_ASSERT((RAH2_NS::ranges::details::random_access_range_impl<R, true>::value));
        AssertSame<RAH2_NS::ranges::details::range_iter_categ_t<R>, RAH2_NS::random_access_iterator_tag>();
        STATIC_ASSERT(not RAH2_NS::ranges::contiguous_range<R>);
    }
};

template <typename R>
struct check_range_cat<RAH2_NS::contiguous_iterator_tag, R>
{
    static void test(R& r)
    {
        STATIC_ASSERT((RAH2_NS::details::sentinel_for_impl<
                       RAH2_NS::ranges::sentinel_t<R>,
                       RAH2_NS::ranges::iterator_t<R>,
                       true>::value));
        auto e = RAH2_NS::ranges::end(r);
        assert(not RAH2_NS::ranges::empty(r));
        auto i = RAH2_NS::ranges::begin(r);
        auto u = i;
        u += 2;
        assert(&(i[2]) == &(*u));
        auto& ref = i[2];
        AssertSame<decltype(ref), RAH2_NS::ranges::range_reference_t<R>>();
        assert(!(u < i));
        assert(i < u);
        assert((u - i) == 2);
        intptr_t const max_iter = check_size<RAH2_NS::ranges::sized_range<R>>::get_stop_iter();
        intptr_t counter = 0;
        for (; i != e && counter != max_iter; ++i, ++counter)
        {
            (void)*i;
        }
        check_size<RAH2_NS::ranges::sized_range<R>>::check(counter, r);
        for (; i != RAH2_NS::ranges::begin(r); --i, --counter)
        {
            assert(counter >= 0);
            assert(i >= RAH2_NS::ranges::begin(r));
        }
        auto* d = r.data();
        auto* d2 = &(*r.begin());
        assert(d == d2);
        assert(d == &(*i));
        AssertSame<RAH2_NS::ranges::details::range_iter_categ_t<R>, RAH2_NS::contiguous_iterator_tag>();
        STATIC_ASSERT((RAH2_NS::ranges::details::contiguous_range_impl<R, true>::value));
    }
};

template <
    bool DoTest,
    CommonOrSent Sentinel,
    typename Cat,
    bool Sized,
    template <CommonOrSent, typename, bool>
    class MakeR,
    typename Check>
struct call_on_range_if_true
{
    static void test(char const* range_type)
    {
        Check{}.template call<Sentinel, Cat, Sized, MakeR>(range_type);
    }
};

template <CommonOrSent Sentinel, typename Cat, bool Sized, template <CommonOrSent, typename, bool> class MakeR, typename Check>
struct call_on_range_if_true<false, Sentinel, Cat, Sized, MakeR, Check>
{
    static void test(char const*)
    {
    }
};

template <class Func>
void foreach_range_combination()
{
#if TEST_LEVEL == 0
    Func{}.template call<Common, RAH2_NS::random_access_iterator_tag, true>(R"(common_RA_sized)");
#else
    Func{}.template call<Sentinel, RAH2_STD::input_iterator_tag, false>("sent_input_unsized");
    Func{}.template call<Sentinel, RAH2_STD::forward_iterator_tag, false>("sent_forward_unsized");
    Func{}.template call<Sentinel, RAH2_NS::bidirectional_iterator_tag, false>(
        R"(sent_bidir_unsized)");
    Func{}.template call<Sentinel, RAH2_NS::random_access_iterator_tag, false>(R"(sent_RA_unsized)");
    Func{}.template call<Sentinel, RAH2_NS::contiguous_iterator_tag, false>(R"(sent_conti_unsized)");

    Func{}.template call<Common, RAH2_NS::forward_iterator_tag, false>(R"(common_forward_unsized)");
    Func{}.template call<Common, RAH2_NS::bidirectional_iterator_tag, false>(
        R"(common_bidir_unsized)");
    // Common random_access can't be not sized
    // Func{}.template call<Common, AH2_NS::random_access_iterator_tag, false>();
    // Func{}.template call<Common, RAH2_NS::contiguous_iterator_tag, false>();

    Func{}.template call<Sentinel, RAH2_NS::input_iterator_tag, true>(R"(sent_input_sized)");
    Func{}.template call<Sentinel, RAH2_NS::forward_iterator_tag, true>(R"(sent_forward_sized)");
    Func{}.template call<Sentinel, RAH2_NS::bidirectional_iterator_tag, true>(R"(sent_bidir_sized)");
    Func{}.template call<Sentinel, RAH2_NS::random_access_iterator_tag, true>(R"(sent_RA_sized)");
    Func{}.template call<Sentinel, RAH2_NS::contiguous_iterator_tag, true>(R"(sent_conti_sized)");

    Func{}.template call<Common, RAH2_NS::forward_iterator_tag, true>(R"(common_fwd_sized)");
    Func{}.template call<Common, RAH2_NS::bidirectional_iterator_tag, true>(R"(common_bidir_sized)");
    Func{}.template call<Common, RAH2_NS::random_access_iterator_tag, true>(R"(common_RA_sized)");
    Func{}.template call<Common, RAH2_NS::contiguous_iterator_tag, true>(R"(common_conti_sized)");
#endif
}

template <template <CommonOrSent, typename, bool> class MakeRange>
struct test_range
{
    struct CheckView
    {
        template <CommonOrSent Sentinel, typename Cat, bool Sized, template <CommonOrSent, typename, bool> class MakeR>
        void call(char const*) const
        {
            auto t1 = MakeR<Sentinel, Cat, Sized>();
            auto r1 = t1.make();
            using ExpectedCat = typename MakeR<Sentinel, Cat, Sized>::expected_cat;
            check_range_cat<ExpectedCat, RAH2_STD::remove_reference_t<decltype(r1)>>::test(r1);
            AssertEqual<RAH2_NS::ranges::common_range<decltype(r1)>, t1.is_common>();
            AssertEqual<RAH2_NS::ranges::sized_range<decltype(r1)>, t1.is_sized>();
            AssertEqual<RAH2_NS::ranges::borrowed_range<decltype(r1)>, t1.is_borrowed>();
        }
    };
    template <CommonOrSent Sentinel, typename Cat, bool Sized>
    void call(char const* range_type)
    {
        constexpr bool do_test = MakeRange<Sentinel, Cat, Sized>::do_test;
        call_on_range_if_true<do_test, Sentinel, Cat, Sized, MakeRange, CheckView>::test(range_type);
    }
};

template <template <CommonOrSent, typename, bool> class MakeRange>
struct test_algo
{
    struct CheckAlgo
    {
        template <CommonOrSent Sentinel, typename Cat, bool Sized, template <CommonOrSent, typename, bool> class MakeR>
        void call(char const* range_type) const
        {
            auto t1 = MakeR<Sentinel, Cat, Sized>();
            t1.template test<>(range_type);
        }
    };
    template <CommonOrSent Sentinel, typename Cat, bool Sized>
    void call(char const* range_type)
    {
        constexpr bool do_test = MakeRange<Sentinel, Cat, Sized>::do_test;
        call_on_range_if_true<do_test, Sentinel, Cat, Sized, MakeRange, CheckAlgo>::test(range_type);
    }
};

template <template <CommonOrSent, typename, bool> class MakeRange>
struct test_range2 // When the adaptor take two ranges
{
    template <CommonOrSent Sentinel, typename Cat, bool Sized>
    void call(char const*)
    {
        foreach_range_combination<test_range<MakeRange<Sentinel, Cat, Sized>::template type>>();
    }
};

template <template <CommonOrSent, typename, bool> class MakeRange>
struct test_iterator
{
    struct CheckIterator
    {
        template <CommonOrSent Sentinel, typename Cat, bool Sized, template <CommonOrSent, typename, bool> class MakeIter>
        void call(char const*) const
        {
            using TestTrait = MakeIter<Sentinel, Cat, Sized>;
            auto maker = TestTrait();
            auto i1 = maker.make();
            using ExpectedCat = typename TestTrait::expected_cat;
            check_iterator_cat<ExpectedCat, RAH2_STD::remove_reference_t<decltype(i1)>>();
        }
    };
    template <CommonOrSent Sentinel, typename Cat, bool Sized>
    void call(char const* range_type)
    {
        constexpr bool do_test = MakeRange<Sentinel, Cat, Sized>::do_test;
        call_on_range_if_true<do_test, Sentinel, Cat, Sized, MakeRange, CheckIterator>::test(
            range_type);
    }
};

template <typename A, typename B, typename C, typename D>
bool operator==(RAH2_STD::tuple<A, B> a, RAH2_STD::pair<D, C> b)
{
    return RAH2_STD::get<0>(a) == RAH2_STD::get<0>(b) && RAH2_STD::get<1>(a) == RAH2_STD::get<1>(b);
}

template <typename A, typename B, typename C, typename D>
bool operator==(RAH2_STD::pair<A, B> a, RAH2_STD::tuple<D, C> b)
{
    return RAH2_STD::get<0>(a) == RAH2_STD::get<0>(b) && RAH2_STD::get<1>(a) == RAH2_STD::get<1>(b);
}

struct PairEqualImpl
{
    template <typename P>
    auto operator()(P&& ab)
    {
        static_assert(
            RAH2_NS::details::weakly_equality_comparable_with<
                decltype(RAH2_STD::get<0>(ab)),
                decltype(RAH2_STD::get<1>(ab))>,
            "second not assignable to first");
        return RAH2_STD::get<0>(ab) == RAH2_STD::get<1>(ab);
    }
};

static constexpr PairEqualImpl PairEqual;

template <typename R, typename I>
void equalRange(R&& RANGE, I&& IL, char const* rangeName, char const* ILName)
{
    ++testSuite.test_count;

    static_assert(
        RAH2_NS::details::weakly_equality_comparable_with<
            RAH2_NS::ranges::range_reference_t<decltype(RANGE)>,
            RAH2_NS::ranges::range_reference_t<decltype(IL)>>,
        "Can't compare");
#if defined(TEST_DISPLAY_ALL)
    std::cout << "assert : " << rangeName << " == " << ILName << std::endl;
#endif
    if (RAH2_NS::ranges::all_of(
            RAH2_NS::views::zip(RAH2_STD::forward<R>(RANGE), RAH2_STD::forward<I>(IL)), PairEqual))
    {
#if defined(TEST_DISPLAY_ALL)
        std::cout << "OK" << std::endl;
#endif
    }
    else
    {
#if defined(TEST_DISPLAY_FAILED) and not defined(TEST_DISPLAY_ALL)
        std::cout << "assert : " << rangeName << " == " << ILName << std::endl;
#endif
#if defined(TEST_DISPLAY_FAILED)
        std::cout << "NOT OK" << std::endl;
#endif
        testSuite.current_test_status = false;
        // abort();
    }
}

#define EQUAL_RANGE(RANGE, IL) equalRange(RANGE, IL, #RANGE, #IL)

template <typename T>
using il = std::initializer_list<T>;

template <typename... Args>
std::ostream& operator<<(std::ostream& os, RAH2_STD::tuple<Args...> tup)
{
    auto print_elt = [](auto&& elt)
    {
        (std::cout << RAH2_STD::forward<decltype(elt)>(elt)) << " ";
    };

    RAH2_NS::ranges::details::for_each(tup, print_elt);
    return os;
}

template <typename T>
struct WhatIs;

struct Fwd
{
    template <typename T>
    T&& operator()(T&& arg) const
    {
        return RAH2_STD::forward<T>(arg);
    }
};

template <bool DoCall, typename Func, RAH2_STD::enable_if_t<DoCall>* = nullptr>
void call_if_true(Func&& f)
{
    f(Fwd{});
}

template <bool DoCall, typename Func, RAH2_STD::enable_if_t<not DoCall>* = nullptr>
void call_if_true(Func&&)
{
}

#ifdef NDEBUG
#define RELEASE_MULTIPLIER 50
#else
#define RELEASE_MULTIPLIER 1
#endif

static auto perf_test_duration = std::chrono::seconds(1);

template <typename F>
std::chrono::nanoseconds compute_duration(
    F&& func,
    char const* algo,
    char const* range_type,
    char const* step, // ref or test
    char const* file,
    int line)
{
#ifdef PERF_TEST
    size_t count = 0;
    const auto start = std::chrono::high_resolution_clock::now();
    auto end = start;

    func();
    ++count;
    end = std::chrono::high_resolution_clock::now();
    if ((end - start) < std::chrono::milliseconds(1))
    {
        std::cerr << "Too short function at " << file << "(" << line << ")" << std::endl;
    }
    if ((end - start) > perf_test_duration)
    {
        std::cerr << "Too long function at " << file << "(" << line << ")" << std::endl;
    }

    while ((end - start) < perf_test_duration)
    {
        func();
        ++count;
        end = std::chrono::high_resolution_clock::now();
    }
    std::cout << algo << "/" << range_type << "/" << step << " : "
              << ((end - start) / count).count() << std::endl;
    return (end - start) / count;
#else
    (void)algo;
    (void)range_type;
    (void)step;
    (void)func;
    (void)file;
    (void)line;
    return std::chrono::seconds(1);
#endif
}

template <typename F, typename F2>
auto compare_duration(
    F&& func_std_range,
    F2&& func_rah2,
    char const* algo,
    char const* range_type,
    char const* ref_ns,
    char const* file,
    int line)
{
    auto duration_std =
        compute_duration(RAH2_STD::forward<F>(func_std_range), algo, range_type, ref_ns, file, line);
    auto duration_rah2 =
        compute_duration(RAH2_STD::forward<F2>(func_rah2), algo, range_type, "rah2", file, line);
    assert(duration_rah2 < (duration_std * 1.2));
}

template <typename F, typename F2, typename F3>
auto compare_duration(
    F&& func_std_algo,
    F2&& func_std_range,
    F3&& func_rah2,
    char const* algo,
    char const* range_type,
    char const* file,
    int line)
{
    auto duration_std =
        compute_duration(RAH2_STD::forward<F>(func_std_algo), algo, range_type, "std", file, line);
    auto duration_std_ranges = compute_duration(
        RAH2_STD::forward<F2>(func_std_range), algo, range_type, "std::ranges", file, line);
    auto duration_rah2 =
        compute_duration(RAH2_STD::forward<F3>(func_rah2), algo, range_type, "rah2", file, line);
    assert(duration_rah2 < (duration_std * 1.2));
    assert(duration_rah2 < (duration_std_ranges * 1.2));
}

#define COMPUTE_DURATION(ALGO, CONCEPT, STEP, F)                                                   \
    compute_duration((F), ALGO, CONCEPT, STEP, __FILE__, __LINE__)

#if RAH2_CPP20
#define COMPUTE_DURATION_IF_CPP20(ALGO, CONCEPT, STEP, F)                                          \
    compute_duration((F), ALGO, CONCEPT, STEP, __FILE__, __LINE__)
#else
#define COMPUTE_DURATION_IF_CPP20(ALGO, CONCEPT, STEP, F)                                          \
    std::chrono::nanoseconds(std::chrono::seconds(1000))
#endif

#define CHECK_EQUAL(A, B) assert((A) == (B))

#if defined(PERF_TEST)
#define COMPARE_DURATION_TO_STD_ALGOS(ALGO, CONCEPT, F)                                            \
    {                                                                                              \
        namespace STD = std;                                                                       \
        auto test_std = (F);                                                                       \
        {                                                                                          \
            namespace STD = RAH2_NS::ranges;                                                       \
            auto test_rah2 = (F);                                                                  \
            compare_duration(test_std, test_rah2, ALGO, CONCEPT, "std", __FILE__, __LINE__);       \
        }                                                                                          \
    }
#else
#define COMPARE_DURATION_TO_STD_ALGOS(ALGO, CONCEPT, F)                                            \
    {                                                                                              \
        namespace STD = RAH2_NS::ranges;                                                           \
        (void)ALGO;                                                                                \
        (void)CONCEPT;                                                                             \
        (void)(F);                                                                                 \
    }
#endif

#if defined(PERF_TEST) and RAH2_CPP20
#define COMPARE_DURATION_TO_STD_RANGES(ALGO, CONCEPT, F)                                            \
    {                                                                                               \
        namespace STD = std::ranges;                                                                \
        auto test_std = (F);                                                                        \
        {                                                                                           \
            namespace STD = RAH2_NS::ranges;                                                        \
            auto test_rah2 = (F);                                                                   \
            compare_duration(test_std, test_rah2, ALGO, CONCEPT, "std_ranges", __FILE__, __LINE__); \
        }                                                                                           \
    }
#else
#define COMPARE_DURATION_TO_STD_RANGES(ALGO, CONCEPT, F)                                           \
    {                                                                                              \
        namespace STD = RAH2_NS::ranges;                                                           \
        (void)ALGO;                                                                                \
        (void)CONCEPT;                                                                             \
        (void)(F);                                                                                 \
    }
#endif

#if defined(PERF_TEST) and RAH2_CPP20
#define COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(IS_COMMON, ALGO, CONCEPT, F)                       \
    call_if_true<IS_COMMON>(                                                                       \
        [&](auto fwd)                                                                              \
        {                                                                                          \
            namespace STD = std;                                                                   \
            auto test_std = (F);                                                                   \
            {                                                                                      \
                namespace STD = std::ranges;                                                       \
                auto test_std_ranges = (F);                                                        \
                {                                                                                  \
                    namespace STD = RAH2_NS::ranges;                                               \
                    auto test_rah2 = (F);                                                          \
                    compare_duration(                                                              \
                        test_std, test_std_ranges, test_rah2, ALGO, CONCEPT, __FILE__, __LINE__);  \
                }                                                                                  \
            }                                                                                      \
        });                                                                                        \
    call_if_true<not(IS_COMMON)>(                                                                  \
        [&](auto fwd)                                                                              \
        {                                                                                          \
            namespace STD = std::ranges;                                                           \
            auto test_std_ranges = (F);                                                            \
            {                                                                                      \
                namespace STD = RAH2_NS::ranges;                                                   \
                auto test_rah2 = (F);                                                              \
                compare_duration(                                                                  \
                    test_std_ranges, test_rah2, ALGO, CONCEPT, "std::ranges", __FILE__, __LINE__); \
            }                                                                                      \
        });
#elif defined(PERF_TEST)
#define COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(IS_COMMON, ALGO, CONCEPT, F)                       \
    call_if_true<IS_COMMON>(                                                                       \
        [&](auto fwd)                                                                              \
        {                                                                                          \
            namespace STD = std;                                                                   \
            auto test_std = (F);                                                                   \
            {                                                                                      \
                namespace STD = RAH2_NS::ranges;                                                   \
                auto test_rah2 = (F);                                                              \
                compare_duration(test_std, test_rah2, ALGO, CONCEPT, "std", __FILE__, __LINE__);   \
            }                                                                                      \
        });
#else
#define COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(IS_COMMON, ALGO, CONCEPT, F)                       \
    {                                                                                              \
        namespace STD = RAH2_NS::ranges;                                                           \
        Fwd fwd;                                                                                   \
        (void)ALGO;                                                                                \
        (void)CONCEPT;                                                                             \
        (void)(F);                                                                                 \
    }
#endif

#if defined(PERF_TEST) and RAH2_CPP20
#define COMPARE_DURATION_TO_STD_ALGO17_AND_RANGES(IS_COMMON, ALGO, CONCEPT, F)                     \
    call_if_true<IS_COMMON>(                                                                       \
        [&](auto fwd)                                                                              \
        {                                                                                          \
            namespace STD = std;                                                                   \
            auto test_std = (F);                                                                   \
            {                                                                                      \
                namespace STD = std::ranges;                                                       \
                auto test_std_ranges = (F);                                                        \
                {                                                                                  \
                    namespace STD = RAH2_NS::ranges;                                               \
                    auto test_rah2 = (F);                                                          \
                    compare_duration(                                                              \
                        test_std, test_std_ranges, test_rah2, ALGO, CONCEPT, __FILE__, __LINE__);  \
                }                                                                                  \
            }                                                                                      \
        });                                                                                        \
    call_if_true<not(IS_COMMON)>(                                                                  \
        [&](auto fwd)                                                                              \
        {                                                                                          \
            namespace STD = std::ranges;                                                           \
            auto test_std_ranges = (F);                                                            \
            {                                                                                      \
                namespace STD = RAH2_NS::ranges;                                                   \
                auto test_rah2 = (F);                                                              \
                compare_duration(                                                                  \
                    test_std_ranges, test_rah2, ALGO, CONCEPT, "std::ranges", __FILE__, __LINE__); \
            }                                                                                      \
        });
#elif defined(PERF_TEST) and RAH2_CPP17
#define COMPARE_DURATION_TO_STD_ALGO17_AND_RANGES(IS_COMMON, ALGO, CONCEPT, F)                     \
    call_if_true<IS_COMMON>(                                                                       \
        [&](auto fwd)                                                                              \
        {                                                                                          \
            namespace STD = std;                                                                   \
            auto test_std = (F);                                                                   \
            {                                                                                      \
                namespace STD = RAH2_NS::ranges;                                                   \
                auto test_rah2 = (F);                                                              \
                compare_duration(test_std, test_rah2, ALGO, CONCEPT, "std", __FILE__, __LINE__);   \
            }                                                                                      \
        });
#else
#define COMPARE_DURATION_TO_STD_ALGO17_AND_RANGES(IS_COMMON, ALGO, CONCEPT, F)                     \
    {                                                                                              \
        namespace STD = RAH2_NS::ranges;                                                           \
        Fwd fwd;                                                                                   \
        (void)ALGO;                                                                                \
        (void)CONCEPT;                                                                             \
        (void)(F);                                                                                 \
    }
#endif

#define DONT_OPTIM(V) memcpy((char*)&testSuite.avoid_optim, (char*)&V, RAH2_STD::min(sizeof(V), sizeof(size_t)))
