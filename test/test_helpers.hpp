#pragma once

#include <rah2/ranges.hpp>
#include <rah2/algorithm.hpp>

#include <iostream>
#include <chrono>

#include <map>
#include <vector>
#include <string>

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
    RAH2_STD::map<RAH2_STD::string, RAH2_STD::vector<RAH2_STD::pair<RAH2_STD::string, RAH2_STD::string>>>
        testCases;

    bool current_test_status = true;
    size_t test_count = 0;

    void addTest(RAH2_STD::string const& group, RAH2_STD::string const& name, std::function<void()> test)
    {
        testMap.emplace(group + " - " + name, std::move(test));
    }

    char const* currentTest = nullptr;
    void test_case(char const* testcase, char const* extra = "")
    {
        testCases[currentTest].emplace_back(testcase, extra);
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
        }
    }
};

template <bool A, bool B>
struct AssertEqual;

template <bool X>
struct AssertEqual<X, X>
{
};

enum CommonOrSent
{
    Common,
    Sentinel
};

template <CommonOrSent Sent, typename Cat, bool SizedRange>
class test_view : public rah2::ranges::view_interface<test_view<Sent, Cat, SizedRange>>
{
    int start_ = 0;
    int stop_ = 0;
    int step_ = 1;

public:
    class iterator;
    struct sentinel
    {
    };

    using ref_type =
        std::conditional_t<rah2::is_same_v<Cat, rah2::output_iterator_tag>, int&, int const&>;

    class iterator : public rah2::ranges::iterator_facade<iterator, sentinel, ref_type, Cat>
    {
        int val_ = int();
        int step_ = static_cast<int>(1);

    public:
        iterator() = default;
        iterator(int val, int step)
            : val_(val)
            , step_(step)
        {
        }

        iterator& operator++()
        {
            val_ += step_;
            return *this;
        }
        RAH2_POST_INCR(Cat)
        template <
            typename C = Cat,
            std::enable_if_t<rah2::derived_from<C, rah2::random_access_iterator_tag>>* = nullptr>
        iterator& operator+=(intptr_t value)
        {
            val_ += static_cast<int>(step_ * value);
            return *this;
        }

        template <
            typename C = Cat,
            std::enable_if_t<rah2::derived_from<C, rah2::bidirectional_iterator_tag>>* = nullptr>
        iterator& operator--()
        {
            val_ -= step_;
            return *this;
        }
        template <
            typename C = Cat,
            std::enable_if_t<rah2::derived_from<C, rah2::bidirectional_iterator_tag>>* = nullptr>
        RAH2_POST_DECR;
        template <
            typename C = Cat,
            std::enable_if_t<rah2::derived_from<C, rah2::random_access_iterator_tag>>* = nullptr>
        auto operator-(iterator const& other) const
        {
            return (val_ - other.val_) / step_;
        }
        template <
            typename C = Cat,
            std::enable_if_t<rah2::derived_from<C, rah2::random_access_iterator_tag>>* = nullptr>
        iterator& operator-=(intptr_t value)
        {
            val_ -= static_cast<int>(step_ * value);
            return *this;
        }
        auto& operator*() const
        {
            return val_;
        }
        ref_type operator*()
        {
            return val_;
        }
        template <typename C = Cat, std::enable_if_t<rah2::derived_from<C, rah2::forward_iterator_tag>>* = nullptr>
        friend constexpr bool operator==(iterator const& it1, iterator const& it2)
        {
            return it1.val_ == it2.val_;
        }
        friend constexpr bool operator==(rah2::default_sentinel_t const&, iterator const&)
        {
            return false;
        }
        friend constexpr bool operator==(iterator const&, rah2::default_sentinel_t const&)
        {
            return false;
        }
        template <
            typename C = Cat,
            std::enable_if_t<rah2::derived_from<C, rah2::random_access_iterator_tag>>* = nullptr>
        friend bool operator<(iterator const& it1, iterator const& it2)
        {
            return it1.val_ < it2.val_;
        }
    };

    test_view() = default;
    test_view(int start, int stop, int step)
        : start_(start)
        , stop_(stop)
        , step_(step)
    {
    }

    template <bool IsSized = SizedRange, std::enable_if_t<IsSized>* = nullptr>
    auto size() const
    {
        return (stop_ - start_) / step_;
    }

    auto begin()
    {
        return iterator(start_, step_);
    }
    template <CommonOrSent S = Sent, std::enable_if_t<S == Sentinel>* = nullptr>
    auto end()
    {
        return rah2::default_sentinel_t{};
    }
    template <CommonOrSent S = Sent, std::enable_if_t<S == Common>* = nullptr>
    auto end()
    {
        auto const last_index = (stop_ - start_);
        auto const rounded_last = ((last_index + (step_ - 1)) / step_) * step_;
        return iterator(start_ + rounded_last, step_);
    }
    template <typename C = Cat, std::enable_if_t<rah2::is_same_v<C, rah2::contiguous_iterator_tag>>* = nullptr>
    RAH2_STD::remove_reference_t<ref_type>* data()
    {
        return nullptr;
    }

    template <typename C = Cat, std::enable_if_t<rah2::is_same_v<C, rah2::contiguous_iterator_tag>>* = nullptr>
    RAH2_STD::remove_reference_t<ref_type>* data() const
    {
        return nullptr;
    }
};

template <CommonOrSent Sent, typename Cat, bool SizedRange, typename Range>
class test_view_adapter : public rah2::ranges::view_interface<test_view<Sent, Cat, SizedRange>>
{
    Range base_;
    using base_iterator = rah2::ranges::iterator_t<Range>;
    using base_sentinel = rah2::ranges::sentinel_t<Range>;

public:
    class iterator
        : public rah2::ranges::iterator_facade<iterator, rah2::ranges::sentinel_iterator, int&, Cat>
    {
        base_iterator iter_;

    public:
        iterator() = default;
        explicit iterator(base_iterator it)
            : iter_(it)
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
            std::enable_if_t<rah2::derived_from<C, rah2::random_access_iterator_tag>>* = nullptr>
        iterator& operator+=(intptr_t value)
        {
            iter_ += value;
            return *this;
        }

        template <
            typename C = Cat,
            std::enable_if_t<rah2::derived_from<C, rah2::bidirectional_iterator_tag>>* = nullptr>
        iterator& operator--()
        {
            --iter_;
            return *this;
        }
        template <
            typename C = Cat,
            std::enable_if_t<rah2::derived_from<C, rah2::bidirectional_iterator_tag>>* = nullptr>
        RAH2_POST_DECR;
        template <
            typename C = Cat,
            std::enable_if_t<rah2::derived_from<C, rah2::random_access_iterator_tag>>* = nullptr>
        auto operator-(iterator const& other) const
        {
            return iter_ - other.iter_;
        }
        template <
            typename C = Cat,
            std::enable_if_t<rah2::derived_from<C, rah2::random_access_iterator_tag>>* = nullptr>
        iterator& operator-=(intptr_t value)
        {
            iter_ -= value;
            return *this;
        }
        rah2::iter_reference_t<base_iterator> operator*() const
        {
            return *iter_;
        }
        rah2::iter_reference_t<base_iterator> operator*()
        {
            return *iter_;
        }
        template <
            typename C = Cat,
            std::enable_if_t<rah2::derived_from<C, RAH2_STD::forward_iterator_tag>>* = nullptr>
        friend constexpr bool operator==(iterator const& it1, iterator const& it2)
        {
            return it1.iter_ == it2.iter_;
        }
        friend constexpr bool operator==(rah2::default_sentinel_t const&, iterator const&)
        {
            return false;
        }
        friend constexpr bool operator==(iterator const&, rah2::default_sentinel_t const&)
        {
            return false;
        }
        template <
            typename C = Cat,
            std::enable_if_t<rah2::derived_from<C, rah2::random_access_iterator_tag>>* = nullptr>
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
        return iterator(rah2::ranges::begin(base_));
    }
    template <CommonOrSent S = Sent, std::enable_if_t<S == Sentinel>* = nullptr>
    auto end()
    {
        return rah2::default_sentinel_t{};
    }
    template <CommonOrSent S = Sent, std::enable_if_t<S == Common>* = nullptr>
    auto end()
    {
        return iterator(rah2::ranges::end(base_));
    }
    template <typename C = Cat, std::enable_if_t<rah2::is_same_v<C, rah2::contiguous_iterator_tag>>* = nullptr>
    int* data()
    {
        return rah2::ranges::data(base_);
    }

    template <typename C = Cat, std::enable_if_t<rah2::is_same_v<C, rah2::contiguous_iterator_tag>>* = nullptr>
    int* data() const
    {
        return rah2::ranges::data(base_);
    }
};

template <CommonOrSent Sent, typename Cat, bool Sized>
auto make_test_view(int start, int stop, int step)
{
    return test_view<Sent, Cat, Sized>(start, stop, step);
}

template <CommonOrSent Sent, typename Cat, bool Sized>
auto make_test_view()
{
    return test_view<Sent, Cat, Sized>(0, 10, 1);
}

template <CommonOrSent Sent, typename Cat, bool Sized, typename Range>
auto make_test_view_adapter(Range&& r)
{
    return test_view_adapter<Sent, Cat, Sized, std::remove_reference_t<Range>>(std::forward<Range>(r));
}

// output
using OutputCommonView = test_view<Common, rah2::output_iterator_tag, false>;
static_assert(
    rah2::input_or_output_iterator<rah2::ranges::iterator_t<OutputCommonView>>, "Should be input");
static_assert(not rah2::ranges::input_range<OutputCommonView>, "Should be input");
static_assert(rah2::ranges::output_range<OutputCommonView, int>, "Should be output");
static_assert(not rah2::input_or_output_iterator<OutputCommonView>, "Should be input");
static_assert(not rah2::ranges::forward_range<OutputCommonView>, "Should be Forward");
static_assert(not rah2::ranges::bidirectional_range<OutputCommonView>, "Should not be bidirectional");
static_assert(not rah2::ranges::random_access_range<OutputCommonView>, "Should not be random");
static_assert(not rah2::ranges::contiguous_range<OutputCommonView>, "Should not be contiguous");

// input
using InputCommonView = test_view<Common, rah2::input_iterator_tag, false>;
static_assert(
    rah2::input_or_output_iterator<rah2::ranges::iterator_t<InputCommonView>>, "Should be input");
static_assert(rah2::ranges::input_range<InputCommonView>, "Should be input");
static_assert(not rah2::ranges::output_range<InputCommonView, int>, "Should not be output");
static_assert(not rah2::ranges::forward_range<InputCommonView>, "Should be Forward");
static_assert(not rah2::ranges::bidirectional_range<InputCommonView>, "Should not be bidirectional");
static_assert(not rah2::ranges::random_access_range<InputCommonView>, "Should not be random");
static_assert(not rah2::ranges::contiguous_range<InputCommonView>, "Should not be contiguous");

// Forward
using ForwardCommonView = test_view<Common, rah2::forward_iterator_tag, false>;
static_assert(
    rah2::input_or_output_iterator<rah2::ranges::iterator_t<ForwardCommonView>>, "Should be input");
static_assert(rah2::ranges::input_range<ForwardCommonView>, "Should be input");
static_assert(not rah2::ranges::output_range<ForwardCommonView, int>, "Should not be output");
static_assert(rah2::ranges::forward_range_impl<ForwardCommonView, true>::value, "Should be Forward");
static_assert(!rah2::ranges::bidirectional_range<ForwardCommonView>, "Should not be bidirectional");
static_assert(!rah2::ranges::random_access_range<ForwardCommonView>, "Should not be random");
static_assert(not rah2::ranges::contiguous_range<ForwardCommonView>, "Should not be contiguous");

// bidirectional
using BidirCommonView = test_view<Common, rah2::bidirectional_iterator_tag, false>;
static_assert(
    rah2::input_or_output_iterator<rah2::ranges::iterator_t<BidirCommonView>>, "Should be input");
static_assert(rah2::ranges::input_range<BidirCommonView>, "Should be input");
static_assert(not rah2::ranges::output_range<BidirCommonView, int>, "Should not be outnput");
static_assert(rah2::ranges::forward_range<BidirCommonView>, "Should be Forward");
STATIC_ASSERT((rah2::ranges::bidirectional_range_impl<BidirCommonView, true>::value));
static_assert(!rah2::ranges::random_access_range<BidirCommonView>, "Should not be random");
static_assert(not rah2::ranges::contiguous_range<BidirCommonView>, "Should not be contiguous");

// random access
using RandomCommonView = test_view<Common, rah2::random_access_iterator_tag, true>;
static_assert(
    rah2::input_or_output_iterator<rah2::ranges::iterator_t<RandomCommonView>>, "Should be input");
static_assert(rah2::ranges::input_range<RandomCommonView>, "Should be input");
static_assert(not rah2::ranges::output_range<RandomCommonView, int>, "Should not be output");
static_assert(rah2::ranges::forward_range<RandomCommonView>, "Should be Forward");
using It = rah2::ranges::iterator_t<RandomCommonView>;
using cat = rah2::ranges::range_iter_categ_t<RandomCommonView>;
static_assert(rah2::forward_iterator<It>, "");
static_assert(rah2::is_same_v<decltype(--std::declval<It>()), It&>, "");
STATIC_ASSERT((rah2::ranges::bidirectional_range_impl<RandomCommonView, true>::value));
static_assert(rah2::ranges::bidirectional_range<RandomCommonView>, "Should be bidirectional");
using RandomCommonViewIter = rah2::ranges::iterator_t<RandomCommonView>;
STATIC_ASSERT((rah2::ranges::random_access_range_impl<RandomCommonView, true>::value));
static_assert(not rah2::ranges::contiguous_range<RandomCommonView>, "Should not be contiguous");

// contiguous
using ContiCommonView = test_view<Common, rah2::contiguous_iterator_tag, true>;
static_assert(
    rah2::input_or_output_iterator<rah2::ranges::iterator_t<ContiCommonView>>, "Should be input");
static_assert(rah2::ranges::input_range<ContiCommonView>, "Should be input");
static_assert(not rah2::ranges::output_range<ContiCommonView, int>, "Should not be output");
static_assert(rah2::ranges::forward_range<ContiCommonView>, "Should be Forward");
static_assert(rah2::ranges::bidirectional_range<ContiCommonView>, "Should be bidirectional");
static_assert(rah2::ranges::random_access_range<ContiCommonView>, "Should be random");
STATIC_ASSERT((rah2::ranges::contiguous_range_impl<ContiCommonView, true>::value));
static_assert(rah2::ranges::contiguous_range<ContiCommonView>, "Should be contiguous");

template <typename R>
constexpr bool is_input_common = rah2::ranges::input_range<R> && not rah2::ranges::forward_range<R>
                                 && rah2::ranges::common_range<R>;

template <typename R>
constexpr bool is_input_not_common =
    rah2::ranges::input_range<R> && not rah2::ranges::forward_range<R>
    && not rah2::ranges::common_range<R>;

template <typename R>
constexpr bool is_forward_common =
    rah2::ranges::forward_range<R> && not rah2::ranges::bidirectional_range<R>
    && rah2::ranges::common_range<R>;

template <typename R>
constexpr bool is_forward_not_common =
    rah2::ranges::forward_range<R> && not rah2::ranges::bidirectional_range<R>
    && not rah2::ranges::common_range<R>;

template <typename R>
constexpr bool is_bidirectional_common =
    rah2::ranges::bidirectional_range<R> && not rah2::ranges::random_access_range<R>
    && rah2::ranges::common_range<R>;

template <typename R>
constexpr bool is_bidirectional_not_common =
    rah2::ranges::bidirectional_range<R> && not rah2::ranges::random_access_range<R>
    && not rah2::ranges::common_range<R>;

template <typename R>
constexpr bool is_random_access_common =
    rah2::ranges::random_access_range<R> && not rah2::ranges::contiguous_range<R>
    && rah2::ranges::common_range<R>;

template <typename R>
constexpr bool is_random_access_not_common =
    rah2::ranges::random_access_range<R> && not rah2::ranges::contiguous_range<R>
    && not rah2::ranges::common_range<R>;

template <typename R>
constexpr bool is_contiguous_common =
    rah2::ranges::contiguous_range<R> && rah2::ranges::common_range<R>;

template <typename R>
constexpr bool is_contiguous_not_common =
    rah2::ranges::contiguous_range<R> && not rah2::ranges::common_range<R>;

template <typename C, typename R>
struct check_cat_impl;

template <typename R>
struct check_cat_impl<RAH2_STD::input_iterator_tag, R>
{
    STATIC_ASSERT(rah2::ranges::input_range<R>);
    STATIC_ASSERT(not rah2::ranges::forward_range<R>);
};

template <typename R>
struct check_cat_impl<RAH2_STD::forward_iterator_tag, R>
{
    STATIC_ASSERT((rah2::forward_iterator_impl<rah2::ranges::iterator_t<R>, true>::value));
    STATIC_ASSERT(rah2::ranges::forward_range<R>);
    // STATIC_ASSERT(not rah2::bidirectional_range<R>);
};

template <typename R>
struct check_cat_impl<rah2::bidirectional_iterator_tag, R>
{
    STATIC_ASSERT((rah2::ranges::bidirectional_range_impl<R, true>::value));
    STATIC_ASSERT(not rah2::ranges::random_access_range<R>);
};

template <typename R>
struct check_cat_impl<rah2::random_access_iterator_tag, R>
{
    STATIC_ASSERT((rah2::ranges::random_access_range_impl<R, true>::value));
    STATIC_ASSERT(not rah2::ranges::contiguous_range<R>);
};

template <typename R>
struct check_cat_impl<rah2::contiguous_iterator_tag, R>
{
    STATIC_ASSERT((rah2::ranges::contiguous_range_impl<R, true>::value));
};

template <typename BaseCat, typename MinCat, typename MaxCat, typename R>
void check_cat(R&&)
{
    using expected_cat = rah2::ranges::cap_iterator_tag<BaseCat, MinCat, MaxCat>;
    check_cat_impl<expected_cat, std::remove_reference_t<R>>();
}

template <
    bool DoTest,
    CommonOrSent Sentinel,
    typename Cat,
    typename ExpectedCat,
    bool Sized,
    template <CommonOrSent, typename, bool>
    class MakeR>
struct test_one_range_setup_impl
{
    static void test()
    {
        auto t1 = MakeR<Sentinel, Cat, Sized>();
        auto r1 = t1.make();
        check_cat_impl<ExpectedCat, std::remove_reference_t<decltype(r1)>>();
        AssertEqual<rah2::ranges::common_range<decltype(r1)>, t1.is_common>();
        AssertEqual<rah2::ranges::sized_range<decltype(r1)>, t1.is_sized>();
        AssertEqual<rah2::ranges::borrowed_range<decltype(r1)>, t1.is_borrowed>();
    }
};

template <CommonOrSent Sentinel, typename Cat, typename ExpectedCat, bool Sized, template <CommonOrSent, typename, bool> class MakeR>
struct test_one_range_setup_impl<false, Sentinel, Cat, ExpectedCat, Sized, MakeR>
{
    static void test()
    {
    }
};

template <
    CommonOrSent Sentinel,
    typename Cat2,
    typename ExpectedCat,
    bool Sized,
    template <CommonOrSent, typename, bool>
    class Trait>
void test_one_range_setup()
{
    constexpr bool do_test = Trait<Sentinel, Cat2, Sized>::do_test;
    test_one_range_setup_impl<do_test, Sentinel, Cat2, ExpectedCat, Sized, Trait>::test();
}

template <typename MaxCat, typename MinCat = RAH2_STD::input_iterator_tag, template <CommonOrSent, typename, bool> class MakeR>
void check_all_cat()
{
    test_one_range_setup<
        Sentinel,
        RAH2_STD::input_iterator_tag,
        rah2::ranges::cap_iterator_tag<RAH2_STD::input_iterator_tag, MinCat, MaxCat>,
        false,
        MakeR>();
    test_one_range_setup<
        Sentinel,
        RAH2_STD::forward_iterator_tag,
        rah2::ranges::cap_iterator_tag<RAH2_STD::forward_iterator_tag, MinCat, MaxCat>,
        false,
        MakeR>();
    test_one_range_setup<
        Sentinel,
        rah2::bidirectional_iterator_tag,
        rah2::ranges::cap_iterator_tag<rah2::bidirectional_iterator_tag, MinCat, MaxCat>,
        false,
        MakeR>();
    test_one_range_setup<
        Sentinel,
        rah2::random_access_iterator_tag,
        rah2::ranges::cap_iterator_tag<rah2::random_access_iterator_tag, MinCat, MaxCat>,
        false,
        MakeR>();
    test_one_range_setup<
        Sentinel,
        rah2::contiguous_iterator_tag,
        rah2::ranges::cap_iterator_tag<rah2::contiguous_iterator_tag, MinCat, MaxCat>,
        false,
        MakeR>();

    test_one_range_setup<
        Common,
        rah2::forward_iterator_tag,
        rah2::ranges::cap_iterator_tag<rah2::forward_iterator_tag, MinCat, MaxCat>,
        false,
        MakeR>();
    test_one_range_setup<
        Common,
        rah2::bidirectional_iterator_tag,
        rah2::ranges::cap_iterator_tag<rah2::bidirectional_iterator_tag, MinCat, MaxCat>,
        false,
        MakeR>();
    // Common random_access can't be not sized
    /*test_one_range_setup<
        Common,
        rah2::random_access_iterator_tag,
        rah2::cap_iterator_tag<rah2::random_access_iterator_tag, MinCat, MaxCat>,
        false,
        MakeR>();
    test_one_range_setup<
        Common,
        rah2::contiguous_iterator_tag,
        rah2::cap_iterator_tag<rah2::contiguous_iterator_tag, MinCat, MaxCat>,
        false,
        MakeR>();*/

    test_one_range_setup<
        Sentinel,
        rah2::input_iterator_tag,
        rah2::ranges::cap_iterator_tag<rah2::input_iterator_tag, MinCat, MaxCat>,
        true,
        MakeR>();
    test_one_range_setup<
        Sentinel,
        rah2::forward_iterator_tag,
        rah2::ranges::cap_iterator_tag<rah2::forward_iterator_tag, MinCat, MaxCat>,
        true,
        MakeR>();
    test_one_range_setup<
        Sentinel,
        rah2::bidirectional_iterator_tag,
        rah2::ranges::cap_iterator_tag<rah2::bidirectional_iterator_tag, MinCat, MaxCat>,
        true,
        MakeR>();
    test_one_range_setup<
        Sentinel,
        rah2::random_access_iterator_tag,
        rah2::ranges::cap_iterator_tag<rah2::random_access_iterator_tag, MinCat, MaxCat>,
        true,
        MakeR>();
    test_one_range_setup<
        Sentinel,
        rah2::contiguous_iterator_tag,
        rah2::ranges::cap_iterator_tag<rah2::contiguous_iterator_tag, MinCat, MaxCat>,
        true,
        MakeR>();

    test_one_range_setup<
        Common,
        rah2::forward_iterator_tag,
        rah2::ranges::cap_iterator_tag<rah2::forward_iterator_tag, MinCat, MaxCat>,
        true,
        MakeR>();
    test_one_range_setup<
        Common,
        rah2::bidirectional_iterator_tag,
        rah2::ranges::cap_iterator_tag<rah2::bidirectional_iterator_tag, MinCat, MaxCat>,
        true,
        MakeR>();
    test_one_range_setup<
        Common,
        rah2::random_access_iterator_tag,
        rah2::ranges::cap_iterator_tag<rah2::random_access_iterator_tag, MinCat, MaxCat>,
        true,
        MakeR>();
    test_one_range_setup<
        Common,
        rah2::contiguous_iterator_tag,
        rah2::ranges::cap_iterator_tag<rah2::contiguous_iterator_tag, MinCat, MaxCat>,
        true,
        MakeR>();
}

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
            rah2::details::weakly_equality_comparable_with<
                decltype(RAH2_STD::get<0>(ab)),
                decltype(RAH2_STD::get<1>(ab))>,
            "second not assignable to first");
        return RAH2_STD::get<0>(ab) == RAH2_STD::get<1>(ab);
    }
};

static constexpr PairEqualImpl PairEqual;

// #define TEST_DISPLAY_ALL
#define TEST_DISPLAY_FAILED
// #define TEST_DISPLAY_NONE

extern TestSuite testSuite;
inline void assert_impl(int line, char const* condition, bool value)
{
    ++testSuite.test_count;
#if defined(TEST_DISPLAY_ALL)
    std::cout << line << " assert : " << condition << std::endl;
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
        std::cout << line << " assert : " << condition << std::endl;
#endif
#if defined(TEST_DISPLAY_FAILED)
        std::cout << "NOT OK (line:" << line << ")" << std::endl;
#endif
        // abort();
        testSuite.current_test_status = false;
    }
}

#undef assert
#define assert(CONDITION) assert_impl(__LINE__, #CONDITION, (CONDITION))

template <typename R, typename I>
void equalRange(R&& RANGE, I&& IL, char const* rangeName, char const* ILName)
{
    ++testSuite.test_count;

    static_assert(
        rah2::details::weakly_equality_comparable_with<
            rah2::ranges::range_reference_t<decltype(RANGE)>,
            rah2::ranges::range_reference_t<decltype(IL)>>,
        "Can't compare");
#if defined(TEST_DISPLAY_ALL)
    std::cout << "assert : " << rangeName << " == " << ILName << std::endl;
#endif
    if (rah2::ranges::all_of(rah2::views::zip(std::forward<R>(RANGE), std::forward<I>(IL)), PairEqual))
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
        (std::cout << std::forward<decltype(elt)>(elt)) << " ";
    };

    rah2::ranges::details::for_each(tup, print_elt);
    return os;
}

template <typename R, typename = std::enable_if_t<rah2::ranges::range<R>>>
void toto(R&&)
{
}

template <typename V>
auto toto(std::initializer_list<V> il)
{
    return toto(rah2::ranges::make_subrange(begin(il), end(il)));
}

template <typename T>
struct WhatIs;

#define PERF_TEST 0

template <typename F>
auto compute_duration(F&& func, char const* name, char const* file, int line)
{
#if PERF_TEST
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
    if ((end - start) > std::chrono::seconds(1))
    {
        std::cerr << "Too long function at " << file << "(" << line << ")" << std::endl;
    }

    while ((end - start) < std::chrono::seconds(1))
    {
        func();
        ++count;
        end = std::chrono::high_resolution_clock::now();
    }
    std::cout << name << " : " << ((end - start) / count).count() << std::endl;
    return (end - start) / count;
#else
    (void)name;
    (void)func;
    (void)file;
    (void)line;
    return std::chrono::seconds(1);
#endif
}

#define COMPUTE_DURATION(N, F) compute_duration((F), N, __FILE__, __LINE__)

#define CHECK_EQUAL(A, B) assert((A) == (B))
