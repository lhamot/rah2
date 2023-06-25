#pragma once

#include "rah4.hpp"

#define STATIC_ASSERT(PRED) static_assert(PRED, #PRED)

enum CommonOrSent
{
    Common,
    Sentinel
};

template <CommonOrSent Sent, typename Cat>
class test_view : public rah::view_interface<test_view<Sent, Cat>>
{
    int start_ = 0;
    int stop_ = 0;
    int step_ = 1;

public:
    class iterator : public rah::iterator_facade<iterator, rah::sentinel_iterator, int&, Cat>
    {
        int val_ = int();
        int step_ = int(1);

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
        RAH_POST_INCR
        template <
            typename C = Cat,
            std::enable_if_t<rah::derived_from<C, std::random_access_iterator_tag>>* = nullptr>
        iterator& operator+=(intptr_t value)
        {
            val_ += int(step_ * value);
            return *this;
        }

        template <
            typename C = Cat,
            std::enable_if_t<rah::derived_from<C, std::bidirectional_iterator_tag>>* = nullptr>
        iterator& operator--()
        {
            val_ -= step_;
            return *this;
        }
        template <
            typename C = Cat,
            std::enable_if_t<rah::derived_from<C, std::bidirectional_iterator_tag>>* = nullptr>
        RAH_POST_DECR;
        template <
            typename C = Cat,
            std::enable_if_t<rah::derived_from<C, std::random_access_iterator_tag>>* = nullptr>
        auto operator-(iterator const& other) const
        {
            return (val_ - other.val_) / step_;
        }
        template <
            typename C = Cat,
            std::enable_if_t<rah::derived_from<C, std::random_access_iterator_tag>>* = nullptr>
        iterator& operator-=(int64_t value)
        {
            val_ -= int(step_ * value);
            return *this;
        }
        auto& operator*() const
        {
            return val_;
        }
        auto& operator*()
        {
            return val_;
        }
        template <typename C = Cat, std::enable_if_t<rah::derived_from<C, std::forward_iterator_tag>>* = nullptr>
        friend constexpr bool operator==(iterator const& it1, iterator const& it2)
        {
            return it1.val_ == it2.val_;
        }
        friend constexpr bool operator==(rah::default_sentinel const&, iterator const&)
        {
            return false;
        }
        friend constexpr bool operator==(iterator const&, rah::default_sentinel const&)
        {
            return false;
        }
        template <
            typename C = Cat,
            std::enable_if_t<rah::derived_from<C, std::random_access_iterator_tag>>* = nullptr>
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

    auto begin()
    {
        return iterator(start_, step_);
    }
    template <CommonOrSent S = Sent, std::enable_if_t<S == Sentinel>* = nullptr>
    auto end()
    {
        return rah::default_sentinel{};
    }
    template <CommonOrSent S = Sent, std::enable_if_t<S == Common>* = nullptr>
    auto end()
    {
        auto const last_index = (stop_ - start_);
        auto const rounded_last = ((last_index + (step_ - 1)) / step_) * step_;
        return iterator(start_ + rounded_last, step_);
    }
    template <typename C = Cat, std::enable_if_t<std::is_same_v<C, rah::contiguous_iterator_tag>>* = nullptr>
    int* data()
    {
        return nullptr;
    }

    template <typename C = Cat, std::enable_if_t<std::is_same_v<C, rah::contiguous_iterator_tag>>* = nullptr>
    int* data() const
    {
        return nullptr;
    }
};

template <CommonOrSent Sent, typename Cat, typename Range>
class test_view_adapter : public rah::view_interface<test_view<Sent, Cat>>
{
    Range base_;
    using base_iterator = rah::iterator_t<Range>;
    using base_sentinel = rah::sentinel_t<Range>;

public:
    class iterator : public rah::iterator_facade<iterator, rah::sentinel_iterator, int&, Cat>
    {
        base_iterator iter_;

    public:
        iterator() = default;
        iterator(base_iterator it)
            : iter_(it)
        {
        }

        iterator& operator++()
        {
            ++iter_;
            return *this;
        }
        RAH_POST_INCR
        template <
            typename C = Cat,
            std::enable_if_t<rah::derived_from<C, std::random_access_iterator_tag>>* = nullptr>
        iterator& operator+=(intptr_t value)
        {
            iter_ += value;
            return *this;
        }

        template <
            typename C = Cat,
            std::enable_if_t<rah::derived_from<C, std::bidirectional_iterator_tag>>* = nullptr>
        iterator& operator--()
        {
            --iter_;
            return *this;
        }
        template <
            typename C = Cat,
            std::enable_if_t<rah::derived_from<C, std::bidirectional_iterator_tag>>* = nullptr>
        RAH_POST_DECR;
        template <
            typename C = Cat,
            std::enable_if_t<rah::derived_from<C, std::random_access_iterator_tag>>* = nullptr>
        auto operator-(iterator const& other) const
        {
            return iter_ - other.iter_;
        }
        template <
            typename C = Cat,
            std::enable_if_t<rah::derived_from<C, std::random_access_iterator_tag>>* = nullptr>
        iterator& operator-=(int64_t value)
        {
            iter_ -= value;
            return *this;
        }
        rah::iter_reference_t<base_iterator> operator*() const
        {
            return *iter_;
        }
        rah::iter_reference_t<base_iterator> operator*()
        {
            return *iter_;
        }
        template <typename C = Cat, std::enable_if_t<rah::derived_from<C, std::forward_iterator_tag>>* = nullptr>
        friend constexpr bool operator==(iterator const& it1, iterator const& it2)
        {
            return it1.iter_ == it2.iter_;
        }
        friend constexpr bool operator==(rah::default_sentinel const&, iterator const&)
        {
            return false;
        }
        friend constexpr bool operator==(iterator const&, rah::default_sentinel const&)
        {
            return false;
        }
        template <
            typename C = Cat,
            std::enable_if_t<rah::derived_from<C, std::random_access_iterator_tag>>* = nullptr>
        friend bool operator<(iterator const& it1, iterator const& it2)
        {
            return it1.iter_ < it2.iter_;
        }
    };

    test_view_adapter() = default;
    test_view_adapter(Range r)
        : base_(std::move(r))
    {
    }

    auto begin()
    {
        return iterator(rah::begin(base_));
    }
    template <CommonOrSent S = Sent, std::enable_if_t<S == Sentinel>* = nullptr>
    auto end()
    {
        return rah::default_sentinel{};
    }
    template <CommonOrSent S = Sent, std::enable_if_t<S == Common>* = nullptr>
    auto end()
    {
        return iterator(rah::end(base_));
    }
    template <typename C = Cat, std::enable_if_t<std::is_same_v<C, rah::contiguous_iterator_tag>>* = nullptr>
    int* data()
    {
        return rah::data(base_);
    }

    template <typename C = Cat, std::enable_if_t<std::is_same_v<C, rah::contiguous_iterator_tag>>* = nullptr>
    int* data() const
    {
        return rah::data(base_);
    }
};

MAKE_CONCEPT(has_pos_incr, true, (std::declval<T>()++));

template <CommonOrSent Sent, typename Cat>
auto make_test_view(int start, int stop, int step)
{
    return test_view<Sent, Cat>(start, stop, step);
}

template <CommonOrSent Sent, typename Cat>
auto make_test_view()
{
    return test_view<Sent, Cat>(0, 10, 1);
}

template <CommonOrSent Sent, typename Cat, typename Range>
auto make_test_view_adapter(Range&& r)
{
    return test_view_adapter<Sent, Cat, std::remove_reference_t<Range>>(std::forward<Range>(r));
}

// output
using OutputCommonView = test_view<Common, std::output_iterator_tag>;
static_assert(
    rah::input_or_output_iterator<RAH_NAMESPACE::iterator_t<OutputCommonView>>, "Should be input");
static_assert(not rah::input_range<OutputCommonView>, "Should be input");
static_assert(rah::output_range<OutputCommonView>, "Should be input");
static_assert(not rah::input_or_output_iterator<OutputCommonView>, "Should be input");
static_assert(not rah::forward_range<OutputCommonView>, "Should be Forward");
static_assert(not rah::bidirectional_range<OutputCommonView>, "Should not be bidirectional");
static_assert(not rah::random_access_range<OutputCommonView>, "Should not be random");
static_assert(not rah::contiguous_range<OutputCommonView>, "Should not be contiguous");

// input
using InputCommonView = test_view<Common, std::input_iterator_tag>;
static_assert(
    rah::input_or_output_iterator<RAH_NAMESPACE::iterator_t<InputCommonView>>, "Should be input");
static_assert(rah::input_range<InputCommonView>, "Should be input");
static_assert(not rah::output_range<InputCommonView>, "Should be input");
static_assert(not rah::forward_range<InputCommonView>, "Should be Forward");
static_assert(not rah::bidirectional_range<InputCommonView>, "Should not be bidirectional");
static_assert(not rah::random_access_range<InputCommonView>, "Should not be random");
static_assert(not rah::contiguous_range<InputCommonView>, "Should not be contiguous");

// Forward
using ForwardCommonView = test_view<Common, std::forward_iterator_tag>;
static_assert(
    rah::input_or_output_iterator<RAH_NAMESPACE::iterator_t<ForwardCommonView>>, "Should be input");
static_assert(rah::input_range<ForwardCommonView>, "Should be input");
static_assert(not rah::output_range<ForwardCommonView>, "Should be input");
static_assert(rah::forward_range<ForwardCommonView>, "Should be Forward");
static_assert(!rah::bidirectional_range<ForwardCommonView>, "Should not be bidirectional");
static_assert(!rah::random_access_range<ForwardCommonView>, "Should not be random");
static_assert(not rah::contiguous_range<ForwardCommonView>, "Should not be contiguous");

// bidirectional
using BidirCommonView = test_view<Common, std::bidirectional_iterator_tag>;
static_assert(
    rah::input_or_output_iterator<RAH_NAMESPACE::iterator_t<BidirCommonView>>, "Should be input");
static_assert(rah::input_range<BidirCommonView>, "Should be input");
static_assert(not rah::output_range<BidirCommonView>, "Should be input");
static_assert(rah::forward_range<BidirCommonView>, "Should be Forward");
STATIC_ASSERT((rah::bidirectional_range_impl<BidirCommonView, true>::value));
static_assert(!rah::random_access_range<BidirCommonView>, "Should not be random");
static_assert(not rah::contiguous_range<BidirCommonView>, "Should not be contiguous");

// random access
using RandomCommonView = test_view<Common, std::random_access_iterator_tag>;
static_assert(
    rah::input_or_output_iterator<RAH_NAMESPACE::iterator_t<RandomCommonView>>, "Should be input");
static_assert(rah::input_range<RandomCommonView>, "Should be input");
static_assert(not rah::output_range<RandomCommonView>, "Should be input");
static_assert(rah::forward_range<RandomCommonView>, "Should be Forward");
using It = rah::iterator_t<RandomCommonView>;
using cat = rah::range_iter_categ_t<RandomCommonView>;
static_assert(rah::forward_iterator<It>, "");
static_assert(std::is_same_v<decltype(--std::declval<It>()), It&>, "");
STATIC_ASSERT((rah::bidirectional_range_impl<RandomCommonView, true>::value));
static_assert(rah::bidirectional_range<RandomCommonView>, "Should be bidirectional");
using RandomCommonViewIter = RAH_NAMESPACE::iterator_t<RandomCommonView>;
STATIC_ASSERT((rah::random_access_range_impl<RandomCommonView, true>::value));
static_assert(not rah::contiguous_range<RandomCommonView>, "Should not be contiguous");

// contiguous
using ContiCommonView = test_view<Common, rah::contiguous_iterator_tag>;
static_assert(
    rah::input_or_output_iterator<RAH_NAMESPACE::iterator_t<ContiCommonView>>, "Should be input");
static_assert(rah::input_range<ContiCommonView>, "Should be input");
static_assert(not rah::output_range<ContiCommonView>, "Should be input");
static_assert(rah::forward_range<ContiCommonView>, "Should be Forward");
static_assert(rah::bidirectional_range<ContiCommonView>, "Should be bidirectional");
static_assert(rah::random_access_range<ContiCommonView>, "Should be random");
STATIC_ASSERT((rah::contiguous_range_impl<ContiCommonView, true>::value));
static_assert(rah::contiguous_range<ContiCommonView>, "Should be contiguous");

template <typename R>
constexpr bool is_input_common =
    rah::input_range<R> && not rah::forward_range<R> && rah::common_range<R>;

template <typename R>
constexpr bool is_input_not_common =
    rah::input_range<R> && not rah::forward_range<R> && not rah::common_range<R>;

template <typename R>
constexpr bool is_forward_common =
    rah::forward_range<R> && not rah::bidirectional_range<R> && rah::common_range<R>;

template <typename R>
constexpr bool is_forward_not_common =
    rah::forward_range<R> && not rah::bidirectional_range<R> && not rah::common_range<R>;

template <typename R>
constexpr bool is_bidirectional_common =
    rah::bidirectional_range<R> && not rah::random_access_range<R> && rah::common_range<R>;

template <typename R>
constexpr bool is_bidirectional_not_common =
    rah::bidirectional_range<R> && not rah::random_access_range<R> && not rah::common_range<R>;

template <typename R>
constexpr bool is_random_access_common =
    rah::random_access_range<R> && not rah::contiguous_range<R> && rah::common_range<R>;

template <typename R>
constexpr bool is_random_access_not_common =
    rah::random_access_range<R> && not rah::contiguous_range<R> && not rah::common_range<R>;

template <typename R>
constexpr bool is_contiguous_common = rah::contiguous_range<R> && rah::common_range<R>;

template <typename R>
constexpr bool is_contiguous_not_common = rah::contiguous_range<R> && not rah::common_range<R>;

template <typename C, typename R>
struct check_cat_impl;

template <typename R>
struct check_cat_impl<std::input_iterator_tag, R>
{
    STATIC_ASSERT(rah::input_range<R>);
    STATIC_ASSERT(not rah::forward_range<R>);
};

template <typename R>
struct check_cat_impl<std::forward_iterator_tag, R>
{
    STATIC_ASSERT((rah::forward_iterator_impl<rah::iterator_t<R>, true>::value));
    STATIC_ASSERT(rah::forward_range<R>);
    STATIC_ASSERT(not rah::bidirectional_range<R>);
};

template <typename R>
struct check_cat_impl<std::bidirectional_iterator_tag, R>
{
    STATIC_ASSERT((rah::bidirectional_range_impl<R, true>::value));
    STATIC_ASSERT(not rah::random_access_range<R>);
};

template <typename R>
struct check_cat_impl<std::random_access_iterator_tag, R>
{
    STATIC_ASSERT((rah::random_access_range_impl<R, true>::value));
    STATIC_ASSERT(not rah::contiguous_range<R>);
};

template <typename R>
struct check_cat_impl<rah::contiguous_iterator_tag, R>
{
    STATIC_ASSERT((rah::contiguous_range_impl<R, true>::value));
};

template <typename BaseCat, typename CapCat, typename R>
void check_cat(R&&)
{
    using expected_cat = rah::common_iterator_tag<BaseCat, CapCat>;
    check_cat_impl<expected_cat, std::remove_reference_t<R>>();
}

enum class SentinelPolicy
{
    AllCommon,
    AllSentinel,
    Keep,
    CommonIfSizedRandomAccess
};

template <CommonOrSent BaseCommon, SentinelPolicy Policy, typename R>
struct check_sent_impl;

template <typename R>
struct check_sent_impl<Common, SentinelPolicy::AllCommon, R>
{
    STATIC_ASSERT(rah::common_range<R>);
};

template <typename R>
struct check_sent_impl<Common, SentinelPolicy::AllSentinel, R>
{
    STATIC_ASSERT(not rah::common_range<R>);
};
template <typename R>
struct check_sent_impl<Common, SentinelPolicy::Keep, R>
{
    STATIC_ASSERT(rah::common_range<R>);
};
template <CommonOrSent BaseCommon, typename R>
struct check_sent_impl<BaseCommon, SentinelPolicy::CommonIfSizedRandomAccess, R>
{
    STATIC_ASSERT(
        (rah::sized_range<R> && rah::random_access_range<R> && rah::common_range<R>)
        || !(rah::sized_range<R> && rah::random_access_range<R>)&&!rah::common_range<R>);
};

template <typename R>
struct check_sent_impl<Sentinel, SentinelPolicy::AllCommon, R>
{
    STATIC_ASSERT(rah::common_range<R>);
};

template <typename R>
struct check_sent_impl<Sentinel, SentinelPolicy::AllSentinel, R>
{
    STATIC_ASSERT(not rah::common_range<R>);
};
template <typename R>
struct check_sent_impl<Sentinel, SentinelPolicy::Keep, R>
{
    STATIC_ASSERT(not rah::common_range<R>);
};

template <CommonOrSent BaseIsCommon, SentinelPolicy SentPolicy, typename R>
void check_sent(R&&)
{
    check_sent_impl<BaseIsCommon, SentPolicy, std::remove_reference_t<R>>();
}

template <SentinelPolicy SentPolicy, typename CapCat, typename MakeR>
void check_all_cat(MakeR&& maker)
{
    {
        auto r1 = maker.template make<Sentinel, std::input_iterator_tag>();
        check_cat<std::input_iterator_tag, CapCat>(r1);
        check_sent<Sentinel, SentPolicy>(r1);
        auto r2 = maker.template make<Sentinel, std::forward_iterator_tag>();
        check_cat<std::forward_iterator_tag, CapCat>(r2);
        check_sent<Sentinel, SentPolicy>(r2);
        auto r3 = maker.template make<Sentinel, std::bidirectional_iterator_tag>();
        check_cat<std::bidirectional_iterator_tag, CapCat>(r3);
        check_sent<Sentinel, SentPolicy>(r3);
        auto r4 = maker.template make<Sentinel, std::random_access_iterator_tag>();
        check_cat<std::random_access_iterator_tag, CapCat>(r4);
        check_sent<Sentinel, SentPolicy>(r4);
        auto r5 = maker.template make<Sentinel, rah::contiguous_iterator_tag>();
        check_cat<rah::contiguous_iterator_tag, CapCat>(r5);
        check_sent<Sentinel, SentPolicy>(r5);
    }
    {
        // A common input range can't exist since it can't compare its begin ot its end
        //auto r1 = maker.template make<Common, std::input_iterator_tag>();
        //check_cat<std::input_iterator_tag, CapCat>(r1);
        //check_sent<Common, SentPolicy>(r1);
        auto r2 = maker.template make<Common, std::forward_iterator_tag>();
        check_cat<std::forward_iterator_tag, CapCat>(r2);
        check_sent<Common, SentPolicy>(r2);
        auto r3 = maker.template make<Common, std::bidirectional_iterator_tag>();
        check_cat<std::bidirectional_iterator_tag, CapCat>(r3);
        check_sent<Common, SentPolicy>(r3);
        auto r4 = maker.template make<Common, std::random_access_iterator_tag>();
        check_cat<std::random_access_iterator_tag, CapCat>(r4);
        check_sent<Common, SentPolicy>(r4);
        auto r5 = maker.template make<Common, rah::contiguous_iterator_tag>();
        check_cat<rah::contiguous_iterator_tag, CapCat>(r5);
        check_sent<Common, SentPolicy>(r5);
    }
}
