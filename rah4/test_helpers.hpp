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
    class iterator : public rah::iterator_facade<iterator, rah::sentinel_iterator, int, Cat>
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
        auto operator*() const
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
static_assert(rah::bidirectional_range<BidirCommonView>, "Should not be bidirectional");
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
static_assert(rah::bidirectional_range<RandomCommonView>, "Should be bidirectional");
using RandomCommonViewIter = RAH_NAMESPACE::iterator_t<RandomCommonView>;
auto tutu = rah::random_access_iterator_impl<RandomCommonViewIter>::Check{};
STATIC_ASSERT(rah::random_access_iterator<RAH_NAMESPACE::iterator_t<RandomCommonView>>);
static_assert(rah::random_access_range<RandomCommonView>, "Should be random");
static_assert(not rah::contiguous_range<RandomCommonView>, "Should not be contiguous");

// contiguous
using ContiCommonView = test_view<Common, rah::contiguous_iterator_tag>;
static_assert(
    rah::input_or_output_iterator<RAH_NAMESPACE::iterator_t<ContiCommonView>>, "Should be input");
static_assert(rah::input_range<ContiCommonView>, "Should be input");
static_assert(not rah::output_range<ContiCommonView>, "Should be input");
static_assert(rah::forward_range<ContiCommonView>, "Should be Forward");
static_assert(rah::bidirectional_range<ContiCommonView>, "Should not be bidirectional");
static_assert(rah::random_access_range<ContiCommonView>, "Should not be random");
static_assert(rah::contiguous_range<ContiCommonView>, "Should not be contiguous");

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

template <class T>
struct check_bidirectional_range
{
    STATIC_ASSERT(rah::range<T>);
    typename rah::bidirectional_iterator_impl<RAH_NAMESPACE::iterator_t<T>>::Check kjghjhg;
};

template <class T>
struct check_random_access_range
{
    STATIC_ASSERT(rah::range<T>);
    typename rah::random_access_iterator_impl<RAH_NAMESPACE::iterator_t<T>>::Check kjghjhg;
};
