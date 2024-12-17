#pragma once

#include "TestSuite.hpp"

#include <rah2/ranges.hpp>
#include <rah2/algorithm.hpp>
#include <chrono>

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

    using ref_type =
        RAH2_STD::conditional_t<RAH2_NS::is_same_v<Cat, RAH2_NS::output_iterator_tag>, int&, int const&>;

    class iterator
        : public RAH2_NS::ranges::iterator_facade<iterator, RAH2_NS::default_sentinel_t, ref_type, Cat>
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

        friend std::ostream& operator<<(std::ostream& os, iterator const& it)
        {
            os << &(*it.iter_);
            return os;
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
class test_view_adapter
    : public RAH2_NS::ranges::view_interface<test_view_adapter<Sent, Cat, SizedRange, Range>>
{
    Range base_;
    using base_iterator = RAH2_STD::remove_reference_t<RAH2_NS::ranges::range_reference_t<Range>>*;
    using base_sentinel = base_iterator;
    using const_base_iterator =
        RAH2_STD::remove_reference_t<RAH2_NS::ranges::range_reference_t<Range const>>*;
    using const_base_sentinel = const_base_iterator;
    using ref = RAH2_NS::ranges::range_reference_t<Range>;
    using const_ref = RAH2_NS::ranges::range_reference_t<Range const>;

public:
    class iterator
        : public RAH2_NS::ranges::iterator_facade<iterator, RAH2_NS::default_sentinel_t, ref, Cat>
    {
        base_iterator iter_;
        base_iterator end_;

    public:
        iterator() = default;
        template <typename I, typename U>
        explicit iterator(I&& it, U&& end)
            : iter_(it)
            , end_(end)
        {
        }

        template <bool IsSized = SizedRange, RAH2_STD::enable_if_t<IsSized>* = nullptr>
        friend auto operator-(RAH2_NS::default_sentinel_t, iterator const& it)
        {
            return it.end_ - it.iter_;
        }
        template <bool IsSized = SizedRange, RAH2_STD::enable_if_t<IsSized>* = nullptr>
        friend auto operator-(iterator const& it, RAH2_NS::default_sentinel_t)
        {
            return it.iter_ - it.end_;
        }

        auto base() const
        {
            return iter_;
        }

        constexpr iterator& operator++() noexcept(noexcept(++iter_))
        {
            ++iter_;
            return *this;
        }
        RAH2_POST_INCR(Cat)
        template <
            typename C = Cat,
            RAH2_STD::enable_if_t<RAH2_NS::derived_from<C, RAH2_NS::random_access_iterator_tag>>* = nullptr>
        constexpr iterator& operator+=(intptr_t value) noexcept(noexcept(iter_ += value))
        {
            iter_ += value;
            return *this;
        }

        template <
            typename C = Cat,
            RAH2_STD::enable_if_t<RAH2_NS::derived_from<C, RAH2_NS::bidirectional_iterator_tag>>* = nullptr>
        constexpr iterator& operator--() noexcept(noexcept(--iter_))
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
        constexpr auto operator-(iterator const& other) const noexcept(noexcept(iter_ - other.iter_))
        {
            return iter_ - other.iter_;
        }
        template <
            typename C = Cat,
            RAH2_STD::enable_if_t<RAH2_NS::derived_from<C, RAH2_NS::random_access_iterator_tag>>* = nullptr>
        constexpr iterator& operator-=(intptr_t value) noexcept(noexcept(iter_ -= value))
        {
            iter_ -= value;
            return *this;
        }
        constexpr RAH2_NS::iter_reference_t<base_iterator> operator*() const
            noexcept(noexcept(*iter_))
        {
            // assert(iter_ != end_);  // Enabling assert breaks benchmarks results...
            return *iter_;
        }
        constexpr RAH2_NS::iter_reference_t<base_iterator> operator*() noexcept(noexcept(*iter_))
        {
            // assert(iter_ != end_);  // Enabling assert breaks benchmarks results...
            return *iter_;
        }

        constexpr base_iterator operator->() const noexcept
        {
            return iter_;
        }
        constexpr base_iterator operator->() noexcept
        {
            return iter_;
        }
        template <
            typename C = Cat,
            RAH2_STD::enable_if_t<RAH2_NS::derived_from<C, RAH2_STD::forward_iterator_tag>>* = nullptr>
        friend constexpr bool operator==(iterator const& it1, iterator const& it2) noexcept(
            noexcept(it1.iter_ == it2.iter_))
        {
            return it1.iter_ == it2.iter_;
        }
        friend constexpr bool operator==(
            RAH2_NS::default_sentinel_t const&,
            iterator const& it) noexcept(noexcept(it.iter_ == it.end_))
        {
            return it.iter_ == it.end_;
        }
        friend constexpr bool operator==(
            iterator const& it,
            RAH2_NS::default_sentinel_t const&) noexcept(noexcept(it.iter_ == it.end_))
        {
            return it.iter_ == it.end_;
        }
        template <
            typename C = Cat,
            RAH2_STD::enable_if_t<RAH2_NS::derived_from<C, RAH2_NS::random_access_iterator_tag>>* = nullptr>
        friend constexpr bool
        operator<(iterator const& it1, iterator const& it2) noexcept(noexcept(it1.iter_ < it2.iter_))
        {
            return it1.iter_ < it2.iter_;
        }

        friend std::ostream& operator<<(std::ostream& os, iterator const& it)
        {
            os << &(*it.iter_);
            return os;
        }
    };

    class const_iterator
        : public RAH2_NS::ranges::iterator_facade<const_iterator, RAH2_NS::default_sentinel_t, ref, Cat>
    {
        const_base_iterator iter_;
        const_base_iterator end_;

    public:
        const_iterator() = default;
        template <typename I, typename U>
        explicit const_iterator(I&& it, U&& end)
            : iter_(it)
            , end_(end)
        {
        }

        template <bool IsSized = SizedRange, RAH2_STD::enable_if_t<IsSized>* = nullptr>
        friend auto operator-(RAH2_NS::default_sentinel_t, const_iterator const& it)
        {
            return it.end_ - it.iter_;
        }
        template <bool IsSized = SizedRange, RAH2_STD::enable_if_t<IsSized>* = nullptr>
        friend auto operator-(const_iterator const& it, RAH2_NS::default_sentinel_t)
        {
            return it.iter_ - it.end_;
        }

        const_iterator& operator++()
        {
            ++iter_;
            return *this;
        }
        RAH2_POST_INCR(Cat)
        template <
            typename C = Cat,
            RAH2_STD::enable_if_t<RAH2_NS::derived_from<C, RAH2_NS::random_access_iterator_tag>>* = nullptr>
        const_iterator& operator+=(intptr_t value)
        {
            iter_ += value;
            return *this;
        }

        template <
            typename C = Cat,
            RAH2_STD::enable_if_t<RAH2_NS::derived_from<C, RAH2_NS::bidirectional_iterator_tag>>* = nullptr>
        const_iterator& operator--()
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
        auto operator-(const_iterator const& other) const
        {
            return iter_ - other.iter_;
        }
        template <
            typename C = Cat,
            RAH2_STD::enable_if_t<RAH2_NS::derived_from<C, RAH2_NS::random_access_iterator_tag>>* = nullptr>
        const_iterator& operator-=(intptr_t value)
        {
            iter_ -= value;
            return *this;
        }
        RAH2_NS::iter_reference_t<base_iterator> operator*() const
        {
            // assert(iter_ != end_);  // Enabling assert breaks benchmarks results...
            return *iter_;
        }
        RAH2_NS::iter_reference_t<base_iterator> operator*()
        {
            // assert(iter_ != end_);  // Enabling assert breaks benchmarks results...
            return *iter_;
        }

        base_iterator operator->() const
        {
            return iter_;
        }
        base_iterator operator->()
        {
            return iter_;
        }
        template <
            typename C = Cat,
            RAH2_STD::enable_if_t<RAH2_NS::derived_from<C, RAH2_STD::forward_iterator_tag>>* = nullptr>
        friend constexpr bool operator==(const_iterator const& it1, const_iterator const& it2)
        {
            return it1.iter_ == it2.iter_;
        }
        friend constexpr bool operator==(RAH2_NS::default_sentinel_t const&, const_iterator const& it)
        {
            return it.iter_ == it.end_;
        }
        friend constexpr bool operator==(const_iterator const& it, RAH2_NS::default_sentinel_t const&)
        {
            return it.iter_ == it.end_;
        }
        template <
            typename C = Cat,
            RAH2_STD::enable_if_t<RAH2_NS::derived_from<C, RAH2_NS::random_access_iterator_tag>>* = nullptr>
        friend bool operator<(const_iterator const& it1, const_iterator const& it2)
        {
            return it1.iter_ < it2.iter_;
        }

        friend std::ostream& operator<<(std::ostream& os, const_iterator const& it)
        {
            os << &(*it.iter_);
            return os;
        }
    };

    test_view_adapter() = default;
    explicit test_view_adapter(Range r)
        : base_(std::move(r))
    {
    }

    template <bool IsSized = SizedRange, RAH2_STD::enable_if_t<IsSized>* = nullptr>
    auto size() const
    {
        return base_.size();
    }

    auto begin()
    {
        auto it = RAH2_NS::ranges::begin(base_);
        auto end = RAH2_NS::ranges::end(base_);
        if (it == end)
        {
            return iterator(nullptr, nullptr);
        }
        else
        {
            auto const data = &(*it);
            auto const size = end - it;
            auto const end_ptr = data + size;
            return iterator(data, end_ptr);
        }
    }
    template <typename R = Range, RAH2_STD::enable_if_t<RAH2_NS::ranges::range<R const>>* = nullptr>
    auto begin() const
    {
        auto it = RAH2_NS::ranges::begin(base_);
        auto end = RAH2_NS::ranges::end(base_);
        if (it == end)
        {
            return const_iterator(nullptr, nullptr);
        }
        else
        {
            auto const data = &(*it);
            auto const size = end - it;
            auto const end_ptr = data + size;
            return const_iterator(data, end_ptr);
        }
    }
    template <CommonOrSent S = Sent, RAH2_STD::enable_if_t<S == Sentinel>* = nullptr>
    auto end()
    {
        return RAH2_NS::default_sentinel_t{};
    }
    template <CommonOrSent S = Sent, RAH2_STD::enable_if_t<S == Common>* = nullptr>
    auto end()
    {
        auto it = RAH2_NS::ranges::begin(base_);
        auto end = RAH2_NS::ranges::end(base_);
        if (it == end)
        {
            return iterator(nullptr, nullptr);
        }
        else
        {
            auto const data = &(*it);
            auto const size = end - it;
            auto const end_ptr = data + size;
            return iterator(end_ptr, end_ptr);
        }
    }
    template <
        CommonOrSent S = Sent,
        RAH2_STD::enable_if_t<S == Sentinel && RAH2_NS::ranges::range<Range const>>* = nullptr>
    auto end() const
    {
        return RAH2_NS::default_sentinel_t{};
    }
    template <
        CommonOrSent S = Sent,
        RAH2_STD::enable_if_t<S == Common && RAH2_NS::ranges::range<Range const>>* = nullptr>
    auto end() const
    {
        auto it = RAH2_NS::ranges::begin(base_);
        auto end = RAH2_NS::ranges::end(base_);
        if (it == end)
        {
            return const_iterator(nullptr, nullptr);
        }
        else
        {
            auto const data = &(*it);
            auto const size = end - it;
            auto const end_ptr = data + size;
            return const_iterator(end_ptr, end_ptr);
        }
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

    friend inline std::ostream& operator<<(std::ostream& os, test_view_adapter const& vec)
    {
        os << "[";
        for (auto&& val : vec)
        {
            os << val << " ";
        }
        os << "]";
        return os;
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
    return test_view_adapter<Sent, Cat, Sized, RAH2_NS::views::all_t<Range>>(
        RAH2_NS::views::all(RAH2_STD::forward<Range>(r)));
}

// output
using OutputCommonView = test_view<Common, RAH2_NS::output_iterator_tag, false>;
static_assert(
    RAH2_NS::details::input_or_output_iterator_impl<RAH2_NS::ranges::iterator_t<OutputCommonView>, true>::value,
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
        void call(char const*
                      // #if defined(PERF_TEST)
                      range_type
                  // #endif
        ) const
        {
            auto t1 = MakeR<Sentinel, Cat, Sized>();
            t1.template test<>();
            // #if defined(PERF_TEST)
            t1.template test_perf<>(range_type);
            // #endif
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

template <
    typename A,
    typename B,
    RAH2_STD::enable_if_t<
        RAH2_NS::ranges::range<A> && RAH2_NS::ranges::range<B>
        && !RAH2_NS::is_same_v<RAH2_NS::remove_cvref_t<A>, RAH2_STD::string>
        && !RAH2_NS::is_same_v<RAH2_NS::remove_cvref_t<B>, RAH2_STD::string>>* = nullptr>
bool operator==(A&& a, B&& b)
{
    return RAH2_NS::ranges::equal(a, b);
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

#define DONT_OPTIM(V)                                                                              \
    memcpy((char*)&testSuite.avoid_optim, (char*)&V, RAH2_STD::min(sizeof(V), sizeof(size_t)))

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
    size_t count = 0;
    const auto start = std::chrono::high_resolution_clock::now();
    auto end = start;

    func();
    ++count;
    end = std::chrono::high_resolution_clock::now();
#ifdef PERF_TEST
    if ((end - start) < std::chrono::microseconds(100))
    {
        std::cerr << "Too short function (" << ((end - start) / count).count() << ") at " << file
                  << "(" << line << ")" << std::endl;
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
    (void)count;
    (void)algo;
    (void)range_type;
    (void)step;
    (void)func;
    (void)file;
    (void)line;
    return std::chrono::seconds(1);
#endif
}

constexpr static auto PerfTolerency = 50.;

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
#ifdef PERF_TEST
    std::chrono::nanoseconds duration_std{};
    std::chrono::nanoseconds duration_rah2{};
    for (size_t i = 0; i < 1; ++i)
    {
        duration_std += compute_duration(
            RAH2_STD::forward<F>(func_std_range), algo, range_type, ref_ns, file, line);
        duration_rah2 +=
            compute_duration(RAH2_STD::forward<F2>(func_rah2), algo, range_type, "rah2", file, line);
        if (duration_rah2 < (duration_std * PerfTolerency))
        {
            break;
        }
    }
    assert(duration_rah2 < (duration_std * PerfTolerency));
#else
    auto time1 =
        compute_duration(RAH2_STD::forward<F>(func_std_range), algo, range_type, ref_ns, file, line);
    DONT_OPTIM(time1);
    auto time2 =
        compute_duration(RAH2_STD::forward<F2>(func_rah2), algo, range_type, "rah2", file, line);
    DONT_OPTIM(time2);
#endif
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
    std::chrono::nanoseconds duration_std{};
    std::chrono::nanoseconds duration_std_ranges{};
    std::chrono::nanoseconds duration_rah2{};
    for (size_t i = 0; i < 1; ++i)
    {
        duration_std += compute_duration(
            RAH2_STD::forward<F>(func_std_algo), algo, range_type, "std", file, line);
        duration_std_ranges += compute_duration(
            RAH2_STD::forward<F2>(func_std_range), algo, range_type, "std::ranges", file, line);
        duration_rah2 +=
            compute_duration(RAH2_STD::forward<F3>(func_rah2), algo, range_type, "rah2", file, line);
        if (duration_rah2 < (duration_std * PerfTolerency)
            && duration_rah2 < (duration_std_ranges * PerfTolerency))
        {
            break;
        }
    }
    assert(duration_rah2 < (duration_std * PerfTolerency));
    assert(duration_rah2 < (duration_std_ranges * PerfTolerency));
}

#define COMPUTE_DURATION(ALGO, CONCEPT, STEP, F)                                                   \
    compute_duration((F), ALGO, CONCEPT, STEP, __FILE__, __LINE__)

#define INTERNAL_COMPARE_DURATION_TO_STD_ALGO_AND_RANGES_2(IS_COMMON, ALGO, CONCEPT, ALGO_F, RANGE_F) \
    call_if_true<IS_COMMON>(                                                                          \
        [&](auto fwd)                                                                                 \
        {                                                                                             \
            namespace STD = RAH2_STD;                                                                 \
            auto test_std = (ALGO_F);                                                                 \
            {                                                                                         \
                namespace STD = RAH2_STD::ranges;                                                     \
                auto test_std_ranges = (RANGE_F);                                                     \
                {                                                                                     \
                    namespace STD = RAH2_NS::ranges;                                                  \
                    auto test_rah2 = (RANGE_F);                                                       \
                    compare_duration(                                                                 \
                        test_std, test_std_ranges, test_rah2, ALGO, CONCEPT, __FILE__, __LINE__);     \
                }                                                                                     \
            }                                                                                         \
        });

#define INTERNAL_COMPARE_DURATION_TO_STD_ALGO_2(IS_COMMON, ALGO, CONCEPT, ALGO_F, RANGE_F)         \
    call_if_true<IS_COMMON>(                                                                       \
        [&](auto fwd)                                                                              \
        {                                                                                          \
            namespace STD = RAH2_STD;                                                              \
            auto test_std = (ALGO_F);                                                              \
            {                                                                                      \
                namespace STD = RAH2_NS::ranges;                                                   \
                auto test_rah2 = (RANGE_F);                                                        \
                compare_duration(test_std, test_rah2, ALGO, CONCEPT, "std", __FILE__, __LINE__);   \
            }                                                                                      \
        });

#define INTERNAL_COMPARE_DURATION_TO_NOTHING_5(IS_COMMON, ALGO, CONCEPT, ALGO_F, RANGE_F)          \
    do                                                                                             \
    {                                                                                              \
        (void)ALGO;                                                                                \
        (void)CONCEPT;                                                                             \
    } while (false)

#define INTERNAL_COMPARE_DURATION_TO_STD_ALGO_AND_RANGES(IS_COMMON, ALGO, CONCEPT, ALGO_F)         \
    call_if_true<IS_COMMON>(                                                                       \
        [&](auto fwd)                                                                              \
        {                                                                                          \
            namespace STD = RAH2_STD;                                                              \
            auto test_std = (ALGO_F);                                                              \
            {                                                                                      \
                namespace STD = RAH2_STD::ranges;                                                  \
                auto test_std_ranges = (ALGO_F);                                                   \
                {                                                                                  \
                    namespace STD = RAH2_NS::ranges;                                               \
                    auto test_rah2 = (ALGO_F);                                                     \
                    compare_duration(                                                              \
                        test_std, test_std_ranges, test_rah2, ALGO, CONCEPT, __FILE__, __LINE__);  \
                }                                                                                  \
            }                                                                                      \
        });                                                                                        \
    call_if_true<not(IS_COMMON)>(                                                                  \
        [&](auto fwd)                                                                              \
        {                                                                                          \
            namespace STD = RAH2_STD::ranges;                                                      \
            auto test_std_ranges = (ALGO_F);                                                       \
            {                                                                                      \
                namespace STD = RAH2_NS::ranges;                                                   \
                auto test_rah2 = (ALGO_F);                                                         \
                compare_duration(                                                                  \
                    test_std_ranges, test_rah2, ALGO, CONCEPT, "std::ranges", __FILE__, __LINE__); \
            }                                                                                      \
        });

#define INTERNAL_COMPARE_DURATION_TO_STD_ALGO(IS_COMMON, ALGO, CONCEPT, ALGO_F)                    \
    call_if_true<IS_COMMON>(                                                                       \
        [&](auto fwd)                                                                              \
        {                                                                                          \
            namespace STD = RAH2_STD;                                                              \
            auto test_std = (ALGO_F);                                                              \
            {                                                                                      \
                namespace STD = RAH2_NS::ranges;                                                   \
                auto test_rah2 = (ALGO_F);                                                         \
                compare_duration(test_std, test_rah2, ALGO, CONCEPT, "std", __FILE__, __LINE__);   \
            }                                                                                      \
        });

#define INTERNAL_COMPARE_DURATION_TO_NOTHING_4(IS_COMMON, ALGO, CONCEPT, ALGO_F)                   \
    do                                                                                             \
    {                                                                                              \
        Fwd fwd;                                                                                   \
        (void)fwd;                                                                                 \
        namespace STD = RAH2_NS::ranges;                                                           \
        auto test_rah2 = (ALGO_F);                                                                 \
        compute_duration(                                                                          \
            RAH2_STD::move(test_rah2), ALGO, CONCEPT, "rah2::ranges", __FILE__, __LINE__);         \
    } while (false)

#define INTERNAL_COMPARE_DURATION_TO_NOTHING_3(ALGO, CONCEPT, ALGO_F)                              \
    do                                                                                             \
    {                                                                                              \
        Fwd fwd;                                                                                   \
        (void)fwd;                                                                                 \
        namespace STD = RAH2_NS::ranges;                                                           \
        auto test_rah2 = (ALGO_F);                                                                 \
        compute_duration(                                                                          \
            RAH2_STD::move(test_rah2), ALGO, CONCEPT, "rah2::ranges", __FILE__, __LINE__);         \
    } while (false)

#define INTERNAL_COMPARE_DURATION_TO_STD_RANGES(ALGO, CONCEPT, ALGO_F)                             \
    do                                                                                             \
    {                                                                                              \
        namespace STD = RAH2_STD::ranges;                                                          \
        auto test_std_ranges = (ALGO_F);                                                           \
        {                                                                                          \
            namespace STD = RAH2_NS::ranges;                                                       \
            auto test_rah2 = (ALGO_F);                                                             \
            compare_duration(                                                                      \
                test_std_ranges, test_rah2, ALGO, CONCEPT, "std::ranges", __FILE__, __LINE__);     \
        }                                                                                          \
    } while (false)

// #if defined(PERF_TEST)
#if RAH2_CPP23 && !defined(RAH2_USE_EASTL)
#define COMPARE_DURATION_TO_STD_ALGO_AND_RANGES INTERNAL_COMPARE_DURATION_TO_STD_ALGO_AND_RANGES
#define COMPARE_DURATION_TO_STD_RANGES_23 INTERNAL_COMPARE_DURATION_TO_STD_RANGES
#define COMPARE_DURATION_TO_STD_RANGES INTERNAL_COMPARE_DURATION_TO_STD_RANGES
#define COMPARE_DURATION_TO_STD_ALGO_17_AND_RANGES_2                                               \
    INTERNAL_COMPARE_DURATION_TO_STD_ALGO_AND_RANGES_2
#define COMPARE_DURATION_TO_STD_ALGO_17_AND_RANGES INTERNAL_COMPARE_DURATION_TO_STD_ALGO_AND_RANGES
#define COMPARE_DURATION_TO_STD_ALGO_20_AND_RANGES_23                                              \
    INTERNAL_COMPARE_DURATION_TO_STD_ALGO_AND_RANGES
#elif RAH2_CPP20 && !defined(RAH2_USE_EASTL)
#define COMPARE_DURATION_TO_STD_ALGO_AND_RANGES INTERNAL_COMPARE_DURATION_TO_STD_ALGO_AND_RANGES
#define COMPARE_DURATION_TO_STD_RANGES_23 INTERNAL_COMPARE_DURATION_TO_NOTHING_3
#define COMPARE_DURATION_TO_STD_RANGES INTERNAL_COMPARE_DURATION_TO_STD_RANGES
#define COMPARE_DURATION_TO_STD_ALGO_17_AND_RANGES_2                                               \
    INTERNAL_COMPARE_DURATION_TO_STD_ALGO_AND_RANGES_2
#define COMPARE_DURATION_TO_STD_ALGO_17_AND_RANGES INTERNAL_COMPARE_DURATION_TO_STD_ALGO_AND_RANGES
#define COMPARE_DURATION_TO_STD_ALGO_20_AND_RANGES_23 INTERNAL_COMPARE_DURATION_TO_STD_ALGO
#elif RAH2_CPP17 && !defined(RAH2_USE_EASTL)
#define COMPARE_DURATION_TO_STD_ALGO_AND_RANGES INTERNAL_COMPARE_DURATION_TO_STD_ALGO
#define COMPARE_DURATION_TO_STD_RANGES_23 INTERNAL_COMPARE_DURATION_TO_NOTHING_3
#define COMPARE_DURATION_TO_STD_RANGES INTERNAL_COMPARE_DURATION_TO_NOTHING_3
#define COMPARE_DURATION_TO_STD_ALGO_17_AND_RANGES_2 INTERNAL_COMPARE_DURATION_TO_STD_ALGO_2
#define COMPARE_DURATION_TO_STD_ALGO_17_AND_RANGES INTERNAL_COMPARE_DURATION_TO_STD_ALGO
#define COMPARE_DURATION_TO_STD_ALGO_20_AND_RANGES_23 INTERNAL_COMPARE_DURATION_TO_NOTHING_4
#else
#define COMPARE_DURATION_TO_STD_ALGO_AND_RANGES INTERNAL_COMPARE_DURATION_TO_STD_ALGO
#define COMPARE_DURATION_TO_STD_RANGES_23 INTERNAL_COMPARE_DURATION_TO_NOTHING_3
#define COMPARE_DURATION_TO_STD_RANGES INTERNAL_COMPARE_DURATION_TO_NOTHING_3
#define COMPARE_DURATION_TO_STD_ALGO_17_AND_RANGES_2 INTERNAL_COMPARE_DURATION_TO_NOTHING_5
#define COMPARE_DURATION_TO_STD_ALGO_17_AND_RANGES INTERNAL_COMPARE_DURATION_TO_NOTHING_4
#define COMPARE_DURATION_TO_STD_ALGO_20_AND_RANGES_23 INTERNAL_COMPARE_DURATION_TO_NOTHING_4
#endif
//#else
//#define COMPARE_DURATION_TO_STD_ALGO_AND_RANGES INTERNAL_COMPARE_DURATION_TO_NOTHING_4
//#define COMPARE_DURATION_TO_STD_RANGES_23 INTERNAL_COMPARE_DURATION_TO_NOTHING_3
//#define COMPARE_DURATION_TO_STD_RANGES INTERNAL_COMPARE_DURATION_TO_NOTHING_3
//#define COMPARE_DURATION_TO_STD_ALGO_17_AND_RANGES_2 INTERNAL_COMPARE_DURATION_TO_NOTHING_5
//#define COMPARE_DURATION_TO_STD_ALGO_17_AND_RANGES INTERNAL_COMPARE_DURATION_TO_NOTHING_4
//#endif

struct Coord
{
    intptr_t x;
    intptr_t y;

    bool operator==(Coord c) const
    {
        return x == c.x and y == c.y;
    }
    bool operator!=(Coord c) const
    {
        return x != c.x || y != c.y;
    }

    friend bool operator<(Coord a, Coord b)
    {
        return (a.x != b.x) ? (a.x < b.x) : (a.y < b.y);
    }

    friend bool operator<=(Coord a, Coord b)
    {
        return (a < b) || (a == b);
    }

    friend bool operator>(Coord a, Coord b)
    {
        return !(a < b) && !(a == b);
    }

    friend bool operator>=(Coord a, Coord b)
    {
        return !(a < b);
    }

    friend std::ostream& operator<<(std::ostream& os, Coord const& c)
    {
        os << "(" << c.x << "," << c.y << ")";
        return os;
    }
};

struct Complex
{
    intptr_t x;
    intptr_t y;

    Complex& operator=(Coord c)
    {
        x = c.x;
        y = c.y;
        return *this;
    }

    /*
    Complex(Coord c)
        : x(c.x)
        , y(c.y)
    {
    }
    */

    bool operator==(Complex c) const
    {
        return x == c.x and y == c.y;
    }
    bool operator!=(Complex c) const
    {
        return x != c.x || y != c.y;
    }

    friend bool operator<(Complex a, Complex b)
    {
        return (a.x != b.x) ? (a.x < b.x) : (a.y < b.y);
    }

    friend bool operator<=(Complex a, Complex b)
    {
        return (a < b) || (a == b);
    }

    friend bool operator>(Complex a, Complex b)
    {
        return !(a < b) && !(a == b);
    }

    friend bool operator>=(Complex a, Complex b)
    {
        return !(a < b);
    }

    friend std::ostream& operator<<(std::ostream& os, Complex const& c)
    {
        os << "(" << c.x << "," << c.y << ")";
        return os;
    }
};
