#pragma once

#include "range_bases.hpp"
#include "algo_sort.hpp"

#include <random>

namespace RAH2_NAMESPACE
{
    struct fold_left_fn
    {
        template <typename I, typename S, class T, typename F>
        constexpr auto operator()(I first, S last, T init, F f) const
        {
            using U = RAH2_NAMESPACE::remove_cvref_t<decltype(f(init, *first))>;
            if (first == last)
                return U(RAH2_STD::move(init));
            U accum = RAH2_INVOKE_2(f, RAH2_STD::move(init), *first);
            for (++first; first != last; ++first)
                accum = RAH2_INVOKE_2(f, RAH2_STD::move(accum), *first);
            return RAH2_STD::move(accum);
        }

        template <typename R, class T, typename F>
        constexpr auto operator()(R&& r, T init, F f) const
        {
            return (*this)(
                RAH2_NAMESPACE::begin(r),
                RAH2_NAMESPACE::end(r),
                RAH2_STD::move(init),
                RAH2_STD::ref(f));
        }
    };

    constexpr fold_left_fn fold_left;

    struct fold_left_first_fn
    {
        template <
            typename I, // RAH2_STD::input_iterator
            typename S, // RAH2_STD::sentinel_for<I>
            typename F // __indirectly_binary_left_foldable<RAH2_STD::iter_value_t<I>, I>
            >
        // requires RAH2_STD::constructible_from<RAH2_STD::iter_value_t<I>, RAH2_STD::iter_reference_t<I>>
        constexpr auto operator()(I first, S last, F f) const
        {
            using U = decltype(RAH2_NAMESPACE::fold_left(
                RAH2_STD::move(first), last, RAH2_NAMESPACE::iter_value_t<I>(*first), f));
            if (first == last)
                return RAH2_NAMESPACE::details::optional<U>();
            RAH2_NAMESPACE::details::optional<U> init(RAH2_STD::move(*first));
            for (++first; first != last; ++first)
                *init = RAH2_INVOKE_2(f, RAH2_STD::move(*init), *first);
            return RAH2_STD::move(init);
        }

        template <
            typename R, // input_range
            typename F // __indirectly_binary_left_foldable<ranges::range_value_t<R>, ranges::iterator_t<R>>
            >
        // requires RAH2_STD::constructible_from<ranges::range_value_t<R>, ranges::range_reference_t<R>>
        constexpr auto operator()(R&& r, F f) const
        {
            return (*this)(RAH2_NAMESPACE::begin(r), RAH2_NAMESPACE::end(r), RAH2_STD::ref(f));
        }
    };

    constexpr fold_left_first_fn fold_left_first;

    struct fold_right_fn
    {
        template <
            typename I, // RAH2_STD::bidirectional_iterator
            typename S, // RAH2_STD::sentinel_for<I>
            class T,
            typename F // __indirectly_binary_right_foldable<T, I>
            >
        constexpr auto operator()(I first, S last, T init, F f) const
        {
            using U = RAH2_STD::decay_t<decltype(RAH2_INVOKE_2(f, *first, RAH2_STD::move(init)))>;
            if (first == last)
                return U(RAH2_STD::move(init));
            I tail = RAH2_NAMESPACE::next(first, last);
            U accum = RAH2_INVOKE_2(f, *--tail, RAH2_STD::move(init));
            while (first != tail)
                accum = RAH2_INVOKE_2(f, *--tail, RAH2_STD::move(accum));
            return accum;
        }

        template <
            typename R, // RAH2_NAMESPACE::bidirectional_range
            class T,
            typename F // __indirectly_binary_right_foldable<T, ranges::iterator_t<R>>
            >
        constexpr auto operator()(R&& r, T init, F f) const
        {
            return (*this)(
                RAH2_NAMESPACE::begin(r),
                RAH2_NAMESPACE::end(r),
                RAH2_STD::move(init),
                RAH2_STD::ref(f));
        }
    };

    constexpr fold_right_fn fold_right;

    struct fold_right_last_fn
    {
        template <
            typename I, // RAH2_STD::bidirectional_iterator
            typename S, // RAH2_STD::sentinel_for<I>
            typename F // __indirectly_binary_right_foldable<RAH2_STD::iter_value_t<I>, I>
            >
        // requires RAH2_STD::constructible_from<RAH2_STD::iter_value_t<I>, RAH2_STD::iter_reference_t<I>>
        constexpr auto operator()(I first, S last, F f) const
        {
            using U = decltype(RAH2_NAMESPACE::fold_right(
                first, last, RAH2_NAMESPACE::iter_value_t<I>(*first), f));

            if (first == last)
                return RAH2_NAMESPACE::details::optional<U>();
            I tail = RAH2_STD::prev(RAH2_NAMESPACE::next(first, RAH2_STD::move(last)));
            return RAH2_NAMESPACE::details::optional<U>(RAH2_NAMESPACE::fold_right(
                RAH2_STD::move(first), tail, RAH2_NAMESPACE::iter_value_t<I>(*tail), RAH2_STD::move(f)));
        }

        template <
            typename R, // ranges::bidirectional_range
            typename F // __indirectly_binary_right_foldable<ranges::range_value_t<R>, ranges::iterator_t<R>>
            >
        // requires RAH2_STD::constructible_from<ranges::range_value_t<R>, ranges::range_reference_t<R>>
        constexpr auto operator()(R&& r, F f) const
        {
            return (*this)(RAH2_NAMESPACE::begin(r), RAH2_NAMESPACE::end(r), RAH2_STD::ref(f));
        }
    };

    constexpr fold_right_last_fn fold_right_last;

    template <class I, class T>
    using fold_left_with_iter_result = RAH2_NAMESPACE::in_value_result<I, T>;

    class fold_left_with_iter_fn
    {
        template <class O, class I, class S, class T, class F>
        constexpr auto impl(I&& first, S&& last, T&& init, F f) const
        {
            using U = RAH2_STD::decay_t<decltype(RAH2_INVOKE_2(f, RAH2_STD::move(init), *first))>;
            using Ret = RAH2_NAMESPACE::fold_left_with_iter_result<O, U>;
            if (first == last)
                return Ret{RAH2_STD::move(first), U(RAH2_STD::move(init))};
            U accum = RAH2_INVOKE_2(f, RAH2_STD::move(init), *first);
            for (++first; first != last; ++first)
                accum = RAH2_INVOKE_2(f, RAH2_STD::move(accum), *first);
            return Ret{RAH2_STD::move(first), RAH2_STD::move(accum)};
        }

    public:
        template <
            typename I, // RAH2_STD::input_iterator
            typename S, // RAH2_STD::sentinel_for<I>
            class T,
            typename F // __indirectly_binary_left_foldable<T, I>
            >
        constexpr auto operator()(I first, S last, T init, F f) const
        {
            return impl<I>(
                RAH2_STD::move(first), RAH2_STD::move(last), RAH2_STD::move(init), RAH2_STD::ref(f));
        }

        template <
            typename R, // ranges::input_range
            class T,
            typename F // __indirectly_binary_left_foldable<T, ranges::iterator_t<R>>
            >
        constexpr auto operator()(R&& r, T init, F f) const
        {
            return impl<RAH2_NAMESPACE::borrowed_iterator_t<R>>(
                RAH2_NAMESPACE::begin(r),
                RAH2_NAMESPACE::end(r),
                RAH2_STD::move(init),
                RAH2_STD::ref(f));
        }
    };

    constexpr fold_left_with_iter_fn fold_left_with_iter;

    template <class I, class T>
    using fold_left_first_with_iter_result = RAH2_NAMESPACE::in_value_result<I, T>;

    class fold_left_first_with_iter_fn
    {
        template <class O, class I, class S, class F>
        constexpr auto impl(I&& first, S&& last, F f) const
        {
            using U = decltype(RAH2_NAMESPACE::fold_left(
                RAH2_STD::move(first), last, RAH2_NAMESPACE::iter_value_t<I>(*first), f));
            using Ret =
                RAH2_NAMESPACE::fold_left_first_with_iter_result<O, RAH2_NAMESPACE::details::optional<U>>;
            if (first == last)
                return Ret{RAH2_STD::move(first), RAH2_NAMESPACE::details::optional<U>()};
            RAH2_NAMESPACE::details::optional<U> init(RAH2_NAMESPACE::in_place, *first);
            for (++first; first != last; ++first)
                *init = RAH2_INVOKE_2(f, RAH2_STD::move(*init), *first);
            return Ret{RAH2_STD::move(first), RAH2_STD::move(init)};
        }

    public:
        template <
            typename I, // RAH2_STD::input_iterator
            typename S, // RAH2_STD::sentinel_for<I>
            typename F // __indirectly_binary_left_foldable<RAH2_STD::iter_value_t<I>, I>
            >
        // requires RAH2_STD::constructible_from<RAH2_STD::iter_value_t<I>, RAH2_STD::iter_reference_t<I>>
        constexpr auto operator()(I first, S last, F f) const
        {
            return impl<I>(RAH2_STD::move(first), RAH2_STD::move(last), RAH2_STD::ref(f));
        }

        template <
            typename R, // ranges::input_range
            typename F // __indirectly_binary_left_foldable<ranges::range_value_t<R>, ranges::iterator_t<R>>
            >
        // requires RAH2_STD::constructible_from<ranges::range_value_t<R>, ranges::range_reference_t<R>>
        constexpr auto operator()(R&& r, F f) const
        {
            return impl<RAH2_NAMESPACE::borrowed_iterator_t<R>>(
                RAH2_NAMESPACE::begin(r), RAH2_NAMESPACE::end(r), RAH2_STD::ref(f));
        }
    };

    constexpr fold_left_first_with_iter_fn fold_left_first_with_iter;

    struct find_last_fn
    {
        template <
            typename I,
            typename S,
            class T,
            class Proj = RAH2_NAMESPACE::identity,
            RAH2_STD::enable_if_t<forward_iterator<I> && sentinel_for<S, I>>* = nullptr>
        constexpr RAH2_NAMESPACE::subrange<I>
        operator()(I first, S last, const T& value, Proj proj = {}) const
        {
            // Note: if I is mere forward_iterator, we may only go from begin to end.
            I found{};
            for (; first != last; ++first)
                if (RAH2_INVOKE_1(proj, *first) == value)
                    found = first;

            if (found == I{})
                return {first, first};

            return {found, RAH2_NAMESPACE::next(found, last)};
        }

        template <
            typename R,
            class T,
            class Proj = RAH2_NAMESPACE::identity,
            RAH2_STD::enable_if_t<forward_range<R>>* = nullptr>
        constexpr RAH2_NAMESPACE::borrowed_subrange_t<R>
        operator()(R&& r, const T& value, Proj proj = {}) const
        {
            return this->operator()(
                RAH2_NAMESPACE::begin(r), RAH2_NAMESPACE::end(r), value, RAH2_STD::ref(proj));
        }
    };

    constexpr find_last_fn find_last;

    struct find_last_if_fn
    {
        template <
            typename I,
            typename S,
            class Proj = RAH2_NAMESPACE::identity,
            typename Pred,
            RAH2_STD::enable_if_t<forward_iterator<I> && sentinel_for<S, I>>* = nullptr>
        constexpr RAH2_NAMESPACE::subrange<I> operator()(I first, S last, Pred pred, Proj proj = {}) const
        {
            // Note: if I is mere forward_iterator, we may only go from begin to end.
            I found{};
            for (; first != last; ++first)
                if (RAH2_INVOKE_1(pred, RAH2_INVOKE_1(proj, *first)))
                    found = first;

            if (found == I{})
                return {first, first};

            return {found, RAH2_NAMESPACE::next(found, last)};
        }

        template <
            typename R,
            class Proj = RAH2_NAMESPACE::identity,
            typename Pred,
            RAH2_STD::enable_if_t<forward_range<R>>* = nullptr>
        constexpr RAH2_NAMESPACE::borrowed_subrange_t<R> operator()(R&& r, Pred pred, Proj proj = {}) const
        {
            return this->operator()(
                RAH2_NAMESPACE::begin(r),
                RAH2_NAMESPACE::end(r),
                RAH2_STD::ref(pred),
                RAH2_STD::ref(proj));
        }
    };

    constexpr find_last_if_fn find_last_if;

    struct find_last_if_not_fn
    {
        template <
            typename I,
            typename S,
            class Proj = RAH2_NAMESPACE::identity,
            typename Pred,
            RAH2_STD::enable_if_t<forward_iterator<I> && sentinel_for<S, I>>* = nullptr>
        constexpr RAH2_NAMESPACE::subrange<I> operator()(I first, S last, Pred pred, Proj proj = {}) const
        {
            // Note: if I is mere forward_iterator, we may only go from begin to end.
            I found{};
            for (; first != last; ++first)
                if (!RAH2_INVOKE_1(pred, RAH2_INVOKE_1(proj, *first)))
                    found = first;

            if (found == I{})
                return {first, first};

            return {found, RAH2_NAMESPACE::next(found, last)};
        }

        template <
            typename R,
            class Proj = RAH2_NAMESPACE::identity,
            typename Pred,
            RAH2_STD::enable_if_t<forward_range<R>>* = nullptr>
        constexpr RAH2_NAMESPACE::borrowed_subrange_t<R> operator()(R&& r, Pred pred, Proj proj = {}) const
        {
            return this->operator()(
                RAH2_NAMESPACE::begin(r),
                RAH2_NAMESPACE::end(r),
                RAH2_STD::ref(pred),
                RAH2_STD::ref(proj));
        }
    };

    constexpr find_last_if_not_fn find_last_if_not;

    struct __contains_fn
    {
        template <
            typename I,
            typename S,
            class T,
            class Proj = RAH2_NAMESPACE::identity,
            RAH2_STD::enable_if_t<input_iterator<I> && sentinel_for<S, I>>* = nullptr>
        constexpr bool operator()(I first, S last, const T& value, Proj proj = {}) const
        {
            return RAH2_NAMESPACE::find(RAH2_STD::move(first), last, value, proj) != last;
        }

        template <
            typename R,
            class T,
            class Proj = RAH2_NAMESPACE::identity,
            RAH2_STD::enable_if_t<input_range<R>>* = nullptr>
        constexpr bool operator()(R&& r, const T& value, Proj proj = {}) const
        {
            return (*this)(
                RAH2_NAMESPACE::begin(r), RAH2_NAMESPACE::end(r), RAH2_STD::move(value), proj);
        }
    };

    constexpr __contains_fn contains{};

    struct __contains_subrange_fn
    {
        template <
            typename I1,
            typename S1,
            typename I2,
            typename S2,
            class Pred = RAH2_NAMESPACE::equal_to,
            class Proj1 = RAH2_NAMESPACE::identity,
            class Proj2 = RAH2_NAMESPACE::identity,
            RAH2_STD::enable_if_t<
                forward_iterator<I1> && sentinel_for<S1, I1> && forward_iterator<I2>
                && sentinel_for<S2, I2>>* = nullptr>
        constexpr bool operator()(
            I1 first1, S1 last1, I2 first2, S2 last2, Pred pred = {}, Proj1 proj1 = {}, Proj2 proj2 = {}) const
        {
            return (first2 == last2)
                   || !RAH2_NAMESPACE::search(first1, last1, first2, last2, pred, proj1, proj2).empty();
        }

        template <
            typename R1,
            typename R2,
            class Pred = RAH2_NAMESPACE::equal_to,
            class Proj1 = RAH2_NAMESPACE::identity,
            class Proj2 = RAH2_NAMESPACE::identity,
            RAH2_STD::enable_if_t<forward_range<R1> && forward_range<R2>>* = nullptr>
        constexpr bool
        operator()(R1&& r1, R2&& r2, Pred pred = {}, Proj1 proj1 = {}, Proj2 proj2 = {}) const
        {
            return (*this)(
                RAH2_NAMESPACE::begin(r1),
                RAH2_NAMESPACE::end(r1),
                RAH2_NAMESPACE::begin(r2),
                RAH2_NAMESPACE::end(r2),
                RAH2_STD::move(pred),
                RAH2_STD::move(proj1),
                RAH2_STD::move(proj2));
        }
    };

    constexpr __contains_subrange_fn contains_subrange{};

    struct starts_with_fn
    {
        template <
            typename I1, // RAH2_STD::input_iterator
            typename S1, // RAH2_STD::sentinel_for<I1>
            typename I2, // RAH2_STD::input_iterator
            typename S2, // RAH2_STD::sentinel_for<I2>
            class Pred = RAH2_NAMESPACE::equal_to,
            class Proj1 = RAH2_NAMESPACE::identity,
            class Proj2 = RAH2_NAMESPACE::identity,
            RAH2_STD::enable_if_t<
                input_iterator<I1> && sentinel_for<S1, I1> && input_iterator<I2>
                && sentinel_for<S2, I2>>* = nullptr>
        constexpr bool operator()(
            I1 first1, S1 last1, I2 first2, S2 last2, Pred pred = {}, Proj1 proj1 = {}, Proj2 proj2 = {}) const
        {
            return RAH2_NAMESPACE::mismatch(
                       RAH2_STD::move(first1),
                       last1,
                       RAH2_STD::move(first2),
                       last2,
                       RAH2_STD::move(pred),
                       RAH2_STD::move(proj1),
                       RAH2_STD::move(proj2))
                       .in2
                   == last2;
        }

        template <
            typename R1, // ranges::input_range
            typename R2, // ranges::input_range
            class Pred = RAH2_NAMESPACE::equal_to,
            class Proj1 = RAH2_NAMESPACE::identity,
            class Proj2 = RAH2_NAMESPACE::identity>
        // requires RAH2_STD::indirectly_comparable<ranges::iterator_t<R1>, ranges::iterator_t<R2>, Pred, Proj1, Proj2>
        constexpr bool
        operator()(R1&& r1, R2&& r2, Pred pred = {}, Proj1 proj1 = {}, Proj2 proj2 = {}) const
        {
            return (*this)(
                RAH2_NAMESPACE::begin(r1),
                RAH2_NAMESPACE::end(r1),
                RAH2_NAMESPACE::begin(r2),
                RAH2_NAMESPACE::end(r2),
                RAH2_STD::move(pred),
                RAH2_STD::move(proj1),
                RAH2_STD::move(proj2));
        }
    };

    constexpr starts_with_fn starts_with{};

    struct ends_with_fn
    {
        template <
            typename I1, // RAH2_STD::input_iterator
            typename S1, // RAH2_STD::sentinel_for<I1>
            typename I2, // RAH2_STD::input_iterator
            typename S2, // RAH2_STD::sentinel_for<I2>
            class Pred = RAH2_NAMESPACE::equal_to,
            RAH2_STD::enable_if_t<
                (RAH2_NAMESPACE::forward_iterator<I1> || RAH2_NAMESPACE::sized_sentinel_for<S1, I1>)&&(
                    RAH2_NAMESPACE::forward_iterator<I2> || RAH2_NAMESPACE::sized_sentinel_for<S2, I2>)>* = nullptr>
        constexpr bool operator()(I1 first1, S1 last1, I2 first2, S2 last2, Pred pred = {}) const
        {
            const auto n1 = RAH2_NAMESPACE::distance(first1, last1);
            const auto n2 = RAH2_NAMESPACE::distance(first2, last2);
            if (n1 < n2)
                return false;
            RAH2_NAMESPACE::advance(first1, n1 - n2);
            return RAH2_NAMESPACE::equal(
                RAH2_STD::move(first1),
                RAH2_STD::move(last1),
                RAH2_STD::move(first2),
                RAH2_STD::move(last2),
                RAH2_STD::move(pred));
        }

        template <
            typename R1, // ranges::input_range
            typename R2, // ranges::input_range
            class Pred = RAH2_NAMESPACE::equal_to,
            RAH2_STD::enable_if_t<(RAH2_NAMESPACE::forward_range<R1> || RAH2_NAMESPACE::sized_range<R1>)&&(
                RAH2_NAMESPACE::forward_range<R2> || RAH2_NAMESPACE::sized_range<R2>)>* = nullptr>
        constexpr bool operator()(R1&& r1, R2&& r2, Pred pred = {}) const
        {
            return (*this)(
                RAH2_NAMESPACE::begin(r1),
                RAH2_NAMESPACE::end(r1),
                RAH2_NAMESPACE::begin(r2),
                RAH2_NAMESPACE::end(r2),
                RAH2_STD::move(pred));
        }
    };

    constexpr ends_with_fn ends_with{};

    template <class I1, class I2>
    using swap_ranges_result = RAH2_NAMESPACE::in_in_result<I1, I2>;

    struct swap_ranges_fn
    {
        template <
            typename I1,
            typename S1,
            typename I2,
            typename S2,
            RAH2_STD::enable_if_t<
                input_iterator<I1> && sentinel_for<S1, I1> && input_iterator<I2>
                && sentinel_for<S2, I2> && indirectly_swappable<I1, I2>>* = nullptr>
        constexpr RAH2_NAMESPACE::swap_ranges_result<I1, I2>
        operator()(I1 first1, S1 last1, I2 first2, S2 last2) const
        {
            for (; !(first1 == last1 or first2 == last2); ++first1, ++first2)
                RAH2_NAMESPACE::iter_swap(first1, first2);
            return {RAH2_STD::move(first1), RAH2_STD::move(first2)};
        }

        template <
            typename R1,
            typename R2,
            RAH2_STD::enable_if_t<
                input_range<R1> && input_range<R2>
                && indirectly_swappable<RAH2_NAMESPACE::iterator_t<R1>, RAH2_NAMESPACE::iterator_t<R2>>>* = nullptr>
        RAH2_NAMESPACE::swap_ranges_result<
            RAH2_NAMESPACE::borrowed_iterator_t<R1>,
            RAH2_NAMESPACE::borrowed_iterator_t<R2>>
        operator()(R1&& r1, R2&& r2) const
        {
            return (*this)(
                RAH2_NAMESPACE::begin(r1),
                RAH2_NAMESPACE::end(r1),
                RAH2_NAMESPACE::begin(r2),
                RAH2_NAMESPACE::end(r2));
        }
    };

    constexpr swap_ranges_fn swap_ranges{};

    struct shift_left_fn
    {
        template <typename I, typename S, RAH2_STD::enable_if_t<permutable<I> && sentinel_for<S, I>>* = nullptr>
        constexpr RAH2_NAMESPACE::subrange<I> operator()(I first, S last, iter_difference_t<I> n) const
        {
            if (n <= 0)
                return {RAH2_STD::move(first), RAH2_STD::move(last)};
            auto mid = first;
            auto diff = RAH2_NAMESPACE::advance(first, n, last);
            if (diff != 0)
            {
                return {RAH2_STD::move(mid), RAH2_STD::move(last)};
            }
            auto result = RAH2_NAMESPACE::move(first, last, mid);
            return {RAH2_STD::move(mid), RAH2_STD::move(result.out)};
        }

        template <typename R, RAH2_STD::enable_if_t<forward_range<R> && permutable<iterator_t<R>>>* = nullptr>
        constexpr borrowed_subrange_t<R> operator()(R&& r, range_difference_t<R> n) const
        {
            return (*this)(RAH2_NAMESPACE::begin(r), RAH2_NAMESPACE::end(r), n);
        }
    };

    constexpr shift_left_fn shift_left{};

    struct shift_right_fn
    {
        template <
            typename I,
            typename S,
            RAH2_STD::enable_if_t<permutable<I> && sentinel_for<S, I> && bidirectional_iterator<S>>* = nullptr>
        static constexpr RAH2_NAMESPACE::subrange<I> impl(I first, S last, iter_difference_t<I> n)
        {
            if (n <= 0)
            {
                return {RAH2_STD::move(first), RAH2_STD::move(last)};
            }
            auto mid = last;
            if (RAH2_NAMESPACE::advance(mid, -n, first) != 0)
            {
                return {RAH2_STD::move(first), RAH2_STD::move(last)};
            }
            auto result = RAH2_NAMESPACE::move_backward(
                RAH2_STD::move(first), RAH2_STD::move(mid), RAH2_STD::move(last));
            return {RAH2_STD::move(result.out), RAH2_STD::move(last)};
        }

        template <
            typename I,
            typename S,
            RAH2_STD::enable_if_t<
                permutable<I> && sentinel_for<S, I> && bidirectional_iterator<I>
                && RAH2_NAMESPACE::assignable_from<I&, S>>* = nullptr>
        constexpr RAH2_NAMESPACE::subrange<I> operator()(I first, S last, iter_difference_t<I> n) const
        {
            auto last2 = first;
            last2 = last;
            return shift_right_fn::impl(first, last2, n);
        }
        template <
            typename I,
            typename S,
            RAH2_STD::enable_if_t<
                permutable<I> && sized_sentinel_for<S, I>
                && !(bidirectional_iterator<I> && assignable_from<I&, S>)&&random_access_iterator<I>>* = nullptr>
        constexpr RAH2_NAMESPACE::subrange<I> operator()(I first, S last, iter_difference_t<I> n) const
        {
            auto last2 = first + (last - first);
            return shift_right_fn::impl(first, last2, n);
        }
        template <
            typename I,
            typename S,
            RAH2_STD::enable_if_t<
                permutable<I> && sentinel_for<S, I>
                && !(bidirectional_iterator<I> && assignable_from<I&, S>)&&!(
                    sized_sentinel_for<S, I> && random_access_iterator<I>)>* = nullptr>
        constexpr RAH2_NAMESPACE::subrange<I> operator()(I first, S last, iter_difference_t<I> n) const
        {
            if (n <= 0)
            {
                return first;
            }
            auto result = first;
            if (RAH2_NAMESPACE::advance(result, n, last) != 0)
            {
                return last;
            }

            // Invariant: next(first, n) == result
            // Invariant: next(trail, n) == lead

            auto lead = result;
            auto trail = first;

            for (; trail != result; ++lead, void(++trail))
            {
                if (lead == last)
                {
                    // The range looks like:
                    //
                    //   |-- (n - k) elements --|-- k elements --|-- (n - k) elements --|
                    //   ^-first          trail-^                ^-result          last-^
                    //
                    // Note that distance(first, trail) == distance(result, last)
                    auto move_in_out = RAH2_NAMESPACE::move(
                        RAH2_STD::move(first), RAH2_STD::move(trail), RAH2_STD::move(result));
                    return {move_in_out.out, result};
                }
            }

            for (;;)
            {
                for (auto mid = first; mid != result; ++lead, void(++trail), ++mid)
                {
                    if (lead == last)
                    {
                        // The range looks like:
                        //
                        //   |-- (n - k) elements --|-- k elements --|-- ... --|-- n elements --|
                        //   ^-first            mid-^         result-^         ^-trail     last-^
                        //
                        trail = RAH2_STD::move(mid, result, RAH2_STD::move(trail));
                        auto move_in_out = RAH2_NAMESPACE::move(
                            RAH2_STD::move(first), RAH2_STD::move(mid), RAH2_STD::move(trail));
                        return {move_in_out.out, trail};
                    }
                    RAH2_STD::iter_swap(mid, trail);
                }
            }
        }

        template <typename R, RAH2_STD::enable_if_t<forward_range<R> && permutable<iterator_t<R>>>* = nullptr>
        constexpr borrowed_subrange_t<R> operator()(R&& r, range_difference_t<R> n) const
        {
            return (*this)(RAH2_NAMESPACE::begin(r), RAH2_NAMESPACE::end(r), n);
        }
    };

    constexpr shift_right_fn shift_right{};

    struct sample_fn
    {
        template <
            typename I,
            typename S,
            typename O,
            class Gen,
            RAH2_STD::enable_if_t<!RAH2_NAMESPACE::forward_iterator<I>>* = nullptr>
        O operator()(I first, S last, O out, RAH2_NAMESPACE::iter_difference_t<I> n, Gen&& gen) const
        {
            using diff_t = RAH2_NAMESPACE::iter_difference_t<I>;
            using distrib_t = RAH2_STD::uniform_int_distribution<diff_t>;
            using param_t = typename distrib_t::param_type;
            distrib_t D{};

            // O is a random_access_iterator
            diff_t sample_size{};
            // copy [first, first + M) elements to "random access" output
            for (; first != last && sample_size != n; ++first)
                out[sample_size++] = *first;
            // overwrite some of the copied elements with randomly selected ones
            for (auto pop_size{sample_size}; first != last; ++first, ++pop_size)
            {
                const auto i{D(gen, param_t{0, pop_size})};
                if (i < n)
                    out[i] = *first;
            }
            return out + sample_size;
        }

        template <
            typename I,
            typename S,
            typename O,
            class Gen,
            RAH2_STD::enable_if_t<RAH2_NAMESPACE::forward_iterator<I>>* = nullptr>
        O operator()(I first, S last, O out, RAH2_NAMESPACE::iter_difference_t<I> n, Gen&& gen) const
        {
            using diff_t = RAH2_NAMESPACE::iter_difference_t<I>;
            using distrib_t = RAH2_STD::uniform_int_distribution<diff_t>;
            using param_t = typename distrib_t::param_type;
            distrib_t D{};

            // this branch preserves "stability" of the sample elements
            auto rest{RAH2_NAMESPACE::distance(first, last)};
            for (n = RAH2_STD::min(n, rest); n != 0; ++first)
            {
                if (D(gen, param_t(0, --rest)) < n)
                {
                    *out++ = *first;
                    --n;
                }
            }
            return out;
        }

        template <typename R, typename O, class Gen>
        O operator()(R&& r, O out, RAH2_NAMESPACE::range_difference_t<R> n, Gen&& gen) const
        {
            return (*this)(
                RAH2_NAMESPACE::begin(r),
                RAH2_NAMESPACE::end(r),
                RAH2_STD::move(out),
                n,
                RAH2_STD::forward<Gen>(gen));
        }
    };

    constexpr sample_fn sample{};

    template <class I, class O>
    using unique_copy_result = RAH2_NAMESPACE::in_out_result<I, O>;

    struct unique_copy_fn
    {
        template <
            typename I, // RAH2_STD::input_iterator
            typename S, // RAH2_STD::sentinel_for<I>
            typename O, // RAH2_STD::weakly_incrementable
            typename C = RAH2_NAMESPACE::equal_to, // RAH2_STD::indirect_equivalence_relation<RAH2_STD::projected<I, Proj>>
            RAH2_STD::enable_if_t<input_iterator<I> && sentinel_for<S, I>>* = nullptr>
        //requires RAH2_STD::indirectly_copyable<I, O> &&(
        //    RAH2_STD::forward_iterator<
        //        I> or (RAH2_STD::input_iterator<O> && RAH2_STD::same_as<RAH2_STD::iter_value_t<I>, RAH2_STD::iter_value_t<O>>)
        //    or RAH2_STD::indirectly_copyable_storable<I, O>)
        constexpr RAH2_NAMESPACE::unique_copy_result<I, O>
        operator()(I first, S last, O result, C comp = {}) const
        {
            if (!(first == last))
            {
                RAH2_NAMESPACE::iter_value_t<I> value = *first;
                *result = value;
                ++result;
                while (!(++first == last))
                {
                    auto&& value2 = *first;
                    if (!RAH2_INVOKE_2(comp, value2, value))
                    {
                        value = RAH2_STD::forward<decltype(value2)>(value2);
                        *result = value;
                        ++result;
                    }
                }
            }

            return {RAH2_STD::move(first), RAH2_STD::move(result)};
        }

        template <
            typename R, // ranges::input_range
            typename O, // RAH2_STD::weakly_incrementable
            typename C = RAH2_NAMESPACE::equal_to, // RAH2_STD::indirect_equivalence_relation<RAH2_STD::projected<ranges::iterator_t<R>, Proj>>
            RAH2_STD::enable_if_t<input_range<R>>* = nullptr>
        //requires RAH2_STD::indirectly_copyable<ranges::iterator_t<R>, O>
        //         && (RAH2_STD::forward_iterator<ranges::iterator_t<R>>
        //             or (RAH2_STD::input_iterator<O>
        //                 && RAH2_STD::same_as<ranges::range_value_t<R>, RAH2_STD::iter_value_t<O>>)
        //             || RAH2_STD::indirectly_copyable_storable<ranges::iterator_t<R>, O>)
        constexpr RAH2_NAMESPACE::unique_copy_result<RAH2_NAMESPACE::borrowed_iterator_t<R>, O>
        operator()(R&& r, O result, C comp = {}) const
        {
            return (*this)(
                RAH2_NAMESPACE::begin(r),
                RAH2_NAMESPACE::end(r),
                RAH2_STD::move(result),
                RAH2_STD::move(comp));
        }
    };

    constexpr unique_copy_fn unique_copy{};

    struct is_partitioned_fn
    {
        template <
            typename I, // RAH2_STD::input_iterator
            typename S, // RAH2_STD::sentinel_for<I
            typename Pred // RAH2_STD::indirect_unary_predicate<RAH2_STD::projected<I, Proj>>
            >
        constexpr bool operator()(I first, S last, Pred pred) const
        {
            for (; first != last; ++first)
                if (!RAH2_INVOKE_1(pred, *first))
                    break;

            for (; first != last; ++first)
                if (RAH2_INVOKE_1(pred, *first))
                    return false;

            return true;
        }

        template <
            typename R, // ranges::input_range
            typename Pred // RAH2_STD::indirect_unary_predicate<RAH2_STD::projected<ranges::iterator_t<R>, Proj>>
            >
        constexpr bool operator()(R&& r, Pred pred) const
        {
            return (*this)(RAH2_NAMESPACE::begin(r), RAH2_NAMESPACE::end(r), RAH2_STD::ref(pred));
        }
    };

    constexpr auto is_partitioned = is_partitioned_fn();

    template <class I, class O1, class O2>
    using partition_copy_result = in_out_out_result<I, O1, O2>;

    struct partition_copy_fn
    {
        template <
            typename I, // RAH2_STD::input_iterator
            typename S, // RAH2_STD::sentinel_for<I>
            typename O1, // RAH2_STD::weakly_incrementable
            typename O2, // RAH2_STD::weakly_incrementable
            // class Proj = RAH2_STD::identity,
            typename Pred // RAH2_STD::indirect_unary_predicate<RAH2_STD::projected<I, Proj>>
            >
        // requires RAH2_STD::indirectly_copyable<I, O1> && RAH2_STD::indirectly_copyable<I, O2>
        constexpr RAH2_NAMESPACE::partition_copy_result<I, O1, O2>
        operator()(I first, S last, O1 out_true, O2 out_false, Pred pred) const
        {
            for (; first != last; ++first)
                if (!!RAH2_INVOKE_1(pred, *first))
                    *out_true = *first, ++out_true;
                else
                    *out_false = *first, ++out_false;
            return {RAH2_STD::move(first), RAH2_STD::move(out_true), RAH2_STD::move(out_false)};
        }

        template <
            typename R, // ranges::input_range
            typename O1, /// RAH2_STD::weakly_incrementable
            typename O2, // RAH2_STD::weakly_incrementable
            // class Proj = RAH2_STD::identity,
            typename Pred // RAH2_STD::indirect_unary_predicate<RAH2_STD::projected<iterator_t<R>, Proj>>
            >
        //requires RAH2_STD::indirectly_copyable<ranges::iterator_t<R>, O1>
        //         && RAH2_STD::indirectly_copyable<ranges::iterator_t<R>, O2>
        constexpr RAH2_NAMESPACE::partition_copy_result<RAH2_NAMESPACE::borrowed_iterator_t<R>, O1, O2>
        operator()(R&& r, O1 out_true, O2 out_false, Pred pred) const
        {
            return (*this)(
                RAH2_NAMESPACE::begin(r),
                RAH2_NAMESPACE::end(r),
                RAH2_STD::move(out_true),
                RAH2_STD::move(out_false),
                RAH2_STD::move(pred));
        }
    };

    constexpr partition_copy_fn partition_copy{};

    struct partition_point_fn
    {
        template <class ForwardIt, class UnaryPredicate>
        constexpr ForwardIt operator()(ForwardIt first, ForwardIt last, UnaryPredicate p) const
        {
            for (auto length = RAH2_STD::distance(first, last); 0 < length;)
            {
                auto half = length / 2;
                auto middle = RAH2_STD::next(first, half);
                if (p(*middle))
                {
                    first = RAH2_STD::next(middle);
                    length -= (half + 1);
                }
                else
                    length = half;
            }

            return first;
        }

        template <class ForwardRange, class UnaryPredicate>
        constexpr borrowed_iterator_t<ForwardRange> operator()(ForwardRange&& range, UnaryPredicate p) const
        {
            return (*this)(
                RAH2_NAMESPACE::begin(range), RAH2_NAMESPACE::end(range), RAH2_STD::move(p));
        }
    };

    constexpr partition_point_fn partition_point{};

    template <class I, class O>
    using partial_sort_copy_result = RAH2_NAMESPACE::in_out_result<I, O>;

    struct partial_sort_copy_fn
    {
        template <
            typename I1, // RAH2_STD::input_iterator
            typename S1, // RAH2_STD::sentinel_for<I1>
            typename I2, // RAH2_STD::random_access_iterator
            typename S2, // RAH2_STD::sentinel_for<I2>
            class Comp = RAH2_NAMESPACE::less>
        //requires RAH2_STD::indirectly_copyable<I1, I2> && RAH2_STD::sortable<I2, Comp, Proj2> && RAH2_STD::
        //    indirect_strict_weak_order<Comp, RAH2_STD::projected<I1, Proj1>, RAH2_STD::projected<I2, Proj2>>
        constexpr RAH2_NAMESPACE::partial_sort_copy_result<I1, I2>
        operator()(I1 first, S1 last, I2 result_first, S2 result_last, Comp comp = {}) const
        {
            if (result_first == result_last)
                return {
                    RAH2_STD::move(RAH2_NAMESPACE::next(RAH2_STD::move(first), RAH2_STD::move(last))),
                    RAH2_STD::move(result_first)};

            auto out_last{result_first};
            // copy first N elements
            for (; !(first == last or out_last == result_last); ++out_last, ++first)
                *out_last = *first;

            // convert N copied elements into a max-heap
            RAH2_STD::make_heap(result_first, out_last, comp);

            // process the rest of the input range (if any), preserving the heap property
            for (; first != last; ++first)
            {
                if (RAH2_INVOKE_2(comp, *first, *result_first))
                {
                    // pop out the biggest item and push in a newly found smaller one
                    RAH2_STD::pop_heap(result_first, out_last, comp);
                    *(out_last - 1) = *first;
                    RAH2_STD::push_heap(result_first, out_last, comp);
                }
            }

            // first N elements in the output range is still
            // a heap - convert it into a sorted range
            RAH2_STD::sort_heap(result_first, out_last, comp);

            return {RAH2_STD::move(first), RAH2_STD::move(out_last)};
        }

        template <
            typename R1, // ranges::input_range
            typename R2, // ranges::random_access_range
            class Comp = RAH2_NAMESPACE::less>
        //requires RAH2_STD::indirectly_copyable<ranges::iterator_t<R1>, ranges::iterator_t<R2>>
        //         && RAH2_STD::sortable<ranges::iterator_t<R2>, Comp, Proj2>
        //         && RAH2_STD::indirect_strict_weak_order<
        //             Comp,
        //             RAH2_STD::projected<ranges::iterator_t<R1>, Proj1>,
        //             RAH2_STD::projected<ranges::iterator_t<R2>, Proj2>>
        constexpr RAH2_NAMESPACE::partial_sort_copy_result<
            RAH2_NAMESPACE::borrowed_iterator_t<R1>,
            RAH2_NAMESPACE::borrowed_iterator_t<R2>>
        operator()(R1&& r, R2&& result_r, Comp comp = {}) const
        {
            return (*this)(
                RAH2_NAMESPACE::begin(r),
                RAH2_NAMESPACE::end(r),
                RAH2_NAMESPACE::begin(result_r),
                RAH2_NAMESPACE::end(result_r),
                RAH2_STD::move(comp));
        }
    };

    constexpr partial_sort_copy_fn partial_sort_copy{};

    // TODO : This is the slower implementation. Make a better one.
    struct inplace_merge_fn
    {
        template <
            typename I, // RAH2_STD::bidirectional_iterator
            typename S, // RAH2_STD::sentinel_for<I>
            class Comp = RAH2_NAMESPACE::less,
            RAH2_STD::enable_if_t<bidirectional_iterator<I> && sentinel_for<S, I>>* = nullptr>
        // requires RAH2_STD::sortable<I, Comp, Proj>
        constexpr I operator()(I first, I middle, S last, Comp comp = {}) const
        {
            I last_it = RAH2_NAMESPACE::next(middle, last);
            inplace_merge_slow(
                first,
                middle,
                last_it,
                RAH2_NAMESPACE::distance(first, middle),
                RAH2_NAMESPACE::distance(middle, last_it),
                RAH2_STD::ref(comp));
            return last_it;
        }

        template <
            typename R, // ranges::bidirectional_range
            class Comp = RAH2_NAMESPACE::less,
            RAH2_STD::enable_if_t<bidirectional_range<R>>* = nullptr>
        // requires RAH2_STD::sortable<ranges::iterator_t<R>, Comp, Proj>
        constexpr RAH2_NAMESPACE::borrowed_iterator_t<R>
        operator()(R&& r, RAH2_NAMESPACE::iterator_t<R> middle, Comp comp = {}) const
        {
            return (*this)(
                RAH2_NAMESPACE::begin(r),
                RAH2_STD::move(middle),
                RAH2_NAMESPACE::end(r),
                RAH2_STD::move(comp));
        }

    private:
        template <class I, class Comp>
        static constexpr void inplace_merge_slow(
            I first,
            I middle,
            I last,
            RAH2_NAMESPACE::iter_difference_t<I> n1,
            RAH2_NAMESPACE::iter_difference_t<I> n2,
            Comp comp)
        {
            if (n1 == 0 || n2 == 0)
                return;
            if (n1 + n2 == 2 && comp(*middle, *first))
            {
                RAH2_NAMESPACE::iter_swap(first, middle);
                return;
            }

            I cut1 = first, cut2 = middle;
            RAH2_NAMESPACE::iter_difference_t<I> d1{}, d2{};

            if (n1 > n2)
            {
                d1 = n1 / 2;
                RAH2_NAMESPACE::advance(cut1, d1);
                cut2 = RAH2_NAMESPACE::lower_bound(middle, last, *cut1, RAH2_STD::ref(comp));
                d2 = RAH2_NAMESPACE::distance(middle, cut2);
            }
            else
            {
                d2 = n2 / 2;
                RAH2_NAMESPACE::advance(cut2, d2);
                cut1 = RAH2_NAMESPACE::upper_bound(first, middle, *cut2, RAH2_STD::ref(comp));
                d1 = RAH2_NAMESPACE::distance(first, cut1);
            }

            I new_middle = RAH2_NAMESPACE::rotate(cut1, middle, cut2).begin();
            inplace_merge_slow(first, cut1, new_middle, d1, d2, RAH2_STD::ref(comp));
            inplace_merge_slow(new_middle, cut2, last, n1 - d1, n2 - d2, RAH2_STD::ref(comp));
        }
    };

    constexpr inplace_merge_fn inplace_merge{};

    struct includes_fn
    {
        template <
            typename I1, // RAH2_STD::input_iterator
            typename S1, // RAH2_STD::sentinel_for<I1>
            typename I2, // RAH2_STD::input_iterator
            typename S2, // RAH2_STD::sentinel_for<I2>
            typename Comp = RAH2_NAMESPACE::less // RAH2_STD::indirect_strict_weak_order<RAH2_STD::projected<I1, Proj1>, RAH2_STD::projected<I2, Proj2>>
            >
        constexpr bool operator()(I1 first1, S1 last1, I2 first2, S2 last2, Comp comp = {}) const
        {
            for (; first2 != last2; ++first1)
            {
                if (first1 == last1 || comp(*first2, *first1))
                    return false;
                if (!comp(*first1, *first2))
                    ++first2;
            }
            return true;
        }

        template <
            typename R1, // ranges::input_range
            typename R2, // ranges::input_range
            typename Comp = RAH2_NAMESPACE::less // RAH2_STD::indirect_strict_weak_order<
            >
        constexpr bool operator()(R1&& r1, R2&& r2, Comp comp = {}) const
        {
            return (*this)(
                RAH2_NAMESPACE::begin(r1),
                RAH2_NAMESPACE::end(r1),
                RAH2_NAMESPACE::begin(r2),
                RAH2_NAMESPACE::end(r2),
                RAH2_STD::move(comp));
        }
    };

    constexpr auto includes = includes_fn{};

    struct max_fn
    {
        template <
            class T,
            typename Comp = RAH2_NAMESPACE::less // RAH2_STD::indirect_strict_weak_order<RAH2_STD::projected<const T*, Proj>>
            >
        constexpr const T& operator()(const T& a, const T& b, Comp comp = {}) const
        {
            return RAH2_INVOKE_2(comp, a, b) ? b : a;
        }

        template <
            typename T, // RAH2_STD::copyable
            typename Comp = RAH2_NAMESPACE::less // RAH2_STD::indirect_strict_weak_order<RAH2_STD::projected<const T*, Proj>>
            >
        constexpr const T operator()(RAH2_STD::initializer_list<T> r, Comp comp = {}) const
        {
            return *RAH2_NAMESPACE::max_element(r, RAH2_STD::ref(comp));
        }

        template <typename R, typename Comp = RAH2_NAMESPACE::less, RAH2_STD::enable_if_t<forward_range<R>>* = nullptr>
        constexpr RAH2_NAMESPACE::range_value_t<R> operator()(R&& r, Comp comp = {}) const
        {
            using V = RAH2_NAMESPACE::range_value_t<R>;
            return static_cast<V>(*RAH2_NAMESPACE::max_element(r, RAH2_STD::ref(comp)));
        }
        template <typename R, typename Comp = RAH2_NAMESPACE::less, RAH2_STD::enable_if_t<!forward_range<R>>* = nullptr>
        constexpr RAH2_NAMESPACE::range_value_t<R> operator()(R&& r, Comp comp = {}) const
        {
            using V = RAH2_NAMESPACE::range_value_t<R>;
            auto i = RAH2_NAMESPACE::begin(r);
            auto s = RAH2_NAMESPACE::end(r);
            V m(*i);
            while (++i != s)
                if (RAH2_INVOKE_2(comp, m, *i))
                    m = *i;
            return m;
        }
    };

    constexpr max_fn max;

    struct min_fn
    {
        template <class T, typename Comp = RAH2_NAMESPACE::less>
        constexpr const T& operator()(const T& a, const T& b, Comp comp = {}) const
        {
            return RAH2_INVOKE_2(comp, b, a) ? b : a;
        }

        template <typename T, typename Comp = RAH2_NAMESPACE::less>
        constexpr const T operator()(RAH2_STD::initializer_list<T> r, Comp comp = {}) const
        {
            return *RAH2_NAMESPACE::min_element(r, RAH2_STD::ref(comp));
        }

        template <typename R, typename Comp = RAH2_NAMESPACE::less, RAH2_STD::enable_if_t<forward_range<R>>* = nullptr>
        constexpr RAH2_NAMESPACE::range_value_t<R> operator()(R&& r, Comp comp = {}) const
        {
            using V = RAH2_NAMESPACE::range_value_t<R>;
            return static_cast<V>(*RAH2_NAMESPACE::min_element(r, RAH2_STD::ref(comp)));
        }
        template <typename R, typename Comp = RAH2_NAMESPACE::less, RAH2_STD::enable_if_t<!forward_range<R>>* = nullptr>
        constexpr RAH2_NAMESPACE::range_value_t<R> operator()(R&& r, Comp comp = {}) const
        {
            using V = RAH2_NAMESPACE::range_value_t<R>;
            auto i = RAH2_NAMESPACE::begin(r);
            auto s = RAH2_NAMESPACE::end(r);
            V m(*i);
            while (++i != s)
                if (RAH2_INVOKE_2(comp, *i, m))
                    m = *i;
            return m;
        }
    };

    constexpr min_fn min;

    template <class T>
    using minmax_result = RAH2_NAMESPACE::min_max_result<T>;

    struct minmax_fn
    {
        template <class T, typename Comp = RAH2_NAMESPACE::less>
        constexpr RAH2_NAMESPACE::minmax_result<const T&>
        operator()(const T& a, const T& b, Comp comp = {}) const
        {
            if (RAH2_INVOKE_2(comp, b, a))
                return {b, a};

            return {a, b};
        }

        template <typename T, typename Comp = RAH2_NAMESPACE::less>
        constexpr RAH2_NAMESPACE::minmax_result<T>
        operator()(RAH2_STD::initializer_list<T> r, Comp comp = {}) const
        {
            auto result = RAH2_NAMESPACE::minmax_element(r, RAH2_STD::ref(comp));
            return {*result.min, *result.max};
        }

        template <typename R, typename Comp = RAH2_NAMESPACE::less>
        // requires RAH2_STD::indirectly_copyable_storable<ranges::iterator_t<R>, ranges::range_value_t<R>*>
        constexpr RAH2_NAMESPACE::minmax_result<RAH2_NAMESPACE::range_value_t<R>>
        operator()(R&& r, Comp comp = {}) const
        {
            auto result = RAH2_NAMESPACE::minmax_element(r, RAH2_STD::ref(comp));
            return {RAH2_STD::move(*result.min), RAH2_STD::move(*result.max)};
        }
    };

    constexpr minmax_fn minmax;

    template <class I>
    using prev_permutation_result = RAH2_NAMESPACE::in_found_result<I>;

    struct prev_permutation_fn
    {
        template <
            typename I,
            typename S,
            class Comp = RAH2_NAMESPACE::less,
            RAH2_STD::enable_if_t<
                RAH2_NAMESPACE::bidirectional_iterator<I> && RAH2_NAMESPACE::sentinel_for<S, I>>* = nullptr>
        // requires RAH2_STD::sortable<I, Comp, Proj>
        constexpr RAH2_NAMESPACE::prev_permutation_result<I>
        operator()(I first, S last, Comp comp = {}) const
        {
            // check that the sequence has at least two elements
            if (first == last)
                return {RAH2_STD::move(first), false};
            auto i{first};
            ++i;
            if (i == last)
                return {RAH2_STD::move(i), false};
            auto i_last{RAH2_NAMESPACE::next(first, last)};
            i = i_last;
            --i;
            // main "permutating" loop
            for (;;)
            {
                auto i1{i};
                --i;
                if (RAH2_INVOKE_2(comp, *i1, *i))
                {
                    auto j{i_last};
                    while (!RAH2_INVOKE_2(comp, *--j, *i))
                        ;
                    RAH2_NAMESPACE::iter_swap(i, j);
                    RAH2_NAMESPACE::reverse(i1, last);
                    return {RAH2_STD::move(i_last), true};
                }
                // permutation "space" is exhausted
                if (i == first)
                {
                    RAH2_NAMESPACE::reverse(first, last);
                    return {RAH2_STD::move(i_last), false};
                }
            }
        }

        template <
            typename R, // RAH2_NAMESPACE::bidirectional_range
            class Comp = RAH2_NAMESPACE::less,
            RAH2_STD::enable_if_t<bidirectional_range<R>>* = nullptr>
        // requires RAH2_STD::sortable<RAH2_NAMESPACE::iterator_t<R>, Comp, Proj>
        constexpr RAH2_NAMESPACE::prev_permutation_result<RAH2_NAMESPACE::borrowed_iterator_t<R>>
        operator()(R&& r, Comp comp = {}) const
        {
            return (*this)(RAH2_NAMESPACE::begin(r), RAH2_NAMESPACE::end(r), RAH2_STD::move(comp));
        }
    };

    constexpr prev_permutation_fn prev_permutation{};

    template <class O, class T>
    using iota_result = RAH2_NAMESPACE::out_value_result<O, T>;

    struct iota_fn
    {
        template <
            typename O, // RAH2_STD::input_or_output_iterator
            typename S, // RAH2_STD::sentinel_for<O>
            typename T // RAH2_STD::weakly_incrementable
            >
        // requires RAH2_STD::indirectly_writable<O, const T&>
        constexpr iota_result<O, T> operator()(O first, S last, T value) const
        {
            while (first != last)
            {
                *first = RAH2_NAMESPACE::as_const(value);
                ++first;
                ++value;
            }
            return {RAH2_STD::move(first), RAH2_STD::move(value)};
        }

        template <
            typename T, // RAH2_STD::weakly_incrementable
            typename R // RAH2_NAMESPACE::output_range<const T&>
            >
        constexpr iota_result<RAH2_NAMESPACE::borrowed_iterator_t<R>, T> operator()(R&& r, T value) const
        {
            return (*this)(RAH2_NAMESPACE::begin(r), RAH2_NAMESPACE::end(r), RAH2_STD::move(value));
        }
    };

    constexpr iota_fn iota;

    template <class I, class O>
    using uninitialized_copy_result = RAH2_NAMESPACE::in_out_result<I, O>;

    struct uninitialized_copy_fn
    {
        // TODO : improve efficiency when the copied type is a TrivialType
        template <
            typename I, // RAH2_STD::input_iterator
            typename S1, // RAH2_STD::sentinel_for<I>
            typename O, // no-throw-forward-iterator
            typename S2 // no-throw-sentinel-for<O>
            >
        // requires RAH2_STD::constructible_from<RAH2_STD::iter_value_t<O>, RAH2_STD::iter_reference_t<I>>
        RAH2_NAMESPACE::uninitialized_copy_result<I, O>
        operator()(I ifirst, S1 ilast, O ofirst, S2 olast) const
        {
            O current{ofirst};
            try
            {
                for (; !(ifirst == ilast or current == olast); ++ifirst, ++current)
                    RAH2_NAMESPACE::construct_at(RAH2_STD::addressof(*current), *ifirst);
                return {RAH2_STD::move(ifirst), RAH2_STD::move(current)};
            }
            catch (...) // rollback: destroy constructed elements
            {
                for (; ofirst != current; ++ofirst)
                    RAH2_NAMESPACE::destroy_at(RAH2_STD::addressof(*ofirst));
                throw;
            }
        }

        template <
            typename IR, // ranges::input_range
            typename OR // no-throw-forward-range
            >
        // requires RAH2_STD::constructible_from<ranges::range_value_t<OR>, ranges::range_reference_t<IR>>
        RAH2_NAMESPACE::uninitialized_copy_result<
            RAH2_NAMESPACE::borrowed_iterator_t<IR>,
            RAH2_NAMESPACE::borrowed_iterator_t<OR>>
        operator()(IR&& in_range, OR&& out_range) const
        {
            return (*this)(
                RAH2_NAMESPACE::begin(in_range),
                RAH2_NAMESPACE::end(in_range),
                RAH2_NAMESPACE::begin(out_range),
                RAH2_NAMESPACE::end(out_range));
        }
    };

    constexpr uninitialized_copy_fn uninitialized_copy{};

    template <class I, class O>
    using uninitialized_copy_n_result = RAH2_NAMESPACE::in_out_result<I, O>;

    struct uninitialized_copy_n_fn
    {
        // TODO : improve efficiency when the copied type is a TrivialType
        template <
            typename I, // RAH2_STD::input_iterator
            typename O, // no-throw-input-iterator
            typename S // no-throw-sentinel-for<O>
            >
        // requires RAH2_STD::constructible_from<RAH2_STD::iter_value_t<O>, RAH2_STD::iter_reference_t<I>>
        RAH2_NAMESPACE::uninitialized_copy_n_result<I, O>
        operator()(I ifirst, RAH2_NAMESPACE::iter_difference_t<I> count, O ofirst, S olast) const
        {
            O current{ofirst};
            try
            {
                for (; count > 0 && current != olast; ++ifirst, ++current, --count)
                    RAH2_NAMESPACE::construct_at(RAH2_STD::addressof(*current), *ifirst);
                return {RAH2_STD::move(ifirst), RAH2_STD::move(current)};
            }
            catch (...) // rollback: destroy constructed elements
            {
                for (; ofirst != current; ++ofirst)
                    RAH2_NAMESPACE::destroy_at(RAH2_STD::addressof(*ofirst));
                throw;
            }
        }
    };

    constexpr uninitialized_copy_n_fn uninitialized_copy_n{};

    struct uninitialized_fill_fn
    {
        template <
            typename I, // no-throw-forward-iterator
            typename S, // no-throw-sentinel-for<I>
            class T>
        // requires RAH2_STD::constructible_from<RAH2_STD::iter_value_t<I>, const T&>
        I operator()(I first, S last, const T& x) const
        {
            I rollback{first};
            try
            {
                for (; !(first == last); ++first)
                    RAH2_NAMESPACE::construct_at(RAH2_STD::addressof(*first), x);
                return first;
            }
            catch (...)
            {
                // rollback: destroy constructed elements
                for (; rollback != first; ++rollback)
                    RAH2_NAMESPACE::destroy_at(RAH2_STD::addressof(*rollback));
                throw;
            }
        }

        template <
            typename R, // no-throw-forward-range
            class T>
        // requires RAH2_STD::constructible_from<ranges::range_value_t<R>, const T&>
        RAH2_NAMESPACE::borrowed_iterator_t<R> operator()(R&& r, const T& x) const
        {
            return (*this)(RAH2_NAMESPACE::begin(r), RAH2_NAMESPACE::end(r), x);
        }
    };

    constexpr uninitialized_fill_fn uninitialized_fill{};

    struct uninitialized_fill_n_fn
    {
        template <
            typename I, // no-throw-forward-range
            class T>
        // requires RAH2_STD::constructible_from<RAH2_STD::iter_value_t<I>, const T&>
        I operator()(I first, RAH2_NAMESPACE::iter_difference_t<I> n, const T& x) const
        {
            I rollback{first};
            try
            {
                for (; n-- > 0; ++first)
                    RAH2_NAMESPACE::construct_at(RAH2_STD::addressof(*first), x);
                return first;
            }
            catch (...) // rollback: destroy constructed elements
            {
                for (; rollback != first; ++rollback)
                    RAH2_NAMESPACE::destroy_at(RAH2_STD::addressof(*rollback));
                throw;
            }
        }
    };

    constexpr uninitialized_fill_n_fn uninitialized_fill_n{};

    template <class I, class O>
    using uninitialized_move_result = RAH2_NAMESPACE::in_out_result<I, O>;

    struct uninitialized_move_fn
    {
        template <
            typename I, // RAH2_STD::input_iterator
            typename S1, // RAH2_STD::sentinel_for<I>
            typename O, // no-throw-forward-iterator
            typename S2 // no-throw-sentinel-for<O>
            >
        //requires RAH2_STD::constructible_from<RAH2_STD::iter_value_t<O>, RAH2_STD::iter_rvalue_reference_t<I>>
        RAH2_NAMESPACE::uninitialized_move_result<I, O>
        operator()(I ifirst, S1 ilast, O ofirst, S2 olast) const
        {
            O current{ofirst};
            try
            {
                for (; !(ifirst == ilast or current == olast); ++ifirst, ++current)
                    ::new (const_cast<void*>(
                        static_cast<const volatile void*>(RAH2_STD::addressof(*current))))
                        RAH2_STD::remove_reference_t<RAH2_NAMESPACE::iter_reference_t<O>>(
                            RAH2_NAMESPACE::iter_move(ifirst));
                return {RAH2_STD::move(ifirst), RAH2_STD::move(current)};
            }
            catch (...) // rollback: destroy constructed elements
            {
                for (; ofirst != current; ++ofirst)
                    RAH2_NAMESPACE::destroy_at(RAH2_STD::addressof(*ofirst));
                throw;
            }
        }

        template <
            typename IR, // ranges::input_range
            typename OR // no-throw-forward-range
            >
        // requires RAH2_STD::constructible_from<ranges::range_value_t<OR>, ranges::range_rvalue_reference_t<IR>>
        RAH2_NAMESPACE::uninitialized_move_result<
            RAH2_NAMESPACE::borrowed_iterator_t<IR>,
            RAH2_NAMESPACE::borrowed_iterator_t<OR>>
        operator()(IR&& in_range, OR&& out_range) const
        {
            return (*this)(
                RAH2_NAMESPACE::begin(in_range),
                RAH2_NAMESPACE::end(in_range),
                RAH2_NAMESPACE::begin(out_range),
                RAH2_NAMESPACE::end(out_range));
        }
    };

    constexpr uninitialized_move_fn uninitialized_move{};

    template <class I, class O>
    using uninitialized_move_n_result = RAH2_NAMESPACE::in_out_result<I, O>;

    struct uninitialized_move_n_fn
    {
        template <
            typename I, // RAH2_STD::input_iterator
            typename O, // no-throw-forward-iterator
            typename S // no-throw-sentinel-for<O>
            >
        // requires RAH2_STD::constructible_from<RAH2_STD::iter_value_t<O>, RAH2_STD::iter_rvalue_reference_t<I>>
        RAH2_NAMESPACE::uninitialized_move_n_result<I, O>
        operator()(I ifirst, RAH2_NAMESPACE::iter_difference_t<I> n, O ofirst, S olast) const
        {
            O current{ofirst};
            try
            {
                for (; n-- > 0 && current != olast; ++ifirst, ++current)
                    ::new (const_cast<void*>(
                        static_cast<const volatile void*>(RAH2_STD::addressof(*current))))
                        RAH2_STD::remove_reference_t<RAH2_NAMESPACE::iter_reference_t<O>>(
                            RAH2_NAMESPACE::iter_move(ifirst));
                return {RAH2_STD::move(ifirst), RAH2_STD::move(current)};
            }
            catch (...) // rollback: destroy constructed elements
            {
                for (; ofirst != current; ++ofirst)
                    RAH2_NAMESPACE::destroy_at(RAH2_STD::addressof(*ofirst));
                throw;
            }
        }
    };

    constexpr uninitialized_move_n_fn uninitialized_move_n{};

    struct uninitialized_default_construct_fn
    {
        template <
            typename I, // no-throw-forward-iterator
            typename S, // no-throw-sentinel-for<I>
            RAH2_STD::enable_if_t<RAH2_STD::is_trivially_default_constructible<
                RAH2_STD::remove_reference_t<RAH2_NAMESPACE::iter_reference_t<I>>>::value>* = nullptr>
        // requires RAH2_STD::default_initializable<RAH2_STD::iter_value_t<I>>
        I operator()(I first, S last) const
        {
            return RAH2_NAMESPACE::next(first, last); // skip initialization
        }
        template <
            typename I, // no-throw-forward-iterator
            typename S, // no-throw-sentinel-for<I>
            RAH2_STD::enable_if_t<!RAH2_STD::is_trivially_default_constructible<
                RAH2_STD::remove_reference_t<RAH2_NAMESPACE::iter_reference_t<I>>>::value>* = nullptr>
        // requires RAH2_STD::default_initializable<RAH2_STD::iter_value_t<I>>
        I operator()(I first, S last) const
        {
            using ValueType = RAH2_STD::remove_reference_t<RAH2_NAMESPACE::iter_reference_t<I>>;
            I rollback{first};
            try
            {
                for (; !(first == last); ++first)
                    ::new (const_cast<void*>(
                        static_cast<const volatile void*>(RAH2_STD::addressof(*first)))) ValueType;
                return first;
            }
            catch (...) // rollback: destroy constructed elements
            {
                for (; rollback != first; ++rollback)
                    RAH2_NAMESPACE::destroy_at(RAH2_STD::addressof(*rollback));
                throw;
            }
        }

        template <typename R // no-throw-forward-range
                  >
        // requires RAH2_STD::default_initializable<ranges::range_value_t<R>>
        RAH2_NAMESPACE::borrowed_iterator_t<R> operator()(R&& r) const
        {
            return (*this)(RAH2_NAMESPACE::begin(r), RAH2_NAMESPACE::end(r));
        }
    };

    constexpr uninitialized_default_construct_fn uninitialized_default_construct{};

    struct uninitialized_default_construct_n_fn
    {
        template <
            typename I, // no-throw-forward-iterator
            RAH2_STD::enable_if_t<RAH2_STD::is_trivially_default_constructible<
                RAH2_STD::remove_reference_t<RAH2_NAMESPACE::iter_reference_t<I>>>::value>* = nullptr>
        // requires RAH2_STD::default_initializable<RAH2_STD::iter_value_t<I>>
        I operator()(I first, RAH2_NAMESPACE::iter_difference_t<I> n) const
        {
            return RAH2_NAMESPACE::next(first, n); // skip initialization
        }

        template <
            typename I, // no-throw-forward-iterator
            RAH2_STD::enable_if_t<!RAH2_STD::is_trivially_default_constructible<
                RAH2_STD::remove_reference_t<RAH2_NAMESPACE::iter_reference_t<I>>>::value>* = nullptr>
        // requires RAH2_STD::default_initializable<RAH2_STD::iter_value_t<I>>
        I operator()(I first, RAH2_NAMESPACE::iter_difference_t<I> n) const
        {
            using ValueType = RAH2_STD::remove_reference_t<RAH2_NAMESPACE::iter_reference_t<I>>;
            I rollback{first};
            try
            {
                for (; n-- > 0; ++first)
                    ::new (const_cast<void*>(
                        static_cast<const volatile void*>(RAH2_STD::addressof(*first)))) ValueType;
                return first;
            }
            catch (...) // rollback: destroy constructed elements
            {
                for (; rollback != first; ++rollback)
                    RAH2_NAMESPACE::destroy_at(RAH2_STD::addressof(*rollback));
                throw;
            }
        }
    };

    constexpr uninitialized_default_construct_n_fn uninitialized_default_construct_n{};

    struct uninitialized_value_construct_fn
    {
        template <
            typename I, // no-throw-forward-iterator
            typename S, // no-throw-sentinel-for<I>
            typename T = RAH2_STD::remove_reference_t<RAH2_NAMESPACE::iter_reference_t<I>>,
            RAH2_STD::enable_if_t<
                RAH2_STD::is_trivial<T>::value && RAH2_STD::is_copy_assignable<T>::value>* = nullptr>
        // requires RAH2_STD::default_initializable<RAH2_STD::iter_value_t<I>>
        I operator()(I first, S last) const
        {
            return RAH2_NAMESPACE::fill(first, last, T());
        }

        template <
            typename I, // no-throw-forward-iterator
            typename S, // no-throw-sentinel-for<I>
            typename T = RAH2_STD::remove_reference_t<RAH2_NAMESPACE::iter_reference_t<I>>,
            RAH2_STD::enable_if_t<!(
                RAH2_STD::is_trivial<T>::value && RAH2_STD::is_copy_assignable<T>::value)>* = nullptr>
        // requires RAH2_STD::default_initializable<RAH2_STD::iter_value_t<I>>
        I operator()(I first, S last) const
        {
            I rollback{first};
            try
            {
                for (; !(first == last); ++first)
                    ::new (const_cast<void*>(
                        static_cast<const volatile void*>(RAH2_STD::addressof(*first)))) T();
                return first;
            }
            catch (...) // rollback: destroy constructed elements
            {
                for (; rollback != first; ++rollback)
                    RAH2_NAMESPACE::destroy_at(RAH2_STD::addressof(*rollback));
                throw;
            }
        }

        template <typename R // no-throw-forward-range
                  >
        // requires RAH2_STD::default_initializable<ranges::range_value_t<R>>
        RAH2_NAMESPACE::borrowed_iterator_t<R> operator()(R&& r) const
        {
            return (*this)(RAH2_NAMESPACE::begin(r), RAH2_NAMESPACE::end(r));
        }
    };

    constexpr uninitialized_value_construct_fn uninitialized_value_construct{};

    struct uninitialized_value_construct_n_fn
    {
        template <
            typename I, // no-throw-forward-iterator
            typename T = RAH2_STD::remove_reference_t<RAH2_NAMESPACE::iter_reference_t<I>>,
            RAH2_STD::enable_if_t<
                (RAH2_STD::is_trivial<T>::value && RAH2_STD::is_copy_assignable<T>::value)>* = nullptr>
        I operator()(I first, RAH2_NAMESPACE::iter_difference_t<I> n) const
        {
            return RAH2_NAMESPACE::fill_n(first, n, T());
        }

        template <
            typename I, // no-throw-forward-iterator
            typename T = RAH2_STD::remove_reference_t<RAH2_NAMESPACE::iter_reference_t<I>>,
            RAH2_STD::enable_if_t<!(
                RAH2_STD::is_trivial<T>::value && RAH2_STD::is_copy_assignable<T>::value)>* = nullptr>
        I operator()(I first, RAH2_NAMESPACE::iter_difference_t<I> n) const
        {
            I rollback{first};
            try
            {
                for (; n-- > 0; ++first)
                    ::new (const_cast<void*>(
                        static_cast<const volatile void*>(RAH2_STD::addressof(*first)))) T();
                return first;
            }
            catch (...) // rollback: destroy constructed elements
            {
                for (; rollback != first; ++rollback)
                    RAH2_NAMESPACE::destroy_at(RAH2_STD::addressof(*rollback));
                throw;
            }
        }
    };

    constexpr uninitialized_value_construct_n_fn uninitialized_value_construct_n{};

    /// shuffle
    ///
    /// New for C++11
    /// Randomizes a sequence of values via a user-supplied UniformRandomNumberGenerator.
    /// The difference between this and the original random_shuffle function is that this uses the more
    /// advanced and flexible UniformRandomNumberGenerator interface as opposed to the more
    /// limited RandomNumberGenerator interface of random_shuffle.
    ///
    /// Effects: Shuffles the elements in the range [first, last) with uniform distribution.
    ///
    /// Complexity: Exactly '(last - first) - 1' swaps.
    ///
    /// Example usage:
    ///     struct Rand{ RAH2_STD::size_t operator()(RAH2_STD::size_t n) { return (RAH2_STD::size_t)(rand() % n); } }; // Note: The C rand function is poor and slow.
    ///     Rand randInstance;
    ///     shuffle(pArrayBegin, pArrayEnd, randInstance);
    ///
    // See the C++11 Standard, 26.5.1.3, Uniform random number generator requirements.
    // Also http://en.cppreference.com/w/cpp/numeric/random/uniform_int_distribution
    template <typename RandomAccessIterator, typename Sentinel, typename UniformRandomNumberGenerator>
    void shuffle(RandomAccessIterator first, Sentinel last, UniformRandomNumberGenerator&& urng)
    {
        if (first != last)
        {
            typedef
                typename RAH2_STD::iterator_traits<RandomAccessIterator>::difference_type difference_type;
            typedef typename RAH2_STD::make_unsigned<difference_type>::type unsigned_difference_type;
            typedef typename RAH2_STD::uniform_int_distribution<unsigned_difference_type>
                uniform_int_distribution;
            typedef typename uniform_int_distribution::param_type uniform_int_distribution_param_type;

            uniform_int_distribution uid;

            for (RandomAccessIterator i = first + 1; i != last; ++i)
                RAH2_NAMESPACE::iter_swap(
                    i,
                    first
                        + iter_difference_t<RandomAccessIterator>(
                            uid(urng, uniform_int_distribution_param_type(0u, size_t(i - first)))));
        }
    }

    template <typename RandomRange, typename UniformRandomNumberGenerator>
    void shuffle(RandomRange&& range, UniformRandomNumberGenerator&& urng)
    {
        RAH2_NAMESPACE::shuffle(
            RAH2_NAMESPACE::begin(range),
            RAH2_NAMESPACE::end(range),
            RAH2_STD::forward<UniformRandomNumberGenerator>(urng));
    }
} // namespace RAH2_NAMESPACE