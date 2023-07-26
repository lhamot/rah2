#pragma once

#include "range_bases.hpp"
#include "algo_sort.hpp"

namespace rah
{
    struct fold_left_fn
    {
        template <typename I, typename S, class T, typename F>
        constexpr auto operator()(I first, S last, T init, F f) const
        {
            using U = RAH_NAMESPACE::remove_cvref_t<decltype(f(init, *first))>;
            if (first == last)
                return U(std::move(init));
            U accum = RAH_INVOKE_2(f, std::move(init), *first);
            for (++first; first != last; ++first)
                accum = RAH_INVOKE_2(f, std::move(accum), *first);
            return std::move(accum);
        }

        template <typename R, class T, typename F>
        constexpr auto operator()(R&& r, T init, F f) const
        {
            return (*this)(
                RAH_NAMESPACE::begin(r), RAH_NAMESPACE::end(r), std::move(init), std::ref(f));
        }
    };

    constexpr fold_left_fn fold_left;

    struct fold_left_first_fn
    {
        template <
            typename I, // std::input_iterator
            typename S, // std::sentinel_for<I>
            typename F // __indirectly_binary_left_foldable<std::iter_value_t<I>, I>
            >
        // requires std::constructible_from<std::iter_value_t<I>, std::iter_reference_t<I>>
        constexpr auto operator()(I first, S last, F f) const
        {
            using U = decltype(RAH_NAMESPACE::fold_left(
                RAH_STD::move(first), last, RAH_NAMESPACE::iter_value_t<I>(*first), f));
            if (first == last)
                return RAH_NAMESPACE::details::optional<U>();
            RAH_NAMESPACE::details::optional<U> init(RAH_STD::move(*first));
            for (++first; first != last; ++first)
                *init = RAH_INVOKE_2(f, std::move(*init), *first);
            return RAH_STD::move(init);
        }

        template <
            typename R, // input_range
            typename F // __indirectly_binary_left_foldable<ranges::range_value_t<R>, ranges::iterator_t<R>>
            >
        // requires std::constructible_from<ranges::range_value_t<R>, ranges::range_reference_t<R>>
        constexpr auto operator()(R&& r, F f) const
        {
            return (*this)(RAH_NAMESPACE::begin(r), RAH_NAMESPACE::end(r), std::ref(f));
        }
    };

    constexpr fold_left_first_fn fold_left_first;

    struct fold_right_fn
    {
        template <
            typename I, // std::bidirectional_iterator
            typename S, // std::sentinel_for<I>
            class T,
            typename F // __indirectly_binary_right_foldable<T, I>
            >
        constexpr auto operator()(I first, S last, T init, F f) const
        {
            using U = std::decay_t<decltype(RAH_INVOKE_2(f, *first, RAH_STD::move(init)))>;
            if (first == last)
                return U(std::move(init));
            I tail = RAH_NAMESPACE::next(first, last);
            U accum = RAH_INVOKE_2(f, *--tail, RAH_STD::move(init));
            while (first != tail)
                accum = RAH_INVOKE_2(f, *--tail, RAH_STD::move(accum));
            return accum;
        }

        template <
            typename R, // RAH_NAMESPACE::bidirectional_range
            class T,
            typename F // __indirectly_binary_right_foldable<T, ranges::iterator_t<R>>
            >
        constexpr auto operator()(R&& r, T init, F f) const
        {
            return (*this)(
                RAH_NAMESPACE::begin(r), RAH_NAMESPACE::end(r), RAH_STD::move(init), RAH_STD::ref(f));
        }
    };

    constexpr fold_right_fn fold_right;

    struct fold_right_last_fn
    {
        template <
            typename I, // std::bidirectional_iterator
            typename S, // std::sentinel_for<I>
            typename F // __indirectly_binary_right_foldable<std::iter_value_t<I>, I>
            >
        // requires std::constructible_from<std::iter_value_t<I>, std::iter_reference_t<I>>
        constexpr auto operator()(I first, S last, F f) const
        {
            using U = decltype(RAH_NAMESPACE::fold_right(
                first, last, RAH_NAMESPACE::iter_value_t<I>(*first), f));

            if (first == last)
                return RAH_NAMESPACE::details::optional<U>();
            I tail = RAH_STD::prev(RAH_NAMESPACE::next(first, RAH_STD::move(last)));
            return RAH_NAMESPACE::details::optional<U>(RAH_NAMESPACE::fold_right(
                RAH_STD::move(first), tail, RAH_NAMESPACE::iter_value_t<I>(*tail), RAH_STD::move(f)));
        }

        template <
            typename R, // ranges::bidirectional_range
            typename F // __indirectly_binary_right_foldable<ranges::range_value_t<R>, ranges::iterator_t<R>>
            >
        // requires std::constructible_from<ranges::range_value_t<R>, ranges::range_reference_t<R>>
        constexpr auto operator()(R&& r, F f) const
        {
            return (*this)(RAH_NAMESPACE::begin(r), RAH_NAMESPACE::end(r), std::ref(f));
        }
    };

    constexpr fold_right_last_fn fold_right_last;

    template <class I, class T>
    using fold_left_with_iter_result = RAH_NAMESPACE::in_value_result<I, T>;

    class fold_left_with_iter_fn
    {
        template <class O, class I, class S, class T, class F>
        constexpr auto impl(I&& first, S&& last, T&& init, F f) const
        {
            using U = std::decay_t<decltype(RAH_INVOKE_2(f, std::move(init), *first))>;
            using Ret = RAH_NAMESPACE::fold_left_with_iter_result<O, U>;
            if (first == last)
                return Ret{std::move(first), U(std::move(init))};
            U accum = RAH_INVOKE_2(f, std::move(init), *first);
            for (++first; first != last; ++first)
                accum = RAH_INVOKE_2(f, std::move(accum), *first);
            return Ret{std::move(first), std::move(accum)};
        }

    public:
        template <
            typename I, // std::input_iterator
            typename S, // std::sentinel_for<I>
            class T,
            typename F // __indirectly_binary_left_foldable<T, I>
            >
        constexpr auto operator()(I first, S last, T init, F f) const
        {
            return impl<I>(std::move(first), std::move(last), std::move(init), std::ref(f));
        }

        template <
            typename R, // ranges::input_range
            class T,
            typename F // __indirectly_binary_left_foldable<T, ranges::iterator_t<R>>
            >
        constexpr auto operator()(R&& r, T init, F f) const
        {
            return impl<RAH_NAMESPACE::borrowed_iterator_t<R>>(
                RAH_NAMESPACE::begin(r), RAH_NAMESPACE::end(r), std::move(init), std::ref(f));
        }
    };

    constexpr fold_left_with_iter_fn fold_left_with_iter;

    template <class I, class T>
    using fold_left_first_with_iter_result = RAH_NAMESPACE::in_value_result<I, T>;

    class fold_left_first_with_iter_fn
    {
        template <class O, class I, class S, class F>
        constexpr auto impl(I&& first, S&& last, F f) const
        {
            using U = decltype(RAH_NAMESPACE::fold_left(
                std::move(first), last, RAH_NAMESPACE::iter_value_t<I>(*first), f));
            using Ret =
                RAH_NAMESPACE::fold_left_first_with_iter_result<O, RAH_NAMESPACE::details::optional<U>>;
            if (first == last)
                return Ret{std::move(first), RAH_NAMESPACE::details::optional<U>()};
            RAH_NAMESPACE::details::optional<U> init(RAH_NAMESPACE::in_place, *first);
            for (++first; first != last; ++first)
                *init = RAH_INVOKE_2(f, std::move(*init), *first);
            return Ret{std::move(first), std::move(init)};
        }

    public:
        template <
            typename I, // std::input_iterator
            typename S, // std::sentinel_for<I>
            typename F // __indirectly_binary_left_foldable<std::iter_value_t<I>, I>
            >
        // requires std::constructible_from<std::iter_value_t<I>, std::iter_reference_t<I>>
        constexpr auto operator()(I first, S last, F f) const
        {
            return impl<I>(std::move(first), std::move(last), std::ref(f));
        }

        template <
            typename R, // ranges::input_range
            typename F // __indirectly_binary_left_foldable<ranges::range_value_t<R>, ranges::iterator_t<R>>
            >
        // requires std::constructible_from<ranges::range_value_t<R>, ranges::range_reference_t<R>>
        constexpr auto operator()(R&& r, F f) const
        {
            return impl<RAH_NAMESPACE::borrowed_iterator_t<R>>(
                RAH_NAMESPACE::begin(r), RAH_NAMESPACE::end(r), std::ref(f));
        }
    };

    constexpr fold_left_first_with_iter_fn fold_left_first_with_iter;

    struct find_last_fn
    {
        template <
            typename I,
            typename S,
            class T,
            class Proj = RAH_NAMESPACE::identity,
            std::enable_if_t<forward_iterator<I> && sentinel_for<S, I>>* = nullptr>
        constexpr RAH_NAMESPACE::subrange<I>
        operator()(I first, S last, const T& value, Proj proj = {}) const
        {
            // Note: if I is mere forward_iterator, we may only go from begin to end.
            I found{};
            for (; first != last; ++first)
                if (RAH_INVOKE_1(proj, *first) == value)
                    found = first;

            if (found == I{})
                return {first, first};

            return {found, RAH_NAMESPACE::next(found, last)};
        }

        template <typename R, class T, class Proj = RAH_NAMESPACE::identity, std::enable_if_t<forward_range<R>>* = nullptr>
        constexpr RAH_NAMESPACE::borrowed_subrange_t<R>
        operator()(R&& r, const T& value, Proj proj = {}) const
        {
            return this->operator()(
                RAH_NAMESPACE::begin(r), RAH_NAMESPACE::end(r), value, std::ref(proj));
        }
    };

    constexpr find_last_fn find_last;

    struct find_last_if_fn
    {
        template <
            typename I,
            typename S,
            class Proj = RAH_NAMESPACE::identity,
            typename Pred,
            std::enable_if_t<forward_iterator<I> && sentinel_for<S, I>>* = nullptr>
        constexpr RAH_NAMESPACE::subrange<I> operator()(I first, S last, Pred pred, Proj proj = {}) const
        {
            // Note: if I is mere forward_iterator, we may only go from begin to end.
            I found{};
            for (; first != last; ++first)
                if (RAH_INVOKE_1(pred, RAH_INVOKE_1(proj, *first)))
                    found = first;

            if (found == I{})
                return {first, first};

            return {found, RAH_NAMESPACE::next(found, last)};
        }

        template <
            typename R,
            class Proj = RAH_NAMESPACE::identity,
            typename Pred,
            std::enable_if_t<forward_range<R>>* = nullptr>
        constexpr RAH_NAMESPACE::borrowed_subrange_t<R> operator()(R&& r, Pred pred, Proj proj = {}) const
        {
            return this->operator()(
                RAH_NAMESPACE::begin(r), RAH_NAMESPACE::end(r), std::ref(pred), std::ref(proj));
        }
    };

    constexpr find_last_if_fn find_last_if;

    struct find_last_if_not_fn
    {
        template <
            typename I,
            typename S,
            class Proj = RAH_NAMESPACE::identity,
            typename Pred,
            std::enable_if_t<forward_iterator<I> && sentinel_for<S, I>>* = nullptr>
        constexpr RAH_NAMESPACE::subrange<I> operator()(I first, S last, Pred pred, Proj proj = {}) const
        {
            // Note: if I is mere forward_iterator, we may only go from begin to end.
            I found{};
            for (; first != last; ++first)
                if (!RAH_INVOKE_1(pred, RAH_INVOKE_1(proj, *first)))
                    found = first;

            if (found == I{})
                return {first, first};

            return {found, RAH_NAMESPACE::next(found, last)};
        }

        template <
            typename R,
            class Proj = RAH_NAMESPACE::identity,
            typename Pred,
            std::enable_if_t<forward_range<R>>* = nullptr>
        constexpr RAH_NAMESPACE::borrowed_subrange_t<R> operator()(R&& r, Pred pred, Proj proj = {}) const
        {
            return this->operator()(
                RAH_NAMESPACE::begin(r), RAH_NAMESPACE::end(r), std::ref(pred), std::ref(proj));
        }
    };

    constexpr find_last_if_not_fn find_last_if_not;

    struct __contains_fn
    {
        template <
            typename I,
            typename S,
            class T,
            class Proj = RAH_NAMESPACE::identity,
            std::enable_if_t<input_iterator<I> && sentinel_for<S, I>>* = nullptr>
        constexpr bool operator()(I first, S last, const T& value, Proj proj = {}) const
        {
            return RAH_NAMESPACE::find(std::move(first), last, value, proj) != last;
        }

        template <typename R, class T, class Proj = RAH_NAMESPACE::identity, std::enable_if_t<input_range<R>>* = nullptr>
        constexpr bool operator()(R&& r, const T& value, Proj proj = {}) const
        {
            return (*this)(RAH_NAMESPACE::begin(r), RAH_NAMESPACE::end(r), std::move(value), proj);
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
            class Pred = RAH_NAMESPACE::equal_to,
            class Proj1 = RAH_NAMESPACE::identity,
            class Proj2 = RAH_NAMESPACE::identity,
            std::enable_if_t<
                forward_iterator<I1> && sentinel_for<S1, I1> && forward_iterator<I2>
                && sentinel_for<S2, I2>>* = nullptr>
        constexpr bool operator()(
            I1 first1, S1 last1, I2 first2, S2 last2, Pred pred = {}, Proj1 proj1 = {}, Proj2 proj2 = {}) const
        {
            return (first2 == last2)
                   || !RAH_NAMESPACE::search(first1, last1, first2, last2, pred, proj1, proj2).empty();
        }

        template <
            typename R1,
            typename R2,
            class Pred = RAH_NAMESPACE::equal_to,
            class Proj1 = RAH_NAMESPACE::identity,
            class Proj2 = RAH_NAMESPACE::identity,
            std::enable_if_t<forward_range<R1> && forward_range<R2>>* = nullptr>
        constexpr bool
        operator()(R1&& r1, R2&& r2, Pred pred = {}, Proj1 proj1 = {}, Proj2 proj2 = {}) const
        {
            return (*this)(
                RAH_NAMESPACE::begin(r1),
                RAH_NAMESPACE::end(r1),
                RAH_NAMESPACE::begin(r2),
                RAH_NAMESPACE::end(r2),
                RAH_STD::move(pred),
                RAH_STD::move(proj1),
                RAH_STD::move(proj2));
        }
    };

    constexpr __contains_subrange_fn contains_subrange{};

    struct starts_with_fn
    {
        template <
            typename I1, // std::input_iterator
            typename S1, // std::sentinel_for<I1>
            typename I2, // std::input_iterator
            typename S2, // std::sentinel_for<I2>
            class Pred = RAH_NAMESPACE::equal_to,
            class Proj1 = RAH_NAMESPACE::identity,
            class Proj2 = RAH_NAMESPACE::identity,
            std::enable_if_t<
                input_iterator<I1> && sentinel_for<S1, I1> && input_iterator<I2>
                && sentinel_for<S2, I2>>* = nullptr>
        constexpr bool operator()(
            I1 first1, S1 last1, I2 first2, S2 last2, Pred pred = {}, Proj1 proj1 = {}, Proj2 proj2 = {}) const
        {
            return RAH_NAMESPACE::mismatch(
                       RAH_STD::move(first1),
                       last1,
                       RAH_STD::move(first2),
                       last2,
                       RAH_STD::move(pred),
                       RAH_STD::move(proj1),
                       RAH_STD::move(proj2))
                       .in2
                   == last2;
        }

        template <
            typename R1, // ranges::input_range
            typename R2, // ranges::input_range
            class Pred = RAH_NAMESPACE::equal_to,
            class Proj1 = RAH_NAMESPACE::identity,
            class Proj2 = RAH_NAMESPACE::identity>
        // requires std::indirectly_comparable<ranges::iterator_t<R1>, ranges::iterator_t<R2>, Pred, Proj1, Proj2>
        constexpr bool
        operator()(R1&& r1, R2&& r2, Pred pred = {}, Proj1 proj1 = {}, Proj2 proj2 = {}) const
        {
            return (*this)(
                RAH_NAMESPACE::begin(r1),
                RAH_NAMESPACE::end(r1),
                RAH_NAMESPACE::begin(r2),
                RAH_NAMESPACE::end(r2),
                RAH_STD::move(pred),
                RAH_STD::move(proj1),
                RAH_STD::move(proj2));
        }
    };

    constexpr starts_with_fn starts_with{};

    struct ends_with_fn
    {
        template <
            typename I1, // std::input_iterator
            typename S1, // std::sentinel_for<I1>
            typename I2, // std::input_iterator
            typename S2, // std::sentinel_for<I2>
            class Pred = RAH_NAMESPACE::equal_to,
            std::enable_if_t<(RAH_NAMESPACE::forward_iterator<I1> || RAH_NAMESPACE::sized_sentinel_for<S1, I1>)&&(
                RAH_NAMESPACE::forward_iterator<I2> || RAH_NAMESPACE::sized_sentinel_for<S2, I2>)>* = nullptr>
        constexpr bool operator()(I1 first1, S1 last1, I2 first2, S2 last2, Pred pred = {}) const
        {
            const auto n1 = RAH_NAMESPACE::distance(first1, last1);
            const auto n2 = RAH_NAMESPACE::distance(first2, last2);
            if (n1 < n2)
                return false;
            RAH_NAMESPACE::advance(first1, n1 - n2);
            return RAH_NAMESPACE::equal(
                std::move(first1),
                std::move(last1),
                std::move(first2),
                std::move(last2),
                std::move(pred));
        }

        template <
            typename R1, // ranges::input_range
            typename R2, // ranges::input_range
            class Pred = RAH_NAMESPACE::equal_to,
            std::enable_if_t<(RAH_NAMESPACE::forward_range<R1> || RAH_NAMESPACE::sized_range<R1>)&&(
                RAH_NAMESPACE::forward_range<R2> || RAH_NAMESPACE::sized_range<R2>)>* = nullptr>
        constexpr bool operator()(R1&& r1, R2&& r2, Pred pred = {}) const
        {
            return (*this)(
                RAH_NAMESPACE::begin(r1),
                RAH_NAMESPACE::end(r1),
                RAH_NAMESPACE::begin(r2),
                RAH_NAMESPACE::end(r2),
                RAH_STD::move(pred));
        }
    };

    constexpr ends_with_fn ends_with{};

    template <class I1, class I2>
    using swap_ranges_result = RAH_NAMESPACE::in_in_result<I1, I2>;

    struct swap_ranges_fn
    {
        template <
            typename I1,
            typename S1,
            typename I2,
            typename S2,
            std::enable_if_t<
                input_iterator<I1> && sentinel_for<S1, I1> && input_iterator<I2>
                && sentinel_for<S2, I2> && indirectly_swappable<I1, I2>>* = nullptr>
        constexpr RAH_NAMESPACE::swap_ranges_result<I1, I2>
        operator()(I1 first1, S1 last1, I2 first2, S2 last2) const
        {
            for (; !(first1 == last1 or first2 == last2); ++first1, ++first2)
                RAH_NAMESPACE::iter_swap(first1, first2);
            return {std::move(first1), std::move(first2)};
        }

        template <
            typename R1,
            typename R2,
            std::enable_if_t<
                input_range<R1> && input_range<R2>
                && indirectly_swappable<RAH_NAMESPACE::iterator_t<R1>, RAH_NAMESPACE::iterator_t<R2>>>* = nullptr>
        RAH_NAMESPACE::swap_ranges_result<
            RAH_NAMESPACE::borrowed_iterator_t<R1>,
            RAH_NAMESPACE::borrowed_iterator_t<R2>>
        operator()(R1&& r1, R2&& r2) const
        {
            return (*this)(
                RAH_NAMESPACE::begin(r1),
                RAH_NAMESPACE::end(r1),
                RAH_NAMESPACE::begin(r2),
                RAH_NAMESPACE::end(r2));
        }
    };

    constexpr swap_ranges_fn swap_ranges{};

    struct shift_left_fn
    {
        template <typename I, typename S, std::enable_if_t<permutable<I> && sentinel_for<S, I>>* = nullptr>
        constexpr RAH_NAMESPACE::subrange<I> operator()(I first, S last, iter_difference_t<I> n) const
        {
            if (n <= 0)
                return {RAH_STD::move(first), RAH_STD::move(last)};
            auto mid = first;
            auto diff = RAH_NAMESPACE::advance(first, n, last);
            if (diff != 0)
            {
                return {RAH_STD::move(mid), RAH_STD::move(last)};
            }
            auto result = RAH_NAMESPACE::move(first, last, mid);
            return {RAH_STD::move(mid), RAH_STD::move(result.out)};
        }

        template <typename R, std::enable_if_t<forward_range<R> && permutable<iterator_t<R>>>* = nullptr>
        constexpr borrowed_subrange_t<R> operator()(R&& r, range_difference_t<R> n) const
        {
            return (*this)(RAH_NAMESPACE::begin(r), RAH_NAMESPACE::end(r), n);
        }
    };

    constexpr shift_left_fn shift_left{};

    struct shift_right_fn
    {
        template <
            typename I,
            typename S,
            std::enable_if_t<permutable<I> && sentinel_for<S, I> && bidirectional_iterator<S>>* = nullptr>
        static constexpr RAH_NAMESPACE::subrange<I> impl(I first, S last, iter_difference_t<I> n)
        {
            if (n <= 0)
            {
                return {RAH_STD::move(first), RAH_STD::move(last)};
            }
            auto mid = last;
            if (RAH_NAMESPACE::advance(mid, -n, first) != 0)
            {
                return {RAH_STD::move(first), RAH_STD::move(last)};
            }
            auto result =
                RAH_NAMESPACE::move_backward(std::move(first), std::move(mid), std::move(last));
            return {std::move(result.out), std::move(last)};
        }

        template <
            typename I,
            typename S,
            std::enable_if_t<
                permutable<I> && sentinel_for<S, I> && bidirectional_iterator<I>
                && RAH_NAMESPACE::assignable_from<I&, S>>* = nullptr>
        constexpr RAH_NAMESPACE::subrange<I> operator()(I first, S last, iter_difference_t<I> n) const
        {
            auto last2 = first;
            last2 = last;
            return shift_right_fn::impl(first, last2, n);
        }
        template <
            typename I,
            typename S,
            std::enable_if_t<
                permutable<I> && sized_sentinel_for<S, I>
                && !(bidirectional_iterator<I> && assignable_from<I&, S>)&&random_access_iterator<I>>* = nullptr>
        constexpr RAH_NAMESPACE::subrange<I> operator()(I first, S last, iter_difference_t<I> n) const
        {
            auto last2 = first + (last - first);
            return shift_right_fn::impl(first, last2, n);
        }
        template <
            typename I,
            typename S,
            std::enable_if_t<
                permutable<I> && sentinel_for<S, I>
                && !(bidirectional_iterator<I> && assignable_from<I&, S>)&&!(
                    sized_sentinel_for<S, I> && random_access_iterator<I>)>* = nullptr>
        constexpr RAH_NAMESPACE::subrange<I> operator()(I first, S last, iter_difference_t<I> n) const
        {
            if (n <= 0)
            {
                return first;
            }
            auto result = first;
            if (RAH_NAMESPACE::advance(result, n, last) != 0)
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
                    auto move_in_out =
                        RAH_NAMESPACE::move(std::move(first), std::move(trail), std::move(result));
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
                        trail = std::move(mid, result, std::move(trail));
                        auto move_in_out =
                            RAH_NAMESPACE::move(std::move(first), std::move(mid), std::move(trail));
                        return {move_in_out.out, trail};
                    }
                    std::iter_swap(mid, trail);
                }
            }
        }

        template <typename R, std::enable_if_t<forward_range<R> && permutable<iterator_t<R>>>* = nullptr>
        constexpr borrowed_subrange_t<R> operator()(R&& r, range_difference_t<R> n) const
        {
            return (*this)(RAH_NAMESPACE::begin(r), RAH_NAMESPACE::end(r), n);
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
            std::enable_if_t<!RAH_NAMESPACE::forward_iterator<I>>* = nullptr>
        O operator()(I first, S last, O out, RAH_NAMESPACE::iter_difference_t<I> n, Gen&& gen) const
        {
            using diff_t = RAH_NAMESPACE::iter_difference_t<I>;
            using distrib_t = std::uniform_int_distribution<diff_t>;
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
            std::enable_if_t<RAH_NAMESPACE::forward_iterator<I>>* = nullptr>
        O operator()(I first, S last, O out, RAH_NAMESPACE::iter_difference_t<I> n, Gen&& gen) const
        {
            using diff_t = RAH_NAMESPACE::iter_difference_t<I>;
            using distrib_t = std::uniform_int_distribution<diff_t>;
            using param_t = typename distrib_t::param_type;
            distrib_t D{};

            // this branch preserves "stability" of the sample elements
            auto rest{RAH_NAMESPACE::distance(first, last)};
            for (n = RAH_STD::min(n, rest); n != 0; ++first)
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
        O operator()(R&& r, O out, RAH_NAMESPACE::range_difference_t<R> n, Gen&& gen) const
        {
            return (*this)(
                RAH_NAMESPACE::begin(r),
                RAH_NAMESPACE::end(r),
                std::move(out),
                n,
                std::forward<Gen>(gen));
        }
    };

    constexpr sample_fn sample{};

    template <class I, class O>
    using unique_copy_result = RAH_NAMESPACE::in_out_result<I, O>;

    struct unique_copy_fn
    {
        template <
            typename I, // std::input_iterator
            typename S, // std::sentinel_for<I>
            typename O, // std::weakly_incrementable
            typename C = RAH_NAMESPACE::equal_to, // std::indirect_equivalence_relation<std::projected<I, Proj>>
            std::enable_if_t<input_iterator<I> && sentinel_for<S, I>>* = nullptr>
        //requires std::indirectly_copyable<I, O> &&(
        //    std::forward_iterator<
        //        I> or (std::input_iterator<O> && std::same_as<std::iter_value_t<I>, std::iter_value_t<O>>)
        //    or std::indirectly_copyable_storable<I, O>)
        constexpr RAH_NAMESPACE::unique_copy_result<I, O>
        operator()(I first, S last, O result, C comp = {}) const
        {
            if (!(first == last))
            {
                RAH_NAMESPACE::iter_value_t<I> value = *first;
                *result = value;
                ++result;
                while (!(++first == last))
                {
                    auto&& value2 = *first;
                    if (!RAH_INVOKE_2(comp, value2, value))
                    {
                        value = std::forward<decltype(value2)>(value2);
                        *result = value;
                        ++result;
                    }
                }
            }

            return {std::move(first), std::move(result)};
        }

        template <
            typename R, // ranges::input_range
            typename O, // std::weakly_incrementable
            typename C = RAH_NAMESPACE::equal_to, // std::indirect_equivalence_relation<std::projected<ranges::iterator_t<R>, Proj>>
            std::enable_if_t<input_range<R>>* = nullptr>
        //requires std::indirectly_copyable<ranges::iterator_t<R>, O>
        //         && (std::forward_iterator<ranges::iterator_t<R>>
        //             or (std::input_iterator<O>
        //                 && std::same_as<ranges::range_value_t<R>, std::iter_value_t<O>>)
        //             || std::indirectly_copyable_storable<ranges::iterator_t<R>, O>)
        constexpr RAH_NAMESPACE::unique_copy_result<RAH_NAMESPACE::borrowed_iterator_t<R>, O>
        operator()(R&& r, O result, C comp = {}) const
        {
            return (*this)(
                RAH_NAMESPACE::begin(r),
                RAH_NAMESPACE::end(r),
                RAH_STD::move(result),
                RAH_STD::move(comp));
        }
    };

    constexpr unique_copy_fn unique_copy{};

    struct is_partitioned_fn
    {
        template <
            typename I, // std::input_iterator
            typename S, // std::sentinel_for<I
            typename Pred // std::indirect_unary_predicate<std::projected<I, Proj>>
            >
        constexpr bool operator()(I first, S last, Pred pred) const
        {
            for (; first != last; ++first)
                if (!RAH_INVOKE_1(pred, *first))
                    break;

            for (; first != last; ++first)
                if (RAH_INVOKE_1(pred, *first))
                    return false;

            return true;
        }

        template <
            typename R, // ranges::input_range
            typename Pred // std::indirect_unary_predicate<std::projected<ranges::iterator_t<R>, Proj>>
            >
        constexpr bool operator()(R&& r, Pred pred) const
        {
            return (*this)(RAH_NAMESPACE::begin(r), RAH_NAMESPACE::end(r), RAH_STD::ref(pred));
        }
    };

    constexpr auto is_partitioned = is_partitioned_fn();

    template <class I, class O1, class O2>
    using partition_copy_result = in_out_out_result<I, O1, O2>;

    struct partition_copy_fn
    {
        template <
            typename I, // std::input_iterator
            typename S, // std::sentinel_for<I>
            typename O1, // std::weakly_incrementable
            typename O2, // std::weakly_incrementable
            // class Proj = std::identity,
            typename Pred // std::indirect_unary_predicate<std::projected<I, Proj>>
            >
        // requires std::indirectly_copyable<I, O1> && std::indirectly_copyable<I, O2>
        constexpr RAH_NAMESPACE::partition_copy_result<I, O1, O2>
        operator()(I first, S last, O1 out_true, O2 out_false, Pred pred) const
        {
            for (; first != last; ++first)
                if (!!RAH_INVOKE_1(pred, *first))
                    *out_true = *first, ++out_true;
                else
                    *out_false = *first, ++out_false;
            return {std::move(first), std::move(out_true), std::move(out_false)};
        }

        template <
            typename R, // ranges::input_range
            typename O1, /// std::weakly_incrementable
            typename O2, // std::weakly_incrementable
            // class Proj = std::identity,
            typename Pred // std::indirect_unary_predicate<std::projected<iterator_t<R>, Proj>>
            >
        //requires std::indirectly_copyable<ranges::iterator_t<R>, O1>
        //         && std::indirectly_copyable<ranges::iterator_t<R>, O2>
        constexpr RAH_NAMESPACE::partition_copy_result<RAH_NAMESPACE::borrowed_iterator_t<R>, O1, O2>
        operator()(R&& r, O1 out_true, O2 out_false, Pred pred) const
        {
            return (*this)(
                RAH_NAMESPACE::begin(r),
                RAH_NAMESPACE::end(r),
                RAH_STD::move(out_true),
                RAH_STD::move(out_false),
                RAH_STD::move(pred));
        }
    };

    constexpr partition_copy_fn partition_copy{};

    struct partition_point_fn
    {
        template <class ForwardIt, class UnaryPredicate>
        constexpr ForwardIt operator()(ForwardIt first, ForwardIt last, UnaryPredicate p) const
        {
            for (auto length = std::distance(first, last); 0 < length;)
            {
                auto half = length / 2;
                auto middle = std::next(first, half);
                if (p(*middle))
                {
                    first = std::next(middle);
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
            return (*this)(RAH_NAMESPACE::begin(range), RAH_NAMESPACE::end(range), RAH_STD::move(p));
        }
    };

    constexpr partition_point_fn partition_point{};

    template <class I, class O>
    using partial_sort_copy_result = RAH_NAMESPACE::in_out_result<I, O>;

    struct partial_sort_copy_fn
    {
        template <
            typename I1, // std::input_iterator
            typename S1, // std::sentinel_for<I1>
            typename I2, // std::random_access_iterator
            typename S2, // std::sentinel_for<I2>
            class Comp = RAH_NAMESPACE::less>
        //requires std::indirectly_copyable<I1, I2> && std::sortable<I2, Comp, Proj2> && std::
        //    indirect_strict_weak_order<Comp, std::projected<I1, Proj1>, std::projected<I2, Proj2>>
        constexpr RAH_NAMESPACE::partial_sort_copy_result<I1, I2>
        operator()(I1 first, S1 last, I2 result_first, S2 result_last, Comp comp = {}) const
        {
            if (result_first == result_last)
                return {
                    std::move(RAH_NAMESPACE::next(std::move(first), std::move(last))),
                    std::move(result_first)};

            auto out_last{result_first};
            // copy first N elements
            for (; !(first == last or out_last == result_last); ++out_last, ++first)
                *out_last = *first;

            // convert N copied elements into a max-heap
            RAH_STD::make_heap(result_first, out_last, comp);

            // process the rest of the input range (if any), preserving the heap property
            for (; first != last; ++first)
            {
                if (RAH_INVOKE_2(comp, *first, *result_first))
                {
                    // pop out the biggest item and push in a newly found smaller one
                    RAH_STD::pop_heap(result_first, out_last, comp);
                    *(out_last - 1) = *first;
                    RAH_STD::push_heap(result_first, out_last, comp);
                }
            }

            // first N elements in the output range is still
            // a heap - convert it into a sorted range
            RAH_STD::sort_heap(result_first, out_last, comp);

            return {std::move(first), std::move(out_last)};
        }

        template <
            typename R1, // ranges::input_range
            typename R2, // ranges::random_access_range
            class Comp = RAH_NAMESPACE::less>
        //requires std::indirectly_copyable<ranges::iterator_t<R1>, ranges::iterator_t<R2>>
        //         && std::sortable<ranges::iterator_t<R2>, Comp, Proj2>
        //         && std::indirect_strict_weak_order<
        //             Comp,
        //             std::projected<ranges::iterator_t<R1>, Proj1>,
        //             std::projected<ranges::iterator_t<R2>, Proj2>>
        constexpr RAH_NAMESPACE::partial_sort_copy_result<
            RAH_NAMESPACE::borrowed_iterator_t<R1>,
            RAH_NAMESPACE::borrowed_iterator_t<R2>>
        operator()(R1&& r, R2&& result_r, Comp comp = {}) const
        {
            return (*this)(
                RAH_NAMESPACE::begin(r),
                RAH_NAMESPACE::end(r),
                RAH_NAMESPACE::begin(result_r),
                RAH_NAMESPACE::end(result_r),
                RAH_STD::move(comp));
        }
    };

    constexpr partial_sort_copy_fn partial_sort_copy{};

    // TODO : This is the slower implementation. Make a better one.
    struct inplace_merge_fn
    {
        template <
            typename I, // std::bidirectional_iterator
            typename S, // std::sentinel_for<I>
            class Comp = RAH_NAMESPACE::less,
            std::enable_if_t<bidirectional_iterator<I> && sentinel_for<S, I>>* = nullptr>
        // requires std::sortable<I, Comp, Proj>
        constexpr I operator()(I first, I middle, S last, Comp comp = {}) const
        {
            I last_it = RAH_NAMESPACE::next(middle, last);
            inplace_merge_slow(
                first,
                middle,
                last_it,
                RAH_NAMESPACE::distance(first, middle),
                RAH_NAMESPACE::distance(middle, last_it),
                std::ref(comp));
            return last_it;
        }

        template <
            typename R, // ranges::bidirectional_range
            class Comp = RAH_NAMESPACE::less,
            std::enable_if_t<bidirectional_range<R>>* = nullptr>
        // requires std::sortable<ranges::iterator_t<R>, Comp, Proj>
        constexpr RAH_NAMESPACE::borrowed_iterator_t<R>
        operator()(R&& r, RAH_NAMESPACE::iterator_t<R> middle, Comp comp = {}) const
        {
            return (*this)(
                RAH_NAMESPACE::begin(r), std::move(middle), RAH_NAMESPACE::end(r), std::move(comp));
        }

    private:
        template <class I, class Comp>
        static constexpr void inplace_merge_slow(
            I first,
            I middle,
            I last,
            RAH_NAMESPACE::iter_difference_t<I> n1,
            RAH_NAMESPACE::iter_difference_t<I> n2,
            Comp comp)
        {
            if (n1 == 0 || n2 == 0)
                return;
            if (n1 + n2 == 2 && comp(*middle, *first))
            {
                RAH_NAMESPACE::iter_swap(first, middle);
                return;
            }

            I cut1 = first, cut2 = middle;
            RAH_NAMESPACE::iter_difference_t<I> d1{}, d2{};

            if (n1 > n2)
            {
                d1 = n1 / 2;
                RAH_NAMESPACE::advance(cut1, d1);
                cut2 = RAH_NAMESPACE::lower_bound(middle, last, *cut1, std::ref(comp));
                d2 = RAH_NAMESPACE::distance(middle, cut2);
            }
            else
            {
                d2 = n2 / 2;
                RAH_NAMESPACE::advance(cut2, d2);
                cut1 = RAH_NAMESPACE::upper_bound(first, middle, *cut2, std::ref(comp));
                d1 = RAH_NAMESPACE::distance(first, cut1);
            }

            I new_middle = RAH_NAMESPACE::rotate(cut1, middle, cut2).begin();
            inplace_merge_slow(first, cut1, new_middle, d1, d2, std::ref(comp));
            inplace_merge_slow(new_middle, cut2, last, n1 - d1, n2 - d2, std::ref(comp));
        }
    };

    constexpr inplace_merge_fn inplace_merge{};

    struct includes_fn
    {
        template <
            typename I1, // std::input_iterator
            typename S1, // std::sentinel_for<I1>
            typename I2, // std::input_iterator
            typename S2, // std::sentinel_for<I2>
            typename Comp = RAH_NAMESPACE::less // std::indirect_strict_weak_order<std::projected<I1, Proj1>, std::projected<I2, Proj2>>
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
            typename Comp = RAH_NAMESPACE::less // std::indirect_strict_weak_order<
            >
        constexpr bool operator()(R1&& r1, R2&& r2, Comp comp = {}) const
        {
            return (*this)(
                RAH_NAMESPACE::begin(r1),
                RAH_NAMESPACE::end(r1),
                RAH_NAMESPACE::begin(r2),
                RAH_NAMESPACE::end(r2),
                RAH_STD::move(comp));
        }
    };

    constexpr auto includes = includes_fn{};

    struct max_fn
    {
        template <
            class T,
            typename Comp = RAH_NAMESPACE::less // std::indirect_strict_weak_order<std::projected<const T*, Proj>>
            >
        constexpr const T& operator()(const T& a, const T& b, Comp comp = {}) const
        {
            return RAH_INVOKE_2(comp, a, b) ? b : a;
        }

        template <
            typename T, // std::copyable
            typename Comp = RAH_NAMESPACE::less // std::indirect_strict_weak_order<std::projected<const T*, Proj>>
            >
        constexpr const T operator()(std::initializer_list<T> r, Comp comp = {}) const
        {
            return *RAH_NAMESPACE::max_element(r, std::ref(comp));
        }

        template <typename R, typename Comp = RAH_NAMESPACE::less, std::enable_if_t<forward_range<R>>* = nullptr>
        constexpr RAH_NAMESPACE::range_value_t<R> operator()(R&& r, Comp comp = {}) const
        {
            using V = RAH_NAMESPACE::range_value_t<R>;
            return static_cast<V>(*RAH_NAMESPACE::max_element(r, std::ref(comp)));
        }
        template <typename R, typename Comp = RAH_NAMESPACE::less, std::enable_if_t<!forward_range<R>>* = nullptr>
        constexpr RAH_NAMESPACE::range_value_t<R> operator()(R&& r, Comp comp = {}) const
        {
            using V = RAH_NAMESPACE::range_value_t<R>;
            auto i = RAH_NAMESPACE::begin(r);
            auto s = RAH_NAMESPACE::end(r);
            V m(*i);
            while (++i != s)
                if (RAH_INVOKE_2(comp, m, *i))
                    m = *i;
            return m;
        }
    };

    constexpr max_fn max;

    struct min_fn
    {
        template <class T, typename Comp = RAH_NAMESPACE::less>
        constexpr const T& operator()(const T& a, const T& b, Comp comp = {}) const
        {
            return RAH_INVOKE_2(comp, b, a) ? b : a;
        }

        template <typename T, typename Comp = RAH_NAMESPACE::less>
        constexpr const T operator()(std::initializer_list<T> r, Comp comp = {}) const
        {
            return *RAH_NAMESPACE::min_element(r, std::ref(comp));
        }

        template <typename R, typename Comp = RAH_NAMESPACE::less, std::enable_if_t<forward_range<R>>* = nullptr>
        constexpr RAH_NAMESPACE::range_value_t<R> operator()(R&& r, Comp comp = {}) const
        {
            using V = RAH_NAMESPACE::range_value_t<R>;
            return static_cast<V>(*RAH_NAMESPACE::min_element(r, std::ref(comp)));
        }
        template <typename R, typename Comp = RAH_NAMESPACE::less, std::enable_if_t<!forward_range<R>>* = nullptr>
        constexpr RAH_NAMESPACE::range_value_t<R> operator()(R&& r, Comp comp = {}) const
        {
            using V = RAH_NAMESPACE::range_value_t<R>;
            auto i = RAH_NAMESPACE::begin(r);
            auto s = RAH_NAMESPACE::end(r);
            V m(*i);
            while (++i != s)
                if (RAH_INVOKE_2(comp, *i, m))
                    m = *i;
            return m;
        }
    };

    constexpr min_fn min;

    template <class T>
    using minmax_result = RAH_NAMESPACE::min_max_result<T>;

    struct minmax_fn
    {
        template <class T, typename Comp = RAH_NAMESPACE::less>
        constexpr RAH_NAMESPACE::minmax_result<const T&>
        operator()(const T& a, const T& b, Comp comp = {}) const
        {
            if (RAH_INVOKE_2(comp, b, a))
                return {b, a};

            return {a, b};
        }

        template <typename T, typename Comp = RAH_NAMESPACE::less>
        constexpr RAH_NAMESPACE::minmax_result<T>
        operator()(std::initializer_list<T> r, Comp comp = {}) const
        {
            auto result = RAH_NAMESPACE::minmax_element(r, std::ref(comp));
            return {*result.min, *result.max};
        }

        template <typename R, typename Comp = RAH_NAMESPACE::less>
        // requires std::indirectly_copyable_storable<ranges::iterator_t<R>, ranges::range_value_t<R>*>
        constexpr RAH_NAMESPACE::minmax_result<RAH_NAMESPACE::range_value_t<R>>
        operator()(R&& r, Comp comp = {}) const
        {
            auto result = RAH_NAMESPACE::minmax_element(r, std::ref(comp));
            return {std::move(*result.min), std::move(*result.max)};
        }
    };

    constexpr minmax_fn minmax;

    template <class I>
    using prev_permutation_result = RAH_NAMESPACE::in_found_result<I>;

    struct prev_permutation_fn
    {
        template <
            typename I,
            typename S,
            class Comp = RAH_NAMESPACE::less,
            std::enable_if_t<
                RAH_NAMESPACE::bidirectional_iterator<I> && RAH_NAMESPACE::sentinel_for<S, I>>* = nullptr>
        // requires std::sortable<I, Comp, Proj>
        constexpr RAH_NAMESPACE::prev_permutation_result<I>
        operator()(I first, S last, Comp comp = {}) const
        {
            // check that the sequence has at least two elements
            if (first == last)
                return {std::move(first), false};
            auto i{first};
            ++i;
            if (i == last)
                return {std::move(i), false};
            auto i_last{RAH_NAMESPACE::next(first, last)};
            i = i_last;
            --i;
            // main "permutating" loop
            for (;;)
            {
                auto i1{i};
                --i;
                if (RAH_INVOKE_2(comp, *i1, *i))
                {
                    auto j{i_last};
                    while (!RAH_INVOKE_2(comp, *--j, *i))
                        ;
                    RAH_NAMESPACE::iter_swap(i, j);
                    RAH_NAMESPACE::reverse(i1, last);
                    return {std::move(i_last), true};
                }
                // permutation "space" is exhausted
                if (i == first)
                {
                    RAH_NAMESPACE::reverse(first, last);
                    return {std::move(i_last), false};
                }
            }
        }

        template <
            typename R, // rah::bidirectional_range
            class Comp = RAH_NAMESPACE::less,
            std::enable_if_t<bidirectional_range<R>>* = nullptr>
        // requires std::sortable<rah::iterator_t<R>, Comp, Proj>
        constexpr RAH_NAMESPACE::prev_permutation_result<rah::borrowed_iterator_t<R>>
        operator()(R&& r, Comp comp = {}) const
        {
            return (*this)(RAH_NAMESPACE::begin(r), RAH_NAMESPACE::end(r), RAH_STD::move(comp));
        }
    };

    constexpr prev_permutation_fn prev_permutation{};

    template <class O, class T>
    using iota_result = RAH_NAMESPACE::out_value_result<O, T>;

    struct iota_fn
    {
        template <
            typename O, // std::input_or_output_iterator
            typename S, // std::sentinel_for<O>
            typename T // std::weakly_incrementable
            >
        // requires std::indirectly_writable<O, const T&>
        constexpr iota_result<O, T> operator()(O first, S last, T value) const
        {
            while (first != last)
            {
                *first = RAH_NAMESPACE::as_const(value);
                ++first;
                ++value;
            }
            return {RAH_STD::move(first), RAH_STD::move(value)};
        }

        template <
            typename T, // std::weakly_incrementable
            typename R // rah::output_range<const T&>
            >
        constexpr iota_result<RAH_NAMESPACE::borrowed_iterator_t<R>, T> operator()(R&& r, T value) const
        {
            return (*this)(RAH_NAMESPACE::begin(r), RAH_NAMESPACE::end(r), RAH_STD::move(value));
        }
    };

    constexpr iota_fn iota;

    template <class I, class O>
    using uninitialized_copy_result = RAH_NAMESPACE::in_out_result<I, O>;

    struct uninitialized_copy_fn
    {
        // TODO : improve efficiency when the copied type is a TrivialType
        template <
            typename I, // std::input_iterator
            typename S1, // std::sentinel_for<I>
            typename O, // no-throw-forward-iterator
            typename S2 // no-throw-sentinel-for<O>
            >
        // requires std::constructible_from<std::iter_value_t<O>, std::iter_reference_t<I>>
        RAH_NAMESPACE::uninitialized_copy_result<I, O>
        operator()(I ifirst, S1 ilast, O ofirst, S2 olast) const
        {
            O current{ofirst};
            try
            {
                for (; !(ifirst == ilast or current == olast); ++ifirst, ++current)
                    RAH_NAMESPACE::construct_at(std::addressof(*current), *ifirst);
                return {std::move(ifirst), std::move(current)};
            }
            catch (...) // rollback: destroy constructed elements
            {
                for (; ofirst != current; ++ofirst)
                    RAH_NAMESPACE::destroy_at(std::addressof(*ofirst));
                throw;
            }
        }

        template <
            typename IR, // ranges::input_range
            typename OR // no-throw-forward-range
            >
        // requires std::constructible_from<ranges::range_value_t<OR>, ranges::range_reference_t<IR>>
        RAH_NAMESPACE::uninitialized_copy_result<
            RAH_NAMESPACE::borrowed_iterator_t<IR>,
            RAH_NAMESPACE::borrowed_iterator_t<OR>>
        operator()(IR&& in_range, OR&& out_range) const
        {
            return (*this)(
                RAH_NAMESPACE::begin(in_range),
                RAH_NAMESPACE::end(in_range),
                RAH_NAMESPACE::begin(out_range),
                RAH_NAMESPACE::end(out_range));
        }
    };

    constexpr uninitialized_copy_fn uninitialized_copy{};

    template <class I, class O>
    using uninitialized_copy_n_result = RAH_NAMESPACE::in_out_result<I, O>;

    struct uninitialized_copy_n_fn
    {
        // TODO : improve efficiency when the copied type is a TrivialType
        template <
            typename I, // std::input_iterator
            typename O, // no-throw-input-iterator
            typename S // no-throw-sentinel-for<O>
            >
        // requires std::constructible_from<std::iter_value_t<O>, std::iter_reference_t<I>>
        RAH_NAMESPACE::uninitialized_copy_n_result<I, O>
        operator()(I ifirst, RAH_NAMESPACE::iter_difference_t<I> count, O ofirst, S olast) const
        {
            O current{ofirst};
            try
            {
                for (; count > 0 && current != olast; ++ifirst, ++current, --count)
                    RAH_NAMESPACE::construct_at(std::addressof(*current), *ifirst);
                return {std::move(ifirst), std::move(current)};
            }
            catch (...) // rollback: destroy constructed elements
            {
                for (; ofirst != current; ++ofirst)
                    RAH_NAMESPACE::destroy_at(std::addressof(*ofirst));
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
        // requires std::constructible_from<std::iter_value_t<I>, const T&>
        I operator()(I first, S last, const T& x) const
        {
            I rollback{first};
            try
            {
                for (; !(first == last); ++first)
                    RAH_NAMESPACE::construct_at(std::addressof(*first), x);
                return first;
            }
            catch (...)
            {
                // rollback: destroy constructed elements
                for (; rollback != first; ++rollback)
                    RAH_NAMESPACE::destroy_at(std::addressof(*rollback));
                throw;
            }
        }

        template <
            typename R, // no-throw-forward-range
            class T>
        // requires std::constructible_from<ranges::range_value_t<R>, const T&>
        RAH_NAMESPACE::borrowed_iterator_t<R> operator()(R&& r, const T& x) const
        {
            return (*this)(RAH_NAMESPACE::begin(r), RAH_NAMESPACE::end(r), x);
        }
    };

    constexpr uninitialized_fill_fn uninitialized_fill{};

    struct uninitialized_fill_n_fn
    {
        template <
            typename I, // no-throw-forward-range
            class T>
        // requires std::constructible_from<std::iter_value_t<I>, const T&>
        I operator()(I first, RAH_NAMESPACE::iter_difference_t<I> n, const T& x) const
        {
            I rollback{first};
            try
            {
                for (; n-- > 0; ++first)
                    RAH_NAMESPACE::construct_at(std::addressof(*first), x);
                return first;
            }
            catch (...) // rollback: destroy constructed elements
            {
                for (; rollback != first; ++rollback)
                    RAH_NAMESPACE::destroy_at(std::addressof(*rollback));
                throw;
            }
        }
    };

    constexpr uninitialized_fill_n_fn uninitialized_fill_n{};

    template <class I, class O>
    using uninitialized_move_result = RAH_NAMESPACE::in_out_result<I, O>;

    struct uninitialized_move_fn
    {
        template <
            typename I, // std::input_iterator
            typename S1, // std::sentinel_for<I>
            typename O, // no-throw-forward-iterator
            typename S2 // no-throw-sentinel-for<O>
            >
        //requires std::constructible_from<std::iter_value_t<O>, std::iter_rvalue_reference_t<I>>
        RAH_NAMESPACE::uninitialized_move_result<I, O>
        operator()(I ifirst, S1 ilast, O ofirst, S2 olast) const
        {
            O current{ofirst};
            try
            {
                for (; !(ifirst == ilast or current == olast); ++ifirst, ++current)
                    ::new (const_cast<void*>(
                        static_cast<const volatile void*>(std::addressof(*current))))
                        std::remove_reference_t<RAH_NAMESPACE::iter_reference_t<O>>(
                            RAH_NAMESPACE::iter_move(ifirst));
                return {std::move(ifirst), std::move(current)};
            }
            catch (...) // rollback: destroy constructed elements
            {
                for (; ofirst != current; ++ofirst)
                    RAH_NAMESPACE::destroy_at(std::addressof(*ofirst));
                throw;
            }
        }

        template <
            typename IR, // ranges::input_range
            typename OR // no-throw-forward-range
            >
        // requires std::constructible_from<ranges::range_value_t<OR>, ranges::range_rvalue_reference_t<IR>>
        RAH_NAMESPACE::uninitialized_move_result<
            RAH_NAMESPACE::borrowed_iterator_t<IR>,
            RAH_NAMESPACE::borrowed_iterator_t<OR>>
        operator()(IR&& in_range, OR&& out_range) const
        {
            return (*this)(
                RAH_NAMESPACE::begin(in_range),
                RAH_NAMESPACE::end(in_range),
                RAH_NAMESPACE::begin(out_range),
                RAH_NAMESPACE::end(out_range));
        }
    };

    constexpr uninitialized_move_fn uninitialized_move{};

    template <class I, class O>
    using uninitialized_move_n_result = RAH_NAMESPACE::in_out_result<I, O>;

    struct uninitialized_move_n_fn
    {
        template <
            typename I, // std::input_iterator
            typename O, // no-throw-forward-iterator
            typename S // no-throw-sentinel-for<O>
            >
        // requires std::constructible_from<std::iter_value_t<O>, std::iter_rvalue_reference_t<I>>
        RAH_NAMESPACE::uninitialized_move_n_result<I, O>
        operator()(I ifirst, RAH_NAMESPACE::iter_difference_t<I> n, O ofirst, S olast) const
        {
            O current{ofirst};
            try
            {
                for (; n-- > 0 && current != olast; ++ifirst, ++current)
                    ::new (const_cast<void*>(
                        static_cast<const volatile void*>(std::addressof(*current))))
                        std::remove_reference_t<RAH_NAMESPACE::iter_reference_t<O>>(
                            RAH_NAMESPACE::iter_move(ifirst));
                return {std::move(ifirst), std::move(current)};
            }
            catch (...) // rollback: destroy constructed elements
            {
                for (; ofirst != current; ++ofirst)
                    RAH_NAMESPACE::destroy_at(std::addressof(*ofirst));
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
            std::enable_if_t<std::is_trivially_default_constructible<
                std::remove_reference_t<RAH_NAMESPACE::iter_reference_t<I>>>::value>* = nullptr>
        // requires std::default_initializable<std::iter_value_t<I>>
        I operator()(I first, S last) const
        {
            return RAH_NAMESPACE::next(first, last); // skip initialization
        }
        template <
            typename I, // no-throw-forward-iterator
            typename S, // no-throw-sentinel-for<I>
            std::enable_if_t<!std::is_trivially_default_constructible<
                std::remove_reference_t<RAH_NAMESPACE::iter_reference_t<I>>>::value>* = nullptr>
        // requires std::default_initializable<std::iter_value_t<I>>
        I operator()(I first, S last) const
        {
            using ValueType = std::remove_reference_t<RAH_NAMESPACE::iter_reference_t<I>>;
            I rollback{first};
            try
            {
                for (; !(first == last); ++first)
                    ::new (const_cast<void*>(
                        static_cast<const volatile void*>(std::addressof(*first)))) ValueType;
                return first;
            }
            catch (...) // rollback: destroy constructed elements
            {
                for (; rollback != first; ++rollback)
                    RAH_NAMESPACE::destroy_at(std::addressof(*rollback));
                throw;
            }
        }

        template <typename R // no-throw-forward-range
                  >
        // requires std::default_initializable<ranges::range_value_t<R>>
        RAH_NAMESPACE::borrowed_iterator_t<R> operator()(R&& r) const
        {
            return (*this)(RAH_NAMESPACE::begin(r), RAH_NAMESPACE::end(r));
        }
    };

    constexpr uninitialized_default_construct_fn uninitialized_default_construct{};

    struct uninitialized_default_construct_n_fn
    {
        template <
            typename I, // no-throw-forward-iterator
            std::enable_if_t<std::is_trivially_default_constructible<
                std::remove_reference_t<RAH_NAMESPACE::iter_reference_t<I>>>::value>* = nullptr>
        // requires std::default_initializable<std::iter_value_t<I>>
        I operator()(I first, RAH_NAMESPACE::iter_difference_t<I> n) const
        {
            return RAH_NAMESPACE::next(first, n); // skip initialization
        }

        template <
            typename I, // no-throw-forward-iterator
            std::enable_if_t<!std::is_trivially_default_constructible<
                std::remove_reference_t<RAH_NAMESPACE::iter_reference_t<I>>>::value>* = nullptr>
        // requires std::default_initializable<std::iter_value_t<I>>
        I operator()(I first, RAH_NAMESPACE::iter_difference_t<I> n) const
        {
            using ValueType = std::remove_reference_t<RAH_NAMESPACE::iter_reference_t<I>>;
            I rollback{first};
            try
            {
                for (; n-- > 0; ++first)
                    ::new (const_cast<void*>(
                        static_cast<const volatile void*>(std::addressof(*first)))) ValueType;
                return first;
            }
            catch (...) // rollback: destroy constructed elements
            {
                for (; rollback != first; ++rollback)
                    RAH_NAMESPACE::destroy_at(std::addressof(*rollback));
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
            typename T = std::remove_reference_t<RAH_NAMESPACE::iter_reference_t<I>>,
            std::enable_if_t<std::is_trivial<T>::value && std::is_copy_assignable<T>::value>* = nullptr>
        // requires std::default_initializable<std::iter_value_t<I>>
        I operator()(I first, S last) const
        {
            return RAH_NAMESPACE::fill(first, last, T());
        }

        template <
            typename I, // no-throw-forward-iterator
            typename S, // no-throw-sentinel-for<I>
            typename T = std::remove_reference_t<RAH_NAMESPACE::iter_reference_t<I>>,
            std::enable_if_t<!(std::is_trivial<T>::value && std::is_copy_assignable<T>::value)>* = nullptr>
        // requires std::default_initializable<std::iter_value_t<I>>
        I operator()(I first, S last) const
        {
            I rollback{first};
            try
            {
                for (; !(first == last); ++first)
                    ::new (const_cast<void*>(
                        static_cast<const volatile void*>(std::addressof(*first)))) T();
                return first;
            }
            catch (...) // rollback: destroy constructed elements
            {
                for (; rollback != first; ++rollback)
                    RAH_NAMESPACE::destroy_at(std::addressof(*rollback));
                throw;
            }
        }

        template <typename R // no-throw-forward-range
                  >
        // requires std::default_initializable<ranges::range_value_t<R>>
        RAH_NAMESPACE::borrowed_iterator_t<R> operator()(R&& r) const
        {
            return (*this)(RAH_NAMESPACE::begin(r), RAH_NAMESPACE::end(r));
        }
    };

    constexpr uninitialized_value_construct_fn uninitialized_value_construct{};

    struct uninitialized_value_construct_n_fn
    {
        template <
            typename I, // no-throw-forward-iterator
            typename T = std::remove_reference_t<RAH_NAMESPACE::iter_reference_t<I>>,
            std::enable_if_t<(std::is_trivial<T>::value && std::is_copy_assignable<T>::value)>* = nullptr>
        I operator()(I first, RAH_NAMESPACE::iter_difference_t<I> n) const
        {
            return RAH_NAMESPACE::fill_n(first, n, T());
        }

        template <
            typename I, // no-throw-forward-iterator
            typename T = std::remove_reference_t<RAH_NAMESPACE::iter_reference_t<I>>,
            std::enable_if_t<!(std::is_trivial<T>::value && std::is_copy_assignable<T>::value)>* = nullptr>
        I operator()(I first, RAH_NAMESPACE::iter_difference_t<I> n) const
        {
            I rollback{first};
            try
            {
                for (; n-- > 0; ++first)
                    ::new (const_cast<void*>(
                        static_cast<const volatile void*>(std::addressof(*first)))) T();
                return first;
            }
            catch (...) // rollback: destroy constructed elements
            {
                for (; rollback != first; ++rollback)
                    RAH_NAMESPACE::destroy_at(std::addressof(*rollback));
                throw;
            }
        }
    };

    constexpr uninitialized_value_construct_n_fn uninitialized_value_construct_n{};

} // namespace rah
