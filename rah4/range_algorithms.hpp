#pragma once

#include "range_bases.hpp"

#include <algorithm> // TODO : Remove this dependency
#include <numeric> // TODO : Remove this dependency
#include <iterator>

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
            U accum = std::invoke(f, std::move(init), *first);
            for (++first; first != last; ++first)
                accum = std::invoke(f, std::move(accum), *first);
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
                if (std::invoke(proj, *first) == value)
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
                if (std::invoke(pred, std::invoke(proj, *first)))
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
                if (!std::invoke(pred, std::invoke(proj, *first)))
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
} // namespace rah
