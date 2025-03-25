#pragma once

#include "range_bases.hpp"
#include "algo_sort.hpp"
#include "algo_heap.hpp"

#ifdef RAH2_USE_EASTL

#include <EASTL/random.h> // uniform_int_distribution

#else

#include <random> // uniform_int_distribution

#endif

namespace RAH2_NS
{
    namespace ranges
    {
        template <class I, class T>
        using fold_left_with_iter_result = in_value_result<I, T>;

        template <class I, class T>
        using fold_left_first_with_iter_result = RAH2_NS::ranges::in_value_result<I, T>;

        namespace niebloids
        {
            struct fold_left_fn
            {
                template <typename I, typename S, class T, typename F>
                constexpr auto operator()(I first, S last, T init, F f) const
                {
                    using U = RAH2_NS::remove_cvref_t<decltype(f(init, *first))>;
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
                        RAH2_NS::ranges::begin(r),
                        RAH2_NS::ranges::end(r),
                        RAH2_STD::move(init),
                        RAH2_STD::ref(f));
                }
            };

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
                    using U = decltype(fold_left_fn{}(
                        RAH2_STD::move(first), last, RAH2_NS::iter_value_t<I>(*first), f));
                    if (first == last)
                        return RAH2_NS::details::movable_box<U>();
                    RAH2_NS::details::movable_box<U> init(*first);
                    for (++first; first != last; ++first)
                        *init = RAH2_INVOKE_2(f, RAH2_STD::move(*init), *first);
                    return init;
                }

                template <
                    typename R, // input_range
                    typename F // __indirectly_binary_left_foldable<ranges::range_value_t<R>, ranges::iterator_t<R>>
                    >
                // requires RAH2_STD::constructible_from<ranges::range_value_t<R>, ranges::range_reference_t<R>>
                constexpr auto operator()(R&& r, F f) const
                {
                    return (*this)(
                        RAH2_NS::ranges::begin(r), RAH2_NS::ranges::end(r), RAH2_STD::ref(f));
                }
            };

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
                    using U =
                        RAH2_STD::decay_t<decltype(RAH2_INVOKE_2(f, *first, RAH2_STD::move(init)))>;
                    if (first == last)
                        return U(RAH2_STD::move(init));
                    I tail = RAH2_NS::ranges::next(first, last);
                    U accum = RAH2_INVOKE_2(f, *--tail, RAH2_STD::move(init));
                    while (first != tail)
                        accum = RAH2_INVOKE_2(f, *--tail, RAH2_STD::move(accum));
                    return accum;
                }

                template <
                    typename R, // RAH2_NS::bidirectional_range
                    class T,
                    typename F // __indirectly_binary_right_foldable<T, ranges::iterator_t<R>>
                    >
                constexpr auto operator()(R&& r, T init, F f) const
                {
                    return (*this)(
                        RAH2_NS::ranges::begin(r),
                        RAH2_NS::ranges::end(r),
                        RAH2_STD::move(init),
                        RAH2_STD::ref(f));
                }
            };

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
                    using U =
                        decltype(fold_right_fn{}(first, last, RAH2_NS::iter_value_t<I>(*first), f));

                    if (first == last)
                        return RAH2_NS::details::movable_box<U>();
                    I tail = RAH2_STD::prev(RAH2_NS::ranges::next(first, RAH2_STD::move(last)));
                    return RAH2_NS::details::movable_box<U>(fold_right_fn{}(
                        RAH2_STD::move(first),
                        tail,
                        RAH2_NS::iter_value_t<I>(*tail),
                        RAH2_STD::move(f)));
                }

                template <
                    typename R, // ranges::bidirectional_range
                    typename F // __indirectly_binary_right_foldable<ranges::range_value_t<R>, ranges::iterator_t<R>>
                    >
                // requires RAH2_STD::constructible_from<ranges::range_value_t<R>, ranges::range_reference_t<R>>
                constexpr auto operator()(R&& r, F f) const
                {
                    return (*this)(
                        RAH2_NS::ranges::begin(r), RAH2_NS::ranges::end(r), RAH2_STD::ref(f));
                }
            };

            class fold_left_with_iter_fn
            {
                template <class O, class I, class S, class T, class F>
                constexpr auto impl(I&& first, S&& last, T&& init, F f) const
                {
                    using U =
                        RAH2_STD::decay_t<decltype(RAH2_INVOKE_2(f, RAH2_STD::forward<T>(init), *first))>;
                    using Ret = fold_left_with_iter_result<O, U>;
                    if (first == last)
                        return Ret{O(RAH2_STD::forward<I>(first)), U(RAH2_STD::forward<T>(init))};
                    U accum = RAH2_INVOKE_2(f, RAH2_STD::forward<T>(init), *first);
                    for (++first; first != last; ++first)
                        accum = RAH2_INVOKE_2(f, RAH2_STD::move(accum), *first);
                    return Ret{O(RAH2_STD::forward<I>(first)), RAH2_STD::move(accum)};
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
                        RAH2_STD::move(first),
                        RAH2_STD::move(last),
                        RAH2_STD::move(init),
                        RAH2_STD::ref(f));
                }

                template <
                    typename R, // ranges::input_range
                    class T,
                    typename F // __indirectly_binary_left_foldable<T, ranges::iterator_t<R>>
                    >
                constexpr auto operator()(R&& r, T init, F f) const
                {
                    return impl<RAH2_NS::ranges::borrowed_iterator_t<R>>(
                        RAH2_NS::ranges::begin(r),
                        RAH2_NS::ranges::end(r),
                        RAH2_STD::move(init),
                        RAH2_STD::ref(f));
                }
            };

            class fold_left_first_with_iter_fn
            {
                template <class O, class I, class S, class F>
                constexpr auto impl(I&& first, S&& last, F f) const
                {
                    using U = decltype(fold_left_fn{}(
                        RAH2_STD::forward<I>(first), last, RAH2_NS::iter_value_t<I>(*first), f));
                    using Ret = RAH2_NS::ranges::
                        fold_left_first_with_iter_result<O, RAH2_NS::details::movable_box<U>>;
                    if (first == last)
                        return Ret{
                            O(RAH2_STD::forward<I>(first)), RAH2_NS::details::movable_box<U>()};
                    RAH2_NS::details::movable_box<U> init(RAH2_NS::in_place, *first);
                    for (++first; first != last; ++first)
                        *init = RAH2_INVOKE_2(f, RAH2_STD::move(*init), *first);
                    return Ret{O(RAH2_STD::forward<I>(first)), RAH2_STD::move(init)};
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
                    return impl<RAH2_NS::ranges::borrowed_iterator_t<R>>(
                        RAH2_NS::ranges::begin(r), RAH2_NS::ranges::end(r), RAH2_STD::ref(f));
                }
            };
        } // namespace niebloids

        constexpr niebloids::fold_left_fn fold_left;

        constexpr niebloids::fold_left_first_fn fold_left_first;

        constexpr niebloids::fold_right_fn fold_right;

        constexpr niebloids::fold_right_last_fn fold_right_last;

        constexpr niebloids::fold_left_with_iter_fn fold_left_with_iter;

        constexpr niebloids::fold_left_first_with_iter_fn fold_left_first_with_iter;

        namespace niebloids
        {
            struct find_last_fn
            {
            private:
                template <
                    typename I,
                    typename S,
                    class T,
                    class Proj = RAH2_NS::details::identity,
                    RAH2_STD::enable_if_t<
                        forward_iterator<I> && sentinel_for<S, I>
                        && not(bidirectional_iterator<I> && RAH2_NS::is_same_v<I, S>)>* = nullptr>
                constexpr RAH2_NS::ranges::subrange<I>
                impl(I first, S last, T const& value, Proj proj = {}) const
                {
                    for (; first != last; ++first)
                    {
                        if (RAH2_INVOKE_1(proj, *first) == value)
                        {
                            I found = first;
                            for (; first != last; ++first)
                            {
                                if (RAH2_INVOKE_1(proj, *first) == value)
                                {
                                    found = first;
                                }
                            }
                            return {found, RAH2_NS::ranges::next(found, last)};
                        }
                    }
                    return {first, first};
                }

                template <
                    typename I,
                    class T,
                    class Proj = RAH2_NS::details::identity,
                    RAH2_STD::enable_if_t<bidirectional_iterator<I>>* = nullptr>
                constexpr RAH2_NS::ranges::subrange<I>
                impl(I first, I last, T const& value, Proj proj = {}) const
                {
                    I last_save = last;
                    if (first == last)
                    {
                        return {last_save, last_save};
                    }
                    do
                    {
                        --last;
                        if (RAH2_INVOKE_1(proj, *last) == value)
                        {
                            return {last, last_save};
                        }
                    } while (first != last);

                    return {last_save, last_save};
                }

            public:
                template <
                    typename I,
                    typename S,
                    class T,
                    class Proj = RAH2_NS::details::identity,
                    RAH2_STD::enable_if_t<forward_iterator<I> && sentinel_for<S, I>>* = nullptr>
                constexpr RAH2_NS::ranges::subrange<I>
                operator()(I first, S last, T const& value, Proj proj = {}) const
                {
                    auto first_last = details::unwrap(RAH2_STD::move(first), RAH2_STD::move(last));
                    auto result = impl(
                        RAH2_STD::move(first_last.iterator),
                        RAH2_STD::move(first_last.sentinel),
                        value,
                        details::move_unary(proj));
                    return {
                        first_last.wrap_iterator(result.begin()),
                        first_last.wrap_iterator(result.end())};
                }

                template <
                    typename R,
                    class T,
                    class Proj = RAH2_NS::details::identity,
                    RAH2_STD::enable_if_t<forward_range<R>>* = nullptr>
                constexpr RAH2_NS::ranges::borrowed_subrange_t<R>
                operator()(R&& r, T const& value, Proj proj = {}) const
                {
                    auto first_last =
                        details::unwrap(RAH2_NS::ranges::begin(r), RAH2_NS::ranges::end(r));
                    auto result = impl(
                        RAH2_STD::move(first_last.iterator),
                        RAH2_STD::move(first_last.sentinel),
                        value,
                        details::move_unary(proj));
                    return {
                        first_last.wrap_iterator(result.begin()),
                        first_last.wrap_iterator(result.end())};
                }
            };
        } // namespace niebloids
        constexpr niebloids::find_last_fn find_last;

        namespace niebloids
        {
            struct find_last_if_fn
            {
            private:
                template <
                    typename I,
                    typename S,
                    class Pred,
                    RAH2_STD::enable_if_t<
                        forward_iterator<I> && sentinel_for<S, I>
                        && not(bidirectional_iterator<I> && RAH2_NS::is_same_v<I, S>)>* = nullptr>
                constexpr RAH2_NS::ranges::subrange<I> impl(I first, S last, Pred&& pred) const
                {
                    for (; first != last; ++first)
                    {
                        if (RAH2_INVOKE_1(pred, *first))
                        {
                            I found = first;
                            for (; first != last; ++first)
                            {
                                if (RAH2_INVOKE_1(pred, *first))
                                {
                                    found = first;
                                }
                            }
                            return {found, RAH2_NS::ranges::next(found, last)};
                        }
                    }
                    return {first, first};
                }

                template <typename I, class Pred, RAH2_STD::enable_if_t<bidirectional_iterator<I>>* = nullptr>
                constexpr RAH2_NS::ranges::subrange<I> impl(I first, I last, Pred&& pred) const
                {
                    I last_save = last;
                    if (first == last)
                    {
                        return {last_save, last_save};
                    }
                    do
                    {
                        --last;
                        if (RAH2_INVOKE_1(pred, *last))
                        {
                            return {last, last_save};
                        }
                    } while (first != last);

                    return {last_save, last_save};
                }

            public:
                template <
                    typename I,
                    typename S,
                    class Pred,
                    class Proj = RAH2_NS::details::identity,
                    RAH2_STD::enable_if_t<forward_iterator<I> && sentinel_for<S, I>>* = nullptr>
                constexpr RAH2_NS::ranges::subrange<I>
                operator()(I first, S last, Pred&& pred, Proj&& proj = {}) const
                {
                    auto first_last = details::unwrap(RAH2_STD::move(first), RAH2_STD::move(last));
                    auto result = impl(
                        RAH2_STD::move(first_last.iterator),
                        RAH2_STD::move(first_last.sentinel),
                        details::wrap_pred_proj(
                            RAH2_STD::forward<Pred>(pred), RAH2_STD::forward<Proj>(proj)));
                    return {
                        first_last.wrap_iterator(result.begin()),
                        first_last.wrap_iterator(result.end())};
                }

                template <
                    typename R,
                    class Pred,
                    class Proj = RAH2_NS::details::identity,
                    RAH2_STD::enable_if_t<forward_range<R>>* = nullptr>
                constexpr RAH2_NS::ranges::borrowed_subrange_t<R>
                operator()(R&& r, Pred&& pred, Proj&& proj = {}) const
                {
                    auto first_last =
                        details::unwrap(RAH2_NS::ranges::begin(r), RAH2_NS::ranges::end(r));
                    auto result = impl(
                        RAH2_STD::move(first_last.iterator),
                        RAH2_STD::move(first_last.sentinel),
                        details::wrap_pred_proj(
                            RAH2_STD::forward<Pred>(pred), RAH2_STD::forward<Proj>(proj)));
                    return {
                        first_last.wrap_iterator(result.begin()),
                        first_last.wrap_iterator(result.end())};
                }
            };
        } // namespace niebloids

        constexpr niebloids::find_last_if_fn find_last_if;

        namespace niebloids
        {
            struct find_last_if_not_fn
            {
            private:
                template <
                    typename I,
                    typename S,
                    class Pred,
                    RAH2_STD::enable_if_t<
                        forward_iterator<I> && sentinel_for<S, I>
                        && not(bidirectional_iterator<I> && RAH2_NS::is_same_v<I, S>)>* = nullptr>
                constexpr RAH2_NS::ranges::subrange<I> impl(I first, S last, Pred&& pred) const
                {
                    for (; first != last; ++first)
                    {
                        if (not RAH2_INVOKE_1(pred, *first))
                        {
                            I found = first;
                            for (; first != last; ++first)
                            {
                                if (not RAH2_INVOKE_1(pred, *first))
                                {
                                    found = first;
                                }
                            }
                            return {found, RAH2_NS::ranges::next(found, last)};
                        }
                    }
                    return {first, first};
                }

                template <typename I, class Pred, RAH2_STD::enable_if_t<bidirectional_iterator<I>>* = nullptr>
                constexpr RAH2_NS::ranges::subrange<I> impl(I first, I last, Pred&& pred) const
                {
                    I last_save = last;
                    if (first == last)
                    {
                        return {last_save, last_save};
                    }
                    do
                    {
                        --last;
                        if (not RAH2_INVOKE_1(pred, *last))
                        {
                            return {last, last_save};
                        }
                    } while (first != last);

                    return {last_save, last_save};
                }

            public:
                template <
                    typename I,
                    typename S,
                    class Pred,
                    class Proj = RAH2_NS::details::identity,
                    RAH2_STD::enable_if_t<forward_iterator<I> && sentinel_for<S, I>>* = nullptr>
                constexpr RAH2_NS::ranges::subrange<I>
                operator()(I first, S last, Pred&& pred, Proj&& proj = {}) const
                {
                    auto first_last = details::unwrap(RAH2_STD::move(first), RAH2_STD::move(last));
                    auto result = impl(
                        RAH2_STD::move(first_last.iterator),
                        RAH2_STD::move(first_last.sentinel),
                        details::wrap_pred_proj(
                            RAH2_STD::forward<Pred>(pred), RAH2_STD::forward<Proj>(proj)));
                    return {
                        first_last.wrap_iterator(result.begin()),
                        first_last.wrap_iterator(result.end())};
                }

                template <
                    typename R,
                    class Pred,
                    class Proj = RAH2_NS::details::identity,
                    RAH2_STD::enable_if_t<forward_range<R>>* = nullptr>
                constexpr RAH2_NS::ranges::borrowed_subrange_t<R>
                operator()(R&& r, Pred&& pred, Proj&& proj = {}) const
                {
                    auto first_last =
                        details::unwrap(RAH2_NS::ranges::begin(r), RAH2_NS::ranges::end(r));
                    auto result = impl(
                        RAH2_STD::move(first_last.iterator),
                        RAH2_STD::move(first_last.sentinel),
                        details::wrap_pred_proj(
                            RAH2_STD::forward<Pred>(pred), RAH2_STD::forward<Proj>(proj)));
                    return {
                        first_last.wrap_iterator(result.begin()),
                        first_last.wrap_iterator(result.end())};
                }
            };
        } // namespace niebloids
        constexpr niebloids::find_last_if_not_fn find_last_if_not;

        namespace niebloids
        {
            struct __contains_fn
            {
                template <
                    typename I,
                    typename S,
                    class T,
                    class Proj = RAH2_NS::details::identity,
                    RAH2_STD::enable_if_t<input_iterator<I> && sentinel_for<S, I>>* = nullptr>
                constexpr bool operator()(I first, S last, T const& value, Proj proj = {}) const
                {
                    return RAH2_NS::ranges::find(RAH2_STD::move(first), last, value, proj) != last;
                }

                template <
                    typename R,
                    class T,
                    class Proj = RAH2_NS::details::identity,
                    RAH2_STD::enable_if_t<input_range<R>>* = nullptr>
                constexpr bool operator()(R&& r, T const& value, Proj proj = {}) const
                {
                    return (*this)(
                        RAH2_NS::ranges::begin(r), RAH2_NS::ranges::end(r), RAH2_STD::move(value), proj);
                }
            };
        } // namespace niebloids

        constexpr niebloids::__contains_fn contains{};

        namespace niebloids
        {
            struct __contains_subrange_fn
            {
                template <
                    typename I1,
                    typename S1,
                    typename I2,
                    typename S2,
                    class Pred = RAH2_NS::ranges::equal_to,
                    class Proj1 = RAH2_NS::details::identity,
                    class Proj2 = RAH2_NS::details::identity,
                    RAH2_STD::enable_if_t<
                        forward_iterator<I1> && sentinel_for<S1, I1> && forward_iterator<I2>
                        && sentinel_for<S2, I2>>* = nullptr>
                constexpr bool operator()(
                    I1 first1,
                    S1 last1,
                    I2 first2,
                    S2 last2,
                    Pred pred = {},
                    Proj1 proj1 = {},
                    Proj2 proj2 = {}) const
                {
                    return (first2 == last2)
                           || !RAH2_NS::ranges::search(
                                   first1, last1, first2, last2, pred, proj1, proj2)
                                   .empty();
                }

                template <
                    typename R1,
                    typename R2,
                    class Pred = RAH2_NS::ranges::equal_to,
                    class Proj1 = RAH2_NS::details::identity,
                    class Proj2 = RAH2_NS::details::identity,
                    RAH2_STD::enable_if_t<forward_range<R1> && forward_range<R2>>* = nullptr>
                constexpr bool
                operator()(R1&& r1, R2&& r2, Pred pred = {}, Proj1 proj1 = {}, Proj2 proj2 = {}) const
                {
                    return (*this)(
                        RAH2_NS::ranges::begin(r1),
                        RAH2_NS::ranges::end(r1),
                        RAH2_NS::ranges::begin(r2),
                        RAH2_NS::ranges::end(r2),
                        RAH2_STD::move(pred),
                        RAH2_STD::move(proj1),
                        RAH2_STD::move(proj2));
                }
            };
        } // namespace niebloids

        constexpr niebloids::__contains_subrange_fn contains_subrange{};

        namespace niebloids
        {
            struct starts_with_fn
            {
                template <
                    typename I1, // RAH2_STD::input_iterator
                    typename S1, // RAH2_STD::sentinel_for<I1>
                    typename I2, // RAH2_STD::input_iterator
                    typename S2, // RAH2_STD::sentinel_for<I2>
                    class Pred = RAH2_NS::ranges::equal_to,
                    class Proj1 = RAH2_NS::details::identity,
                    class Proj2 = RAH2_NS::details::identity,
                    RAH2_STD::enable_if_t<
                        input_iterator<I1> && sentinel_for<S1, I1> && input_iterator<I2>
                        && sentinel_for<S2, I2>>* = nullptr>
                constexpr bool operator()(
                    I1 first1,
                    S1 last1,
                    I2 first2,
                    S2 last2,
                    Pred pred = {},
                    Proj1 proj1 = {},
                    Proj2 proj2 = {}) const
                {
                    return RAH2_NS::ranges::mismatch(
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
                    class Pred = RAH2_NS::ranges::equal_to,
                    class Proj1 = RAH2_NS::details::identity,
                    class Proj2 = RAH2_NS::details::identity,
                    RAH2_STD::enable_if_t<input_range<R1> && input_range<R2>>* = nullptr>
                // requires RAH2_STD::indirectly_comparable<ranges::iterator_t<R1>, ranges::iterator_t<R2>, Pred, Proj1, Proj2>
                constexpr bool
                operator()(R1&& r1, R2&& r2, Pred pred = {}, Proj1 proj1 = {}, Proj2 proj2 = {}) const
                {
                    return (*this)(
                        RAH2_NS::ranges::begin(r1),
                        RAH2_NS::ranges::end(r1),
                        RAH2_NS::ranges::begin(r2),
                        RAH2_NS::ranges::end(r2),
                        RAH2_STD::move(pred),
                        RAH2_STD::move(proj1),
                        RAH2_STD::move(proj2));
                }
            };
        } // namespace niebloids

        constexpr niebloids::starts_with_fn starts_with{};

        namespace niebloids
        {
            struct ends_with_fn
            {
                template <
                    typename I1, // RAH2_STD::input_iterator
                    typename S1, // RAH2_STD::sentinel_for<I1>
                    typename I2, // RAH2_STD::input_iterator
                    typename S2, // RAH2_STD::sentinel_for<I2>
                    class Pred = RAH2_NS::ranges::equal_to,
                    class Proj1 = RAH2_NS::details::identity,
                    class Proj2 = RAH2_NS::details::identity,
                    RAH2_STD::enable_if_t<
                        (RAH2_NS::forward_iterator<I1> || RAH2_NS::sized_sentinel_for<S1, I1>)
                        && (RAH2_NS::forward_iterator<I2> || RAH2_NS::sized_sentinel_for<S2, I2>)>* = nullptr>
                constexpr bool operator()(
                    I1 first1,
                    S1 last1,
                    I2 first2,
                    S2 last2,
                    Pred pred = {},
                    Proj1 proj1 = {},
                    Proj2 proj2 = {}) const
                {
                    auto const n1 = RAH2_NS::ranges::distance(first1, last1);
                    auto const n2 = RAH2_NS::ranges::distance(first2, last2);
                    if (n1 < n2)
                        return false;
                    RAH2_NS::ranges::advance(first1, n1 - n2);
                    return RAH2_NS::ranges::equal(
                        RAH2_STD::move(first1),
                        RAH2_STD::move(last1),
                        RAH2_STD::move(first2),
                        RAH2_STD::move(last2),
                        RAH2_STD::move(pred),
                        RAH2_STD::move(proj1),
                        RAH2_STD::move(proj2));
                }

                template <
                    typename R1, // ranges::input_range
                    typename R2, // ranges::input_range
                    class Pred = RAH2_NS::ranges::equal_to,
                    class Proj1 = RAH2_NS::details::identity,
                    class Proj2 = RAH2_NS::details::identity,
                    RAH2_STD::enable_if_t<
                        (RAH2_NS::ranges::forward_range<R1> || RAH2_NS::ranges::sized_range<R1>)
                        && (RAH2_NS::ranges::forward_range<R2> || RAH2_NS::ranges::sized_range<R2>)>* = nullptr>
                constexpr bool
                operator()(R1&& r1, R2&& r2, Pred pred = {}, Proj1 proj1 = {}, Proj2 proj2 = {}) const
                {
                    return (*this)(
                        RAH2_NS::ranges::begin(r1),
                        RAH2_NS::ranges::end(r1),
                        RAH2_NS::ranges::begin(r2),
                        RAH2_NS::ranges::end(r2),
                        RAH2_STD::move(pred),
                        RAH2_STD::move(proj1),
                        RAH2_STD::move(proj2));
                }
            };
        } // namespace niebloids

        constexpr niebloids::ends_with_fn ends_with{};

        template <class I1, class I2>
        using swap_ranges_result = RAH2_NS::ranges::in_in_result<I1, I2>;

        namespace niebloids
        {
            struct swap_ranges_fn
            {
                template <typename I1, typename S1, typename I2, typename S2>
                inline constexpr RAH2_NS::ranges::swap_ranges_result<I1, I2>
                impl(I1 first1, S1 last1, I2 first2, S2 last2) const
                {
                    for (; first1 != last1 and first2 != last2; ++first1, ++first2)
                    {
                        RAH2_NS::ranges::iter_swap(first1, first2);
                    }
                    return {RAH2_STD::move(first1), RAH2_STD::move(first2)};
                }

                template <
                    typename I1,
                    typename I2,
                    RAH2_STD::enable_if_t<not(
                        is_trivially_move_assignable_v<RAH2_NS::iter_value_t<I1>>
                        && is_trivially_constructible_v<RAH2_NS::iter_value_t<I1>>
                        && RAH2_NS::is_same_v<RAH2_NS::iter_value_t<I1>, RAH2_NS::iter_value_t<I2>>
                        && RAH2_NS::contiguous_iterator<I1> && RAH2_NS::contiguous_iterator<I2>)>* = nullptr>
                inline constexpr RAH2_NS::ranges::swap_ranges_result<I1, I2>
                impl(I1 first1, I2 first2, size_t size) const
                {
                    RAH2_FOR_N(size, {
                        RAH2_NS::ranges::iter_swap(first1, first2);
                        ++first1;
                        ++first2;
                    });
                    return {RAH2_STD::move(first1), RAH2_STD::move(first2)};
                }

                template <
                    typename I1,
                    typename I2,
                    RAH2_STD::enable_if_t<
                        RAH2_NS::is_trivially_move_assignable_v<RAH2_NS::iter_value_t<I1>>
                        && RAH2_NS::is_trivially_constructible_v<RAH2_NS::iter_value_t<I1>>
                        && RAH2_NS::is_same_v<RAH2_NS::iter_value_t<I1>, RAH2_NS::iter_value_t<I2>>
                        && RAH2_NS::contiguous_iterator<I1> && RAH2_NS::contiguous_iterator<I2>>* = nullptr>
                inline constexpr RAH2_NS::ranges::swap_ranges_result<I1, I2>
                impl(I1 first1, I2 first2, size_t size) const
                {
                    const auto value_size = sizeof(RAH2_NS::iter_value_t<I1>);
                    const auto buffer_size = value_size > 256 ? 1 : 256 / value_size;
                    RAH2_NS::iter_value_t<I1> buffer[buffer_size];
                    while (size >= buffer_size)
                    {
                        memcpy((void*)buffer, (void*)first1, buffer_size * value_size);
                        memcpy((void*)first1, (void*)first2, buffer_size * value_size);
                        memcpy((void*)first2, (void*)buffer, buffer_size * value_size);
                        first1 += buffer_size;
                        first2 += buffer_size;
                        size -= buffer_size;
                    }
                    memcpy((void*)buffer, (void*)first1, size * value_size);
                    memcpy((void*)first1, (void*)first2, size * value_size);
                    memcpy((void*)first2, (void*)buffer, size * value_size);
                    first1 += size;
                    first2 += size;
                    return {RAH2_STD::move(first1), RAH2_STD::move(first2)};
                }

                template <
                    typename I1,
                    typename S1,
                    typename I2,
                    typename S2,
                    RAH2_STD::enable_if_t<sized_sentinel_for<S1, I1> && sized_sentinel_for<S2, I2>>* = nullptr>
                inline constexpr RAH2_NS::ranges::swap_ranges_result<I1, I2>
                operator()(I1 first1, S1 last1, I2 first2, S2 last2) const
                {
                    auto first_last = details::unwrap(RAH2_STD::move(first1), RAH2_STD::move(last1));
                    auto first2_last2 =
                        details::unwrap(RAH2_STD::move(first2), RAH2_STD::move(last2));
                    auto result = impl(
                        RAH2_STD::move(first_last.iterator),
                        RAH2_STD::move(first2_last2.iterator),
                        RAH2_STD::min(
                            RAH2_NS::ranges::distance(first1, last1),
                            RAH2_NS::ranges::distance(first2, last2)));
                    return {
                        first_last.wrap_iterator(RAH2_STD::move(result.in1)),
                        first2_last2.wrap_iterator(RAH2_STD::move(result.in2))};
                }

                template <
                    typename I1,
                    typename S1,
                    typename I2,
                    typename S2,
                    RAH2_STD::enable_if_t<not(sized_sentinel_for<S1, I1> && sized_sentinel_for<S2, I2>)>* = nullptr>
                inline constexpr RAH2_NS::ranges::swap_ranges_result<I1, I2>
                operator()(I1 first1, S1 last1, I2 first2, S2 last2) const
                {
                    auto first_last = details::unwrap(RAH2_STD::move(first1), RAH2_STD::move(last1));
                    auto first2_last2 =
                        details::unwrap(RAH2_STD::move(first2), RAH2_STD::move(last2));
                    auto result = impl(
                        RAH2_STD::move(first_last.iterator),
                        RAH2_STD::move(first_last.sentinel),
                        RAH2_STD::move(first2_last2.iterator),
                        RAH2_STD::move(first2_last2.sentinel));
                    return {
                        first_last.wrap_iterator(RAH2_STD::move(result.in1)),
                        first2_last2.wrap_iterator(RAH2_STD::move(result.in2))};
                }

                template <
                    typename R1,
                    typename R2,
                    RAH2_STD::enable_if_t<not(
                        RAH2_NS::ranges::sized_range<R1> && RAH2_NS::ranges::sized_range<R2>)>* = nullptr>
                inline constexpr RAH2_NS::ranges::swap_ranges_result<
                    RAH2_NS::ranges::borrowed_iterator_t<R1>,
                    RAH2_NS::ranges::borrowed_iterator_t<R2>>
                operator()(R1&& r1, R2&& r2) const
                {
                    return (*this)(
                        RAH2_NS::ranges::begin(r1),
                        RAH2_NS::ranges::end(r1),
                        RAH2_NS::ranges::begin(r2),
                        RAH2_NS::ranges::end(r2));
                }

                template <
                    typename R1,
                    typename R2,
                    RAH2_STD::enable_if_t<
                        RAH2_NS::ranges::sized_range<R1> && RAH2_NS::ranges::sized_range<R2>>* = nullptr>
                inline constexpr RAH2_NS::ranges::swap_ranges_result<
                    RAH2_NS::ranges::borrowed_iterator_t<R1>,
                    RAH2_NS::ranges::borrowed_iterator_t<R2>>
                operator()(R1&& r1, R2&& r2) const
                {
                    auto first_last =
                        details::unwrap(RAH2_NS::ranges::begin(r1), RAH2_NS::ranges::end(r1));
                    auto first2_last2 =
                        details::unwrap(RAH2_NS::ranges::begin(r2), RAH2_NS::ranges::end(r2));
                    auto out1_out2 = impl(
                        RAH2_STD::move(first_last.iterator),
                        RAH2_STD::move(first2_last2.iterator),
                        RAH2_STD::min(RAH2_NS::ranges::size(r1), RAH2_NS::ranges::size(r2)));
                    return {
                        first_last.wrap_iterator(RAH2_STD::move(out1_out2.in1)),
                        first2_last2.wrap_iterator(RAH2_STD::move(out1_out2.in2))};
                }
            };
        } // namespace niebloids

        constexpr niebloids::swap_ranges_fn swap_ranges{};

        namespace niebloids
        {
            struct shift_left_fn
            {
                template <typename I, typename S, RAH2_STD::enable_if_t<permutable<I> && sentinel_for<S, I>>* = nullptr>
                constexpr RAH2_NS::ranges::subrange<I> impl(I first, S last, iter_difference_t<I> n) const
                {
                    if (n <= 0)
                    {
                        auto last2 = RAH2_NS::ranges::next(first, last);
                        return {RAH2_STD::move(first), RAH2_STD::move(last2)};
                    }
                    auto mid = first;
                    auto diff = RAH2_NS::ranges::advance(first, n, last);
                    if (diff != 0) // return [first, first] if n > size(range)
                    {
                        return {mid, mid};
                    }
                    auto result = RAH2_NS::ranges::move(first, last, mid);
                    return {RAH2_STD::move(mid), RAH2_STD::move(result.out)};
                }

                template <typename I, typename S, RAH2_STD::enable_if_t<permutable<I> && sentinel_for<S, I>>* = nullptr>
                constexpr RAH2_NS::ranges::subrange<I>
                operator()(I first, S last, iter_difference_t<I> n) const
                {
                    auto first_last = details::unwrap(RAH2_STD::move(first), RAH2_STD::move(last));
                    auto ret = impl(first_last.iterator, first_last.sentinel, n);
                    return {
                        first_last.wrap_iterator(ret.begin()), first_last.wrap_iterator(ret.end())};
                }

                template <
                    typename R,
                    RAH2_STD::enable_if_t<forward_range<R> && permutable<iterator_t<R>>>* = nullptr>
                constexpr borrowed_subrange_t<R> operator()(R&& r, range_difference_t<R> n) const
                {
                    return (*this)(RAH2_NS::ranges::begin(r), RAH2_NS::ranges::end(r), n);
                }
            };
        } // namespace niebloids

        constexpr niebloids::shift_left_fn shift_left{};

        namespace niebloids
        {
            struct shift_right_fn
            {
                template <
                    typename I,
                    typename S,
                    RAH2_STD::enable_if_t<
                        permutable<I> && sentinel_for<S, I> && bidirectional_iterator<S>>* = nullptr>
                static constexpr RAH2_NS::ranges::subrange<I>
                impl(I first, S last, iter_difference_t<I> n)
                {
                    if (n <= 0)
                    {
                        return {first, last};
                    }
                    auto mid = last;
                    if (RAH2_NS::ranges::advance(mid, -n, first) != 0)
                    {
                        return {last, last};
                    }
                    auto result = RAH2_NS::ranges::move_backward(
                        RAH2_STD::move(first), RAH2_STD::move(mid), last);
                    return {RAH2_STD::move(result.out), RAH2_STD::move(last)};
                }

                template <
                    typename I,
                    typename S,
                    RAH2_STD::enable_if_t<
                        permutable<I> && sentinel_for<S, I> && bidirectional_iterator<I>
                        && RAH2_NS::assignable_from<I&, S>>* = nullptr>
                constexpr RAH2_NS::ranges::subrange<I>
                operator()(I first, S last, iter_difference_t<I> n) const
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
                        && !(bidirectional_iterator<I> && assignable_from<I&, S>)
                        && random_access_iterator<I>>* = nullptr>
                constexpr RAH2_NS::ranges::subrange<I>
                operator()(I first, S last, iter_difference_t<I> n) const
                {
                    auto last2 = first + (last - first);
                    return shift_right_fn::impl(first, last2, n);
                }
                template <
                    typename I,
                    typename S,
                    RAH2_STD::enable_if_t<
                        permutable<I> && sentinel_for<S, I>
                        && !(bidirectional_iterator<I> && assignable_from<I&, S>)
                        && !(sized_sentinel_for<S, I> && random_access_iterator<I>)>* = nullptr>
                constexpr RAH2_NS::ranges::subrange<I>
                operator()(I first, S last, iter_difference_t<I> n) const
                {
                    if (n <= 0)
                    {
                        auto last2 = RAH2_NS::ranges::next(first, last);
                        return {first, last2};
                    }
                    auto result = first;
                    if (RAH2_NS::ranges::advance(result, n, last) != 0)
                    {
                        auto last2 = RAH2_NS::ranges::next(first, last);
                        return {last2, last2};
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
                            auto move_in_out = RAH2_NS::ranges::move(
                                RAH2_STD::move(first), RAH2_STD::move(trail), RAH2_STD::move(result));
                            return {result, move_in_out.out};
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
                                auto move_in_out = RAH2_NS::ranges::move(
                                    RAH2_STD::move(first), RAH2_STD::move(mid), RAH2_STD::move(trail));
                                return {result, move_in_out.out};
                            }
                            RAH2_NS::ranges::iter_swap(mid, trail);
                        }
                    }
                }

                template <
                    typename R,
                    RAH2_STD::enable_if_t<forward_range<R> && permutable<iterator_t<R>>>* = nullptr>
                constexpr borrowed_subrange_t<R> operator()(R&& r, range_difference_t<R> n) const
                {
                    return (*this)(RAH2_NS::ranges::begin(r), RAH2_NS::ranges::end(r), n);
                }
            };
        } // namespace niebloids

        constexpr niebloids::shift_right_fn shift_right{};

        namespace niebloids
        {
            struct sample_fn
            {
                template <typename DistibType, typename I, typename S, typename O, class Gen>
                inline constexpr O sample_sized_impl(
                    I first,
                    S last,
                    RAH2_NS::iter_difference_t<I> input_size,
                    O out,
                    RAH2_NS::iter_difference_t<I> n,
                    Gen&& gen) const
                {
                    if (n == 0 or input_size == 0)
                    {
                        return out;
                    }

                    using uniform_int_distrib = RAH2_STD::uniform_int_distribution<DistibType>;
                    auto first_last = details::unwrap(RAH2_STD::move(first), RAH2_STD::move(last));
                    auto first2 = first_last.iterator;
                    auto out_iter = details::unwrap_begin(RAH2_STD::move(out));
                    auto out2 = out_iter.iterator;

                    n = std::min(n, input_size);
                    for (; input_size > 0; ++first2, (void)--input_size)
                    {
                        uniform_int_distrib distrib(
                            DistibType(), static_cast<DistibType>(input_size - 1));
                        if (distrib(gen) < DistibType(n))
                        {
                            --n;
                            *out2 = *first2;
                            ++out2;
                        }
                    }

                    return out_iter.wrap_iterator(RAH2_STD::move(out2));
                }

                template <typename DistibType, typename I, typename S, typename O, class Gen>
                inline constexpr O sample_unsized_impl(
                    I first, S last, O out, RAH2_NS::iter_difference_t<I> n, Gen&& gen) const
                {
                    if (n == 0 or first == last)
                    {
                        return out;
                    }

                    using uniform_int_distrib = RAH2_STD::uniform_int_distribution<DistibType>;
                    auto first_last = details::unwrap(RAH2_STD::move(first), RAH2_STD::move(last));
                    auto first2 = first_last.iterator;
                    auto last2 = first_last.sentinel;
                    auto out_iter = details::unwrap_begin(RAH2_STD::move(out));
                    auto out2 = out_iter.iterator;

                    DistibType sample_size{};
                    // copy [first, first + M) elements to "random access" output
                    for (; first2 != last2 && sample_size != DistibType(n); ++first2, ++sample_size)
                    {
                        out2[sample_size] = *first2;
                    }
                    // overwrite some of the copied elements with randomly selected ones
                    for (auto pop_size{sample_size}; first2 != last2; ++first2, ++pop_size)
                    {
                        uniform_int_distrib distrib(DistibType(), static_cast<DistibType>(pop_size));
                        auto const i = distrib(gen);
                        if (i < DistibType(n))
                            out2[i] = *first2;
                    }
                    return out + sample_size;
                }

                template <
                    typename I,
                    typename S,
                    typename O,
                    class Gen,
                    RAH2_STD::enable_if_t<!(
                        RAH2_NS::forward_iterator<I> || RAH2_NS::sized_sentinel_for<S, I>)>* = nullptr>
                inline constexpr O
                operator()(I first, S last, O out, RAH2_NS::iter_difference_t<I> n, Gen&& gen) const
                {
                    if (n >= RAH2_STD::numeric_limits<uint32_t>::max())
                    {
                        return this->sample_unsized_impl<uint64_t>(
                            RAH2_MOV(first), RAH2_MOV(last), RAH2_MOV(out), n, RAH2_MOV(gen));
                    }
                    else
                    {
                        return this->sample_unsized_impl<uint32_t>(
                            RAH2_MOV(first), RAH2_MOV(last), RAH2_MOV(out), n, RAH2_MOV(gen));
                    }
                }

                template <
                    typename I,
                    typename S,
                    typename O,
                    class Gen,
                    RAH2_STD::enable_if_t<
                        RAH2_NS::forward_iterator<I> || RAH2_NS::sized_sentinel_for<S, I>>* = nullptr>
                inline constexpr O
                operator()(I first, S last, O out, RAH2_NS::iter_difference_t<I> n, Gen&& gen) const
                {
                    auto input_size = RAH2_NS::ranges::distance(first, last);
                    return this->sample_sized_impl<uint32_t>(
                        RAH2_MOV(first), RAH2_MOV(last), input_size, RAH2_MOV(out), n, RAH2_MOV(gen));
                }

                template <
                    typename R,
                    typename O,
                    class Gen,
                    RAH2_STD::enable_if_t<RAH2_NS::ranges::sized_range<R>>* = nullptr>
                inline constexpr O operator()(R&& r, O out, range_difference_t<R> n, Gen&& gen) const
                {
                    auto input_size = RAH2_NS::ranges::size(r);
                    return this->sample_sized_impl<uint32_t>(
                        RAH2_NS::ranges::begin(r),
                        RAH2_NS::ranges::end(r),
                        input_size,
                        RAH2_MOV(out),
                        n,
                        RAH2_MOV(gen));
                }

                template <
                    typename R,
                    typename O,
                    class Gen,
                    RAH2_STD::enable_if_t<!RAH2_NS::ranges::sized_range<R>>* = nullptr>
                inline constexpr O operator()(R&& r, O out, range_difference_t<R> n, Gen&& gen) const
                {
                    return (*this)(
                        RAH2_NS::ranges::begin(r),
                        RAH2_NS::ranges::end(r),
                        RAH2_MOV(out),
                        n,
                        RAH2_MOV(gen));
                }
            };
        } // namespace niebloids

        constexpr niebloids::sample_fn sample{};

        template <class I, class O>
        using unique_copy_result = RAH2_NS::ranges::in_out_result<I, O>;

        namespace niebloids
        {
            struct unique_copy_fn
            {
                template <
                    typename I, // RAH2_STD::input_iterator
                    typename S, // RAH2_STD::sentinel_for<I>
                    typename O, // RAH2_STD::weakly_incrementable
                    typename C = RAH2_NS::ranges::equal_to,
                    typename Proj = RAH2_NS::details::identity,
                    RAH2_STD::enable_if_t<input_iterator<I> && sentinel_for<S, I>>* = nullptr>
                constexpr RAH2_NS::ranges::unique_copy_result<I, O>
                operator()(I first, S last, O result, C comp = {}, Proj proj = {}) const
                {
                    auto pred_proj =
                        details::wrap_pred_proj(RAH2_STD::move(comp), RAH2_STD::move(proj));

                    if (!(first == last))
                    {
                        RAH2_NS::iter_value_t<I> value = *first;
                        *result = value;
                        ++result;
                        while (!(++first == last))
                        {
                            auto&& value2 = *first;
                            if (!RAH2_INVOKE_2(pred_proj, value2, value))
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
                    typename C = RAH2_NS::ranges::equal_to,
                    typename Proj = RAH2_NS::details::identity,
                    RAH2_STD::enable_if_t<input_range<R>>* = nullptr>
                constexpr RAH2_NS::ranges::unique_copy_result<RAH2_NS::ranges::borrowed_iterator_t<R>, O>
                operator()(R&& r, O result, C comp = {}, Proj proj = {}) const
                {
                    return (*this)(
                        RAH2_NS::ranges::begin(r),
                        RAH2_NS::ranges::end(r),
                        RAH2_STD::move(result),
                        RAH2_STD::move(comp),
                        RAH2_STD::move(proj));
                }
            };
        } // namespace niebloids

        constexpr niebloids::unique_copy_fn unique_copy{};

        namespace niebloids
        {
            struct is_partitioned_fn
            {
                template <
                    typename I, // RAH2_STD::input_iterator
                    typename S, // RAH2_STD::sentinel_for<I
                    typename Pred, // RAH2_STD::indirect_unary_predicate<RAH2_STD::projected<I, Proj>>
                    typename Proj = RAH2_NS::details::identity,
                    RAH2_STD::enable_if_t<sentinel_for<S, I>>* = nullptr>
                constexpr bool operator()(I first, S last, Pred pred, Proj proj = {}) const
                {
                    auto pred_proj =
                        details::wrap_pred_proj(RAH2_STD::move(pred), RAH2_STD::move(proj));

                    for (; first != last; ++first)
                        if (!pred_proj(*first))
                            break;

                    for (; first != last; ++first)
                        if (pred_proj(*first))
                            return false;

                    return true;
                }

                template <
                    typename R, // ranges::input_range
                    typename Pred, // RAH2_STD::indirect_unary_predicate<RAH2_STD::projected<ranges::iterator_t<R>, Proj>>
                    typename Proj = RAH2_NS::details::identity,
                    RAH2_STD::enable_if_t<RAH2_NS::ranges::range<R>>* = nullptr>
                constexpr bool operator()(R&& r, Pred pred, Proj proj = {}) const
                {
                    return (*this)(
                        RAH2_NS::ranges::begin(r),
                        RAH2_NS::ranges::end(r),
                        RAH2_STD::move(pred),
                        RAH2_STD::move(proj));
                }
            };
        } // namespace niebloids

        constexpr auto is_partitioned = niebloids::is_partitioned_fn();

        template <class I, class O1, class O2>
        using partition_copy_result = in_out_out_result<I, O1, O2>;

        namespace niebloids
        {
            struct partition_copy_fn
            {
                template <
                    typename I, // RAH2_STD::input_iterator
                    typename S, // RAH2_STD::sentinel_for<I>
                    typename O1, // RAH2_STD::weakly_incrementable
                    typename O2, // RAH2_STD::weakly_incrementable
                    typename Proj = RAH2_NS::details::identity,
                    typename Pred, // RAH2_STD::indirect_unary_predicate<RAH2_STD::projected<I, Proj>>
                    RAH2_STD::enable_if_t<sentinel_for<S, I>>* = nullptr>
                // requires RAH2_STD::indirectly_copyable<I, O1> && RAH2_STD::indirectly_copyable<I, O2>
                constexpr RAH2_NS::ranges::partition_copy_result<I, O1, O2>
                operator()(I first, S last, O1 out_true, O2 out_false, Pred pred, Proj proj = {}) const
                {
                    auto pred_proj =
                        details::wrap_pred_proj(RAH2_STD::move(pred), RAH2_STD::move(proj));

                    for (; first != last; ++first)
                    {
                        if (RAH2_INVOKE_1(pred_proj, *first))
                        {
                            *out_true = *first, ++out_true;
                        }
                        else
                        {
                            *out_false = *first, ++out_false;
                        }
                    }
                    return {
                        RAH2_STD::move(first), RAH2_STD::move(out_true), RAH2_STD::move(out_false)};
                }

                template <
                    typename R, // ranges::input_range
                    typename O1, /// RAH2_STD::weakly_incrementable
                    typename O2, // RAH2_STD::weakly_incrementable
                    typename Proj = RAH2_NS::details::identity,
                    typename Pred, // RAH2_STD::indirect_unary_predicate<RAH2_STD::projected<iterator_t<R>, Proj>>
                    RAH2_STD::enable_if_t<RAH2_NS::ranges::range<R>>* = nullptr>
                //requires RAH2_STD::indirectly_copyable<ranges::iterator_t<R>, O1>
                //         && RAH2_STD::indirectly_copyable<ranges::iterator_t<R>, O2>
                constexpr RAH2_NS::ranges::
                    partition_copy_result<RAH2_NS::ranges::borrowed_iterator_t<R>, O1, O2>
                    operator()(R&& r, O1 out_true, O2 out_false, Pred pred, Proj proj = {}) const
                {
                    return (*this)(
                        RAH2_NS::ranges::begin(r),
                        RAH2_NS::ranges::end(r),
                        RAH2_STD::move(out_true),
                        RAH2_STD::move(out_false),
                        RAH2_STD::move(pred),
                        RAH2_STD::move(proj));
                }
            };
        } // namespace niebloids

        constexpr niebloids::partition_copy_fn partition_copy{};

        namespace niebloids
        {
            struct partition_point_fn
            {
                template <
                    typename ForwardIt,
                    typename Sentinel,
                    typename UnaryPredicate,
                    typename Proj = RAH2_NS::details::identity,
                    RAH2_STD::enable_if_t<sentinel_for<Sentinel, ForwardIt>>* = nullptr>
                constexpr ForwardIt
                operator()(ForwardIt first, Sentinel last, UnaryPredicate pred, Proj proj = {}) const
                {
                    auto pred_proj =
                        details::wrap_pred_proj(RAH2_STD::move(pred), RAH2_STD::move(proj));

                    for (auto length = RAH2_NS::ranges::distance(first, last); 0 < length;)
                    {
                        auto half = length / 2;
                        auto middle = RAH2_STD::next(first, half);
                        if (pred_proj(*middle))
                        {
                            first = RAH2_STD::next(middle);
                            length -= (half + 1);
                        }
                        else
                            length = half;
                    }

                    return first;
                }

                template <
                    typename ForwardRange,
                    typename UnaryPredicate,
                    typename Proj = RAH2_NS::details::identity,
                    RAH2_STD::enable_if_t<RAH2_NS::ranges::range<ForwardRange>>* = nullptr>
                constexpr borrowed_iterator_t<ForwardRange>
                operator()(ForwardRange&& range, UnaryPredicate pred, Proj proj = {}) const
                {
                    return (*this)(
                        RAH2_NS::ranges::begin(range),
                        RAH2_NS::ranges::end(range),
                        RAH2_STD::move(pred),
                        RAH2_STD::move(proj));
                }
            };
        } // namespace niebloids

        constexpr niebloids::partition_point_fn partition_point{};

        template <class I, class O>
        using partial_sort_copy_result = RAH2_NS::ranges::in_out_result<I, O>;
        namespace niebloids
        {
            struct partial_sort_copy_fn
            {
                template <
                    typename I1, // RAH2_STD::input_iterator
                    typename S1, // RAH2_STD::sentinel_for<I1>
                    typename I2, // RAH2_STD::random_access_iterator
                    typename S2, // RAH2_STD::sentinel_for<I2>
                    class Comp = RAH2_NS::ranges::less,
                    typename Proj1 = RAH2_NS::details::identity,
                    typename Proj2 = RAH2_NS::details::identity,
                    RAH2_STD::enable_if_t<RAH2_NS::sentinel_for<S1, I1> && RAH2_NS::sentinel_for<S2, I2>>* = nullptr>
                constexpr RAH2_NS::ranges::partial_sort_copy_result<I1, I2> operator()(
                    I1 first,
                    S1 last,
                    I2 result_first,
                    S2 result_last,
                    Comp comp = {},
                    Proj1 proj1 = {},
                    Proj2 proj2 = {}) const
                {
                    auto pred_out =
                        details::wrap_pred_proj(RAH2_STD::move(comp), RAH2_STD::move(proj2));
                    auto pred_in_out = details::wrap_pred_proj(
                        RAH2_STD::move(comp), RAH2_STD::move(proj1), RAH2_STD::move(proj2));
                    auto p1 = details::wrap_unary(RAH2_FWD(proj1));
                    auto p2 = details::wrap_unary(RAH2_FWD(proj2));

                    if (result_first == result_last)
                        return {
                            RAH2_NS::ranges::next(RAH2_STD::move(first), RAH2_STD::move(last)),
                            RAH2_STD::move(result_first)};

                    auto out_last{result_first};
                    // copy first N elements
                    for (; !(first == last or out_last == result_last); ++out_last, ++first)
                        p2(*out_last) = p1(*first);

                    // convert N copied elements into a max-heap
                    RAH2_NS::ranges::make_heap(result_first, out_last, pred_out);

                    // process the rest of the input range (if any), preserving the heap property
                    for (; first != last; ++first)
                    {
                        if (RAH2_INVOKE_2(pred_in_out, *first, *result_first))
                        {
                            // pop out the biggest item and push in a newly found smaller one
                            RAH2_NS::ranges::pop_heap(result_first, out_last, pred_out);
                            p2(*(out_last - 1)) = p1(*first);
                            RAH2_NS::ranges::push_heap(result_first, out_last, pred_out);
                        }
                    }

                    // first N elements in the output range is still
                    // a heap - convert it into a sorted range
                    RAH2_NS::ranges::sort_heap(result_first, out_last, pred_out);

                    return {RAH2_STD::move(first), RAH2_STD::move(out_last)};
                }

                template <
                    typename R1, // ranges::input_range
                    typename R2, // ranges::random_access_range
                    class Comp = RAH2_NS::ranges::less,
                    typename Proj1 = RAH2_NS::details::identity,
                    typename Proj2 = RAH2_NS::details::identity,
                    RAH2_STD::enable_if_t<
                        RAH2_NS::ranges::input_range<R1> && RAH2_NS::ranges::random_access_range<R2>>* = nullptr>
                constexpr RAH2_NS::ranges::partial_sort_copy_result<
                    RAH2_NS::ranges::borrowed_iterator_t<R1>,
                    RAH2_NS::ranges::borrowed_iterator_t<R2>>
                operator()(R1&& r, R2&& result_r, Comp comp = {}, Proj1 proj1 = {}, Proj2 proj2 = {}) const
                {
                    return (*this)(
                        RAH2_NS::ranges::begin(r),
                        RAH2_NS::ranges::end(r),
                        RAH2_NS::ranges::begin(result_r),
                        RAH2_NS::ranges::end(result_r),
                        RAH2_STD::move(comp),
                        RAH2_STD::move(proj1),
                        RAH2_STD::move(proj2));
                }
            };
        } // namespace niebloids
        constexpr niebloids::partial_sort_copy_fn partial_sort_copy{};

        namespace niebloids
        {
            // TODO : This is the slower implementation. Make a better one.
            struct inplace_merge_fn
            {
                template <
                    typename I, // RAH2_STD::bidirectional_iterator
                    typename S, // RAH2_STD::sentinel_for<I>
                    class Comp = RAH2_NS::ranges::less,
                    typename Proj = RAH2_NS::details::identity,
                    RAH2_STD::enable_if_t<bidirectional_iterator<I> && sentinel_for<S, I>>* = nullptr>
                // requires RAH2_STD::sortable<I, Comp, Proj>
                constexpr I operator()(I first, I middle, S last, Comp comp = {}, Proj proj = {}) const
                {
                    auto pred_proj =
                        details::wrap_pred_proj(RAH2_STD::move(comp), RAH2_STD::move(proj));

                    I last_it = RAH2_NS::ranges::next(middle, last);
                    inplace_merge_slow(
                        first,
                        middle,
                        last_it,
                        RAH2_NS::ranges::distance(first, middle),
                        RAH2_NS::ranges::distance(middle, last_it),
                        RAH2_STD::ref(pred_proj));
                    return last_it;
                }

                template <
                    typename R, // ranges::bidirectional_range
                    class Comp = RAH2_NS::ranges::less,
                    typename Proj = RAH2_NS::details::identity,
                    RAH2_STD::enable_if_t<bidirectional_range<R>>* = nullptr>
                // requires RAH2_STD::sortable<ranges::iterator_t<R>, Comp, Proj>
                constexpr RAH2_NS::ranges::borrowed_iterator_t<R> operator()(
                    R&& r, RAH2_NS::ranges::iterator_t<R> middle, Comp comp = {}, Proj proj = {}) const
                {
                    return (*this)(
                        RAH2_NS::ranges::begin(r),
                        RAH2_STD::move(middle),
                        RAH2_NS::ranges::end(r),
                        RAH2_STD::move(comp),
                        RAH2_STD::move(proj));
                }

            private:
                template <class I, class Comp>
                static constexpr void inplace_merge_slow(
                    I first,
                    I middle,
                    I last,
                    RAH2_NS::iter_difference_t<I> n1,
                    RAH2_NS::iter_difference_t<I> n2,
                    Comp comp)
                {
                    if (n1 == 0 || n2 == 0)
                        return;
                    if (n1 + n2 == 2 && comp(*middle, *first))
                    {
                        RAH2_NS::ranges::iter_swap(first, middle);
                        return;
                    }

                    I cut1 = first, cut2 = middle;
                    RAH2_NS::iter_difference_t<I> d1{}, d2{};

                    if (n1 > n2)
                    {
                        d1 = n1 / 2;
                        RAH2_NS::ranges::advance(cut1, d1);
                        cut2 = RAH2_NS::ranges::lower_bound(middle, last, *cut1, RAH2_STD::ref(comp));
                        d2 = RAH2_NS::ranges::distance(middle, cut2);
                    }
                    else
                    {
                        d2 = n2 / 2;
                        RAH2_NS::ranges::advance(cut2, d2);
                        cut1 =
                            RAH2_NS::ranges::upper_bound(first, middle, *cut2, RAH2_STD::ref(comp));
                        d1 = RAH2_NS::ranges::distance(first, cut1);
                    }

                    I new_middle = RAH2_NS::ranges::rotate(cut1, middle, cut2).begin();
                    inplace_merge_slow(first, cut1, new_middle, d1, d2, RAH2_STD::ref(comp));
                    inplace_merge_slow(new_middle, cut2, last, n1 - d1, n2 - d2, RAH2_STD::ref(comp));
                }
            };
        } // namespace niebloids
        constexpr niebloids::inplace_merge_fn inplace_merge{};
        namespace niebloids
        {
            struct includes_fn
            {
                template <
                    typename I1, // RAH2_STD::input_iterator
                    typename S1, // RAH2_STD::sentinel_for<I1>
                    typename I2, // RAH2_STD::input_iterator
                    typename S2, // RAH2_STD::sentinel_for<I2>
                    typename Proj1 = RAH2_NS::details::identity,
                    typename Proj2 = RAH2_NS::details::identity,
                    typename Comp = RAH2_NS::ranges::less, // RAH2_STD::indirect_strict_weak_order<RAH2_STD::projected<I1, Proj1>, RAH2_STD::projected<I2, Proj2>>
                    RAH2_STD::enable_if_t<
                        RAH2_NS::input_iterator<I1> && RAH2_NS::sentinel_for<S1, I1>
                        && RAH2_NS::input_iterator<I2> && RAH2_NS::sentinel_for<S2, I2>>* = nullptr>
                constexpr bool operator()(
                    I1 first1,
                    S1 last1,
                    I2 first2,
                    S2 last2,
                    Comp comp = {},
                    Proj1 proj1 = {},
                    Proj2 proj2 = {}) const
                {
                    auto pred_proj_1_2 = details::wrap_pred_proj(RAH2_STD::move(comp), proj1, proj2);
                    auto pred_proj_2_1 = details::wrap_pred_proj(
                        RAH2_STD::move(comp), RAH2_STD::move(proj2), RAH2_STD::move(proj1));

                    for (; first2 != last2; ++first1)
                    {
                        if (first1 == last1 || pred_proj_2_1(*first2, *first1))
                            return false;
                        if (!pred_proj_1_2(*first1, *first2))
                            ++first2;
                    }
                    return true;
                }

                template <
                    typename R1, // ranges::input_range
                    typename R2, // ranges::input_range
                    typename Comp = RAH2_NS::ranges::less, // RAH2_STD::indirect_strict_weak_order<
                    typename Proj1 = RAH2_NS::details::identity,
                    typename Proj2 = RAH2_NS::details::identity,
                    RAH2_STD::enable_if_t<
                        RAH2_NS::ranges::input_range<R1> && RAH2_NS::ranges::input_range<R2>>* = nullptr>
                constexpr bool
                operator()(R1&& r1, R2&& r2, Comp comp = {}, Proj1 proj1 = {}, Proj2 proj2 = {}) const
                {
                    return (*this)(
                        RAH2_NS::ranges::begin(r1),
                        RAH2_NS::ranges::end(r1),
                        RAH2_NS::ranges::begin(r2),
                        RAH2_NS::ranges::end(r2),
                        RAH2_STD::move(comp),
                        RAH2_STD::move(proj1),
                        RAH2_STD::move(proj2));
                }
            };
        } // namespace niebloids
        constexpr auto includes = niebloids::includes_fn{};
        namespace niebloids
        {
            struct max_fn
            {
                template <
                    class T,
                    typename Comp = RAH2_NS::ranges::less // RAH2_STD::indirect_strict_weak_order<RAH2_STD::projected<const T*, Proj>>
                    >
                constexpr T const& operator()(T const& a, T const& b, Comp comp = {}) const
                {
                    return RAH2_INVOKE_2(comp, a, b) ? b : a;
                }

                template <
                    typename T, // RAH2_STD::copyable
                    typename Comp = RAH2_NS::ranges::less // RAH2_STD::indirect_strict_weak_order<RAH2_STD::projected<const T*, Proj>>
                    >
                constexpr T operator()(std::initializer_list<T> r, Comp comp = {}) const
                {
                    return *RAH2_NS::ranges::max_element(r, RAH2_STD::ref(comp));
                }

                template <
                    typename R,
                    typename Comp = RAH2_NS::ranges::less,
                    typename Proj = RAH2_NS::details::identity,
                    RAH2_STD::enable_if_t<forward_range<R>>* = nullptr>
                constexpr range_value_t<R> operator()(R&& r, Comp&& comp = {}, Proj&& proj = {}) const
                {
                    using V = range_value_t<R>;
                    return static_cast<V>(
                        *RAH2_NS::ranges::max_element(r, RAH2_FWD(comp), RAH2_FWD(proj)));
                }
                template <
                    typename R,
                    typename Comp = RAH2_NS::ranges::less,
                    typename Proj = RAH2_NS::details::identity,
                    RAH2_STD::enable_if_t<!forward_range<R>>* = nullptr>
                constexpr range_value_t<R> operator()(R&& r, Comp&& comp = {}, Proj&& proj = {}) const
                {
                    auto pred_proj = details::wrap_pred_proj(RAH2_FWD(comp), RAH2_FWD(proj));
                    auto i = RAH2_NS::ranges::begin(r);
                    auto s = RAH2_NS::ranges::end(r);
                    decltype(auto) m(*i);
                    while (++i != s)
                    {
                        decltype(auto) val(*i);
                        if (RAH2_INVOKE_2(pred_proj, m, val))
                            m = val;
                    }
                    return m;
                }
            };
        } // namespace niebloids
        constexpr niebloids::max_fn max;
        namespace niebloids
        {
            struct min_fn
            {
                template <class T, typename Comp = RAH2_NS::ranges::less, typename Proj = RAH2_NS::details::identity>
                constexpr T const&
                operator()(T const& a, T const& b, Comp&& comp = {}, Proj&& proj = {}) const
                {
                    return RAH2_INVOKE_2(comp, proj(b), proj(a)) ? b : a;
                }

                template <typename T, typename Comp = RAH2_NS::ranges::less, typename Proj = RAH2_NS::details::identity>
                constexpr T operator()(std::initializer_list<T> r, Comp&& comp = {}, Proj&& proj = {}) const
                {
                    return *RAH2_NS::ranges::min_element(r, RAH2_FWD(comp), RAH2_FWD(proj));
                }

                template <
                    typename R,
                    typename Comp = RAH2_NS::ranges::less,
                    typename Proj = RAH2_NS::details::identity,
                    RAH2_STD::enable_if_t<forward_range<R>>* = nullptr>
                constexpr range_value_t<R> operator()(R&& r, Comp comp = {}, Proj&& proj = {}) const
                {
                    using V = range_value_t<R>;
                    return static_cast<V>(
                        *RAH2_NS::ranges::min_element(r, RAH2_FWD(comp), RAH2_FWD(proj)));
                }
                template <
                    typename R,
                    typename Comp = RAH2_NS::ranges::less,
                    typename Proj = RAH2_NS::details::identity,
                    RAH2_STD::enable_if_t<!forward_range<R>>* = nullptr>
                constexpr range_value_t<R> operator()(R&& r, Comp&& comp = {}, Proj&& proj = {}) const
                {
                    auto pred_proj = details::wrap_pred_proj(RAH2_FWD(comp), RAH2_FWD(proj));
                    using V = range_value_t<R>;
                    auto i = RAH2_NS::ranges::begin(r);
                    auto s = RAH2_NS::ranges::end(r);
                    V m(*i);
                    ++i;
                    for (; i != s; ++i)
                    {
                        V m2(*i);
                        if (RAH2_INVOKE_2(pred_proj, m2, m))
                            m = m2;
                    }
                    return m;
                }
            };
        } // namespace niebloids
        constexpr niebloids::min_fn min;

        template <class T>
        using minmax_result = RAH2_NS::ranges::min_max_result<T>;
        namespace niebloids
        {
            struct minmax_fn
            {
                template <class T, typename Comp = RAH2_NS::ranges::less, typename Proj = RAH2_NS::details::identity>
                constexpr RAH2_NS::ranges::minmax_result<T const&>
                operator()(T const& a, T const& b, Comp const& comp = {}, Proj const& proj = {}) const
                {
                    if (RAH2_INVOKE_2(comp, proj(b), proj(a)))
                    {
                        return {b, a};
                    }
                    else
                    {
                        return {a, b};
                    }
                }

                template <typename T, typename Comp = RAH2_NS::ranges::less, typename Proj = RAH2_NS::details::identity>
                constexpr RAH2_NS::ranges::minmax_result<T>
                operator()(std::initializer_list<T> r, Comp&& comp = {}, Proj&& proj = {}) const
                {
                    auto result = RAH2_NS::ranges::minmax_element(r, RAH2_FWD(comp), RAH2_FWD(proj));
                    return {*result.min, *result.max};
                }

                template <
                    typename R,
                    typename Comp = RAH2_NS::ranges::less,
                    typename Proj = RAH2_NS::details::identity,
                    RAH2_STD::enable_if_t<input_range<R>>* = nullptr>
                constexpr RAH2_NS::ranges::minmax_result<range_value_t<R>>
                operator()(R&& r, Comp&& comp = {}, Proj&& proj = {}) const
                {
                    auto pred_proj = details::wrap_pred_proj(RAH2_FWD(comp), RAH2_FWD(proj));

                    auto first = RAH2_NS::ranges::begin(r);
                    auto last = RAH2_NS::ranges::end(r);

                    using V = range_value_t<R>;
                    V const first_val(*first);
                    RAH2_NS::ranges::minmax_result<V> result{first_val, first_val};
                    ++first;
                    for (; first != last; ++first)
                    {
                        V new_val(*first);
                        if (pred_proj(new_val, result.min))
                            result.min = std::move(new_val);
                        else if (pred_proj(result.max, new_val))
                            result.max = std::move(new_val);
                    }
                    return result;
                }

                template <
                    typename R,
                    typename Comp = RAH2_NS::ranges::less,
                    typename Proj = RAH2_NS::details::identity,
                    RAH2_STD::enable_if_t<forward_range<R> && !input_range<R>>* = nullptr>
                constexpr RAH2_NS::ranges::minmax_result<range_value_t<R>>
                operator()(R&& r, Comp&& comp = {}, Proj&& proj = {}) const
                {
                    auto result = RAH2_NS::ranges::minmax_element(r, RAH2_FWD(comp), RAH2_FWD(proj));
                    return {*result.min, *result.max};
                }
            };
        } // namespace niebloids
        constexpr niebloids::minmax_fn minmax;

        template <class I>
        using prev_permutation_result = RAH2_NS::ranges::in_found_result<I>;
        namespace niebloids
        {
            struct prev_permutation_fn
            {
                template <
                    typename I,
                    typename S,
                    class Comp = RAH2_NS::ranges::less,
                    typename Proj = RAH2_NS::details::identity,
                    RAH2_STD::enable_if_t<
                        RAH2_NS::bidirectional_iterator<I> && RAH2_NS::sentinel_for<S, I>>* = nullptr>
                // requires RAH2_STD::sortable<I, Comp, Proj>
                constexpr RAH2_NS::ranges::prev_permutation_result<I>
                operator()(I first, S last, Comp&& comp = {}, Proj&& proj = {}) const
                {
                    auto pred_proj = details::wrap_pred_proj(RAH2_FWD(comp), RAH2_FWD(proj));

                    // check that the sequence has at least two elements
                    if (first == last)
                        return {RAH2_STD::move(first), false};
                    auto i{first};
                    ++i;
                    if (i == last)
                        return {RAH2_STD::move(i), false};
                    auto i_last{RAH2_NS::ranges::next(first, last)};
                    i = i_last;
                    --i;
                    // main "permutating" loop
                    for (;;)
                    {
                        auto i1{i};
                        --i;
                        if (RAH2_INVOKE_2(pred_proj, *i1, *i))
                        {
                            auto j{i_last};
                            while (!RAH2_INVOKE_2(pred_proj, *--j, *i))
                                ;
                            RAH2_NS::ranges::iter_swap(i, j);
                            RAH2_NS::ranges::reverse(i1, last);
                            return {RAH2_STD::move(i_last), true};
                        }
                        // permutation "space" is exhausted
                        if (i == first)
                        {
                            RAH2_NS::ranges::reverse(first, last);
                            return {RAH2_STD::move(i_last), false};
                        }
                    }
                }

                template <
                    typename R, // RAH2_NS::bidirectional_range
                    class Comp = RAH2_NS::ranges::less,
                    typename Proj = RAH2_NS::details::identity,
                    RAH2_STD::enable_if_t<bidirectional_range<R>>* = nullptr>
                // requires RAH2_STD::sortable<RAH2_NS::ranges::iterator_t<R>, Comp, Proj>
                constexpr RAH2_NS::ranges::prev_permutation_result<RAH2_NS::ranges::borrowed_iterator_t<R>>
                operator()(R&& r, Comp&& comp = {}, Proj&& proj = {}) const
                {
                    return (*this)(
                        RAH2_NS::ranges::begin(r),
                        RAH2_NS::ranges::end(r),
                        RAH2_FWD(comp),
                        RAH2_FWD(proj));
                }
            };
        } // namespace niebloids
        constexpr niebloids::prev_permutation_fn prev_permutation{};

        template <class O, class T>
        using iota_result = RAH2_NS::ranges::out_value_result<O, T>;
        namespace niebloids
        {
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
                        *first = RAH2_NS::as_const(value);
                        ++first;
                        ++value;
                    }
                    return {RAH2_STD::move(first), RAH2_STD::move(value)};
                }

                template <
                    typename T, // RAH2_STD::weakly_incrementable
                    typename R // RAH2_NS::output_range<const T&>
                    >
                constexpr iota_result<RAH2_NS::ranges::borrowed_iterator_t<R>, T>
                operator()(R&& r, T value) const
                {
                    return (*this)(
                        RAH2_NS::ranges::begin(r), RAH2_NS::ranges::end(r), RAH2_STD::move(value));
                }
            };
        } // namespace niebloids
        constexpr niebloids::iota_fn iota;

        template <class I, class O>
        using uninitialized_copy_result = RAH2_NS::ranges::in_out_result<I, O>;
        namespace niebloids
        {
            struct uninitialized_copy_fn
            {
                // TODO : improve efficiency when the copied type is a TrivialType
                template <
                    typename I, // RAH2_STD::input_iterator
                    typename S1, // RAH2_STD::sentinel_for<I>
                    typename O, // no-throw-forward-iterator
                    typename S2, // no-throw-sentinel-for<O>
                    RAH2_STD::enable_if_t<not(
                        RAH2_NS::sized_sentinel_for<I, S1> and RAH2_NS::sized_sentinel_for<O, S2>
                        and RAH2_NS::contiguous_iterator<I> and RAH2_NS::contiguous_iterator<O>
                        and RAH2_NS::is_trivially_copyable_v<RAH2_NS::iter_value_t<I>>
                        and RAH2_NS::is_same_v<RAH2_NS::iter_value_t<I>, RAH2_NS::iter_value_t<O>>)>* = nullptr>
                // requires RAH2_STD::constructible_from<RAH2_STD::iter_value_t<O>, RAH2_STD::iter_reference_t<I>>
                RAH2_NS::ranges::uninitialized_copy_result<I, O>
                operator()(I ifirst, S1 ilast, O ofirst, S2 olast) const
                {
                    O current{ofirst};
                    size_t construct_count = 0;
                    try
                    {
                        for (; !(ifirst == ilast or current == olast);
                             ++ifirst, ++current, ++construct_count)
                            RAH2_NS::ranges::construct_at(RAH2_STD::addressof(*current), *ifirst);
                        return {RAH2_STD::move(ifirst), RAH2_STD::move(current)};
                    }
                    catch (...) // rollback: destroy constructed elements
                    {
                        for (size_t i = 0; i != construct_count; ++ofirst, ++i)
                            RAH2_NS::ranges::destroy_at(RAH2_STD::addressof(*ofirst));
                        throw;
                    }
                }

                // TODO : improve efficiency when the copied type is a TrivialType
                template <
                    typename I, // RAH2_STD::input_iterator
                    typename S1, // RAH2_STD::sentinel_for<I>
                    typename O, // no-throw-forward-iterator
                    typename S2, // no-throw-sentinel-for<O>
                    RAH2_STD::enable_if_t<
                        RAH2_NS::sized_sentinel_for<I, S1> and RAH2_NS::sized_sentinel_for<O, S2>
                        and RAH2_NS::contiguous_iterator<I> and RAH2_NS::contiguous_iterator<O>
                        and is_trivially_copyable_v<RAH2_NS::iter_value_t<I>>
                        and RAH2_NS::is_same_v<RAH2_NS::iter_value_t<I>, RAH2_NS::iter_value_t<O>>>* = nullptr>
                // requires RAH2_STD::constructible_from<RAH2_STD::iter_value_t<O>, RAH2_STD::iter_reference_t<I>>
                RAH2_NS::ranges::uninitialized_copy_result<I, O>
                operator()(I ifirst, S1 ilast, O ofirst, S2 olast) const
                {
                    auto const len = RAH2_STD::min(
                        RAH2_STD::distance(ifirst, ilast), RAH2_STD::distance(ofirst, olast));
                    memcpy(&(*ofirst), &(*ifirst), len * sizeof(RAH2_NS::iter_value_t<I>));
                    return {ifirst + len, ofirst + len};
                }

                template <
                    typename IR, // ranges::input_range
                    typename OR, // no-throw-forward-range
                    RAH2_STD::enable_if_t<not(
                        RAH2_NS::ranges::sized_range<IR> and RAH2_NS::ranges::sized_range<OR>
                        and RAH2_NS::ranges::contiguous_range<IR> and RAH2_NS::ranges::contiguous_range<OR>
                        and is_trivially_copyable_v<RAH2_NS::ranges::range_value_t<IR>>
                        and RAH2_NS::is_same_v<
                            RAH2_NS::ranges::range_value_t<IR>,
                            RAH2_NS::ranges::range_value_t<OR>>)>* = nullptr

                    >
                // requires RAH2_STD::constructible_from<ranges::range_value_t<OR>, ranges::range_reference_t<IR>>
                RAH2_NS::ranges::uninitialized_copy_result<
                    RAH2_NS::ranges::borrowed_iterator_t<IR>,
                    RAH2_NS::ranges::borrowed_iterator_t<OR>>
                operator()(IR&& in_range, OR&& out_range) const
                {
                    return (*this)(
                        RAH2_NS::ranges::begin(in_range),
                        RAH2_NS::ranges::end(in_range),
                        RAH2_NS::ranges::begin(out_range),
                        RAH2_NS::ranges::end(out_range));
                }

                template <
                    typename IR, // ranges::input_range
                    typename OR, // no-throw-forward-range
                    RAH2_STD::enable_if_t<
                        RAH2_NS::ranges::sized_range<IR> and RAH2_NS::ranges::sized_range<OR>
                        and RAH2_NS::ranges::contiguous_range<IR> and RAH2_NS::ranges::contiguous_range<OR>
                        and is_trivially_copyable_v<RAH2_NS::ranges::range_value_t<IR>>
                        and RAH2_NS::is_same_v<
                            RAH2_NS::ranges::range_value_t<IR>,
                            RAH2_NS::ranges::range_value_t<OR>>>* = nullptr>
                // requires RAH2_STD::constructible_from<ranges::range_value_t<OR>, ranges::range_reference_t<IR>>
                RAH2_NS::ranges::uninitialized_copy_result<
                    RAH2_NS::ranges::borrowed_iterator_t<IR>,
                    RAH2_NS::ranges::borrowed_iterator_t<OR>>
                operator()(IR&& in_range, OR&& out_range) const
                {
                    return (*this)(
                        RAH2_NS::ranges::begin(in_range),
                        RAH2_NS::ranges::begin(in_range) + RAH2_NS::ranges::size(in_range),
                        RAH2_NS::ranges::begin(out_range),
                        RAH2_NS::ranges::begin(out_range) + RAH2_NS::ranges::size(out_range));
                }
            };
        } // namespace niebloids
        constexpr niebloids::uninitialized_copy_fn uninitialized_copy{};

        template <class I, class O>
        using uninitialized_copy_n_result = RAH2_NS::ranges::in_out_result<I, O>;
        namespace niebloids
        {
            struct uninitialized_copy_n_fn
            {
                // TODO : improve efficiency when the copied type is a TrivialType
                template <
                    typename I, // RAH2_STD::input_iterator
                    typename O, // no-throw-input-iterator
                    typename S, // no-throw-sentinel-for<O>
                    RAH2_STD::enable_if_t<not(
                        RAH2_NS::contiguous_iterator<I> and RAH2_NS::contiguous_iterator<O>
                        and RAH2_NS::is_trivially_copyable_v<RAH2_NS::iter_value_t<I>>
                        and RAH2_NS::is_same_v<RAH2_NS::iter_value_t<I>, RAH2_NS::iter_value_t<O>>)>* = nullptr>
                // requires RAH2_STD::constructible_from<RAH2_STD::iter_value_t<O>, RAH2_STD::iter_reference_t<I>>
                RAH2_NS::ranges::uninitialized_copy_n_result<I, O>
                operator()(I ifirst, RAH2_NS::iter_difference_t<I> count, O ofirst, S olast) const
                {
                    O current{ofirst};
                    size_t copy_count = 0;
                    try
                    {
                        for (; count > 0 && current != olast;
                             ++ifirst, ++current, --count, ++copy_count)
                            RAH2_NS::ranges::construct_at(RAH2_STD::addressof(*current), *ifirst);
                        return {RAH2_STD::move(ifirst), RAH2_STD::move(current)};
                    }
                    catch (...) // rollback: destroy constructed elements
                    {
                        for (size_t i = 0; i != copy_count; ++ofirst, ++i)
                            RAH2_NS::ranges::destroy_at(RAH2_STD::addressof(*ofirst));
                        throw;
                    }
                }

                template <
                    typename I, // RAH2_STD::input_iterator
                    typename O, // no-throw-input-iterator
                    typename S, // no-throw-sentinel-for<O>
                    RAH2_STD::enable_if_t<
                        RAH2_NS::contiguous_iterator<I> and RAH2_NS::contiguous_iterator<O>
                        and RAH2_NS::is_trivially_copyable_v<RAH2_NS::iter_value_t<I>>
                        and RAH2_NS::is_same_v<RAH2_NS::iter_value_t<I>, RAH2_NS::iter_value_t<O>>>* = nullptr>
                RAH2_NS::ranges::uninitialized_copy_n_result<I, O>
                operator()(I ifirst, RAH2_NS::iter_difference_t<I> count, O ofirst, S olast) const
                {
                    auto len = RAH2_STD::min(count, ranges::distance(ofirst, olast));
                    memcpy(&(*ofirst), &(*ifirst), len * sizeof(RAH2_NS::iter_value_t<I>));
                    return {ifirst + count, ofirst + count};
                }
            };
        } // namespace niebloids
        constexpr niebloids::uninitialized_copy_n_fn uninitialized_copy_n{};
        namespace niebloids
        {
            struct uninitialized_fill_fn
            {
                template <
                    typename I, // no-throw-forward-iterator
                    typename S, // no-throw-sentinel-for<I>
                    class T>
                // requires RAH2_STD::constructible_from<RAH2_STD::iter_value_t<I>, const T&>
                I operator()(I first, S last, T const& x) const
                {
                    I rollback{first};
                    size_t construct_count = 0;
                    try
                    {
                        for (; !(first == last); ++first, ++construct_count)
                            RAH2_NS::ranges::construct_at(RAH2_STD::addressof(*first), x);
                        return first;
                    }
                    catch (...)
                    {
                        // rollback: destroy constructed elements
                        for (size_t i = 0; i != construct_count; ++rollback, ++i)
                            RAH2_NS::ranges::destroy_at(RAH2_STD::addressof(*rollback));
                        throw;
                    }
                }

                template <
                    typename R, // no-throw-forward-range
                    class T>
                // requires RAH2_STD::constructible_from<ranges::range_value_t<R>, const T&>
                RAH2_NS::ranges::borrowed_iterator_t<R> operator()(R&& r, T const& x) const
                {
                    return (*this)(RAH2_NS::ranges::begin(r), RAH2_NS::ranges::end(r), x);
                }
            };
        } // namespace niebloids
        constexpr niebloids::uninitialized_fill_fn uninitialized_fill{};
        namespace niebloids
        {
            struct uninitialized_fill_n_fn
            {
                template <
                    typename I, // no-throw-forward-range
                    class T>
                // requires RAH2_STD::constructible_from<RAH2_STD::iter_value_t<I>, const T&>
                I operator()(I first, RAH2_NS::iter_difference_t<I> n, T const& x) const
                {
                    I rollback{first};
                    try
                    {
                        for (; n-- > 0; ++first)
                            RAH2_NS::ranges::construct_at(RAH2_STD::addressof(*first), x);
                        return first;
                    }
                    catch (...) // rollback: destroy constructed elements
                    {
                        for (; rollback != first; ++rollback)
                            RAH2_NS::ranges::destroy_at(RAH2_STD::addressof(*rollback));
                        throw;
                    }
                }
            };
        } // namespace niebloids
        constexpr niebloids::uninitialized_fill_n_fn uninitialized_fill_n{};

        template <class I, class O>
        using uninitialized_move_result = RAH2_NS::ranges::in_out_result<I, O>;
        namespace niebloids
        {
            struct uninitialized_move_fn
            {
                template <
                    typename I, // RAH2_STD::input_iterator
                    typename S1, // RAH2_STD::sentinel_for<I>
                    typename O, // no-throw-forward-iterator
                    typename S2, // no-throw-sentinel-for<O>
                    RAH2_STD::enable_if_t<not(
                        RAH2_NS::sized_sentinel_for<I, S1> and RAH2_NS::sized_sentinel_for<O, S2>
                        and RAH2_NS::contiguous_iterator<I> and RAH2_NS::contiguous_iterator<O>
                        and RAH2_NS::is_trivially_copyable_v<RAH2_NS::iter_value_t<I>>
                        and RAH2_NS::is_same_v<RAH2_NS::iter_value_t<I>, RAH2_NS::iter_value_t<O>>)>* = nullptr>
                //requires RAH2_STD::constructible_from<RAH2_STD::iter_value_t<O>, RAH2_STD::iter_rvalue_reference_t<I>>
                RAH2_NS::ranges::uninitialized_move_result<I, O>
                operator()(I ifirst, S1 ilast, O ofirst, S2 olast) const
                {
                    O current{ofirst};
                    size_t move_count = 0;
                    try
                    {
                        for (; !(ifirst == ilast or current == olast);
                             ++ifirst, ++current, ++move_count)
                            ::new (const_cast<void*>(
                                static_cast<void const volatile*>(RAH2_STD::addressof(*current))))
                                RAH2_STD::remove_reference_t<RAH2_NS::iter_reference_t<O>>(
                                    RAH2_NS::ranges::iter_move(ifirst));
                        return {RAH2_STD::move(ifirst), RAH2_STD::move(current)};
                    }
                    catch (...) // rollback: destroy constructed elements
                    {
                        for (size_t i = 0; i < move_count; ++ofirst, ++i)
                            RAH2_NS::ranges::destroy_at(RAH2_STD::addressof(*ofirst));
                        throw;
                    }
                }

                template <
                    typename I, // RAH2_STD::input_iterator
                    typename S1, // RAH2_STD::sentinel_for<I>
                    typename O, // no-throw-forward-iterator
                    typename S2, // no-throw-sentinel-for<O>
                    RAH2_STD::enable_if_t<
                        RAH2_NS::sized_sentinel_for<I, S1> and RAH2_NS::sized_sentinel_for<O, S2>
                        and RAH2_NS::contiguous_iterator<I> and RAH2_NS::contiguous_iterator<O>
                        and RAH2_NS::is_trivially_copyable_v<RAH2_NS::iter_value_t<I>>
                        and RAH2_NS::is_same_v<RAH2_NS::iter_value_t<I>, RAH2_NS::iter_value_t<O>>>* = nullptr>
                RAH2_NS::ranges::uninitialized_copy_result<I, O>
                operator()(I ifirst, S1 ilast, O ofirst, S2 olast) const
                {
                    auto const len = RAH2_STD::min(
                        RAH2_STD::distance(ifirst, ilast), RAH2_STD::distance(ofirst, olast));
                    memcpy(&(*ofirst), &(*ifirst), len * sizeof(RAH2_NS::iter_value_t<I>));
                    return {ifirst + len, ofirst + len};
                }

                template <
                    typename IR, // ranges::input_range
                    typename OR, // no-throw-forward-range
                    RAH2_STD::enable_if_t<not(
                        RAH2_NS::ranges::sized_range<IR> and RAH2_NS::ranges::sized_range<OR>
                        and RAH2_NS::ranges::contiguous_range<IR> and RAH2_NS::ranges::contiguous_range<OR>
                        and RAH2_NS::is_trivially_copyable_v<RAH2_NS::ranges::range_value_t<IR>>
                        and RAH2_NS::is_same_v<
                            RAH2_NS::ranges::range_value_t<IR>,
                            RAH2_NS::ranges::range_value_t<OR>>)>* = nullptr>
                // requires RAH2_STD::constructible_from<ranges::range_value_t<OR>, ranges::range_rvalue_reference_t<IR>>
                RAH2_NS::ranges::uninitialized_move_result<
                    RAH2_NS::ranges::borrowed_iterator_t<IR>,
                    RAH2_NS::ranges::borrowed_iterator_t<OR>>
                operator()(IR&& in_range, OR&& out_range) const
                {
                    return (*this)(
                        RAH2_NS::ranges::begin(in_range),
                        RAH2_NS::ranges::end(in_range),
                        RAH2_NS::ranges::begin(out_range),
                        RAH2_NS::ranges::end(out_range));
                }

                template <
                    typename IR, // ranges::input_range
                    typename OR, // no-throw-forward-range
                    RAH2_STD::enable_if_t<
                        RAH2_NS::ranges::sized_range<IR> and RAH2_NS::ranges::sized_range<OR>
                        and RAH2_NS::ranges::contiguous_range<IR> and RAH2_NS::ranges::contiguous_range<OR>
                        and RAH2_NS::is_trivially_copyable_v<RAH2_NS::ranges::range_value_t<IR>>
                        and RAH2_NS::is_same_v<
                            RAH2_NS::ranges::range_value_t<IR>,
                            RAH2_NS::ranges::range_value_t<OR>>>* = nullptr>
                // requires RAH2_STD::constructible_from<ranges::range_value_t<OR>, ranges::range_rvalue_reference_t<IR>>
                RAH2_NS::ranges::uninitialized_move_result<
                    RAH2_NS::ranges::borrowed_iterator_t<IR>,
                    RAH2_NS::ranges::borrowed_iterator_t<OR>>
                operator()(IR&& in_range, OR&& out_range) const
                {
                    return (*this)(
                        RAH2_NS::ranges::begin(in_range),
                        RAH2_NS::ranges::begin(in_range) + RAH2_NS::ranges::size(in_range),
                        RAH2_NS::ranges::begin(out_range),
                        RAH2_NS::ranges::begin(out_range) + RAH2_NS::ranges::size(out_range));
                }
            };
        } // namespace niebloids
        constexpr niebloids::uninitialized_move_fn uninitialized_move{};

        template <class I, class O>
        using uninitialized_move_n_result = RAH2_NS::ranges::in_out_result<I, O>;
        namespace niebloids
        {
            struct uninitialized_move_n_fn
            {
                template <
                    typename I, // RAH2_STD::input_iterator
                    typename O, // no-throw-forward-iterator
                    typename S, // no-throw-sentinel-for<O>
                    RAH2_STD::enable_if_t<not(
                        RAH2_NS::contiguous_iterator<I> and RAH2_NS::contiguous_iterator<O>
                        and RAH2_NS::is_trivially_copyable_v<RAH2_NS::iter_value_t<I>>
                        and RAH2_NS::is_same_v<RAH2_NS::iter_value_t<I>, RAH2_NS::iter_value_t<O>>)>* = nullptr>
                RAH2_NS::ranges::uninitialized_move_n_result<I, O>
                operator()(I ifirst, RAH2_NS::iter_difference_t<I> n, O ofirst, S olast) const
                {
                    O current{ofirst};
                    size_t move_count = 0;
                    try
                    {
                        for (; n-- > 0 && current != olast; ++ifirst, ++current, ++move_count)
                            ::new (const_cast<void*>(
                                static_cast<void const volatile*>(RAH2_STD::addressof(*current))))
                                RAH2_STD::remove_reference_t<RAH2_NS::iter_reference_t<O>>(
                                    RAH2_NS::ranges::iter_move(ifirst));
                        return {RAH2_STD::move(ifirst), RAH2_STD::move(current)};
                    }
                    catch (...) // rollback: destroy constructed elements
                    {
                        for (size_t i = 0; i != move_count; ++ofirst, ++i)
                            RAH2_NS::ranges::destroy_at(RAH2_STD::addressof(*ofirst));
                        throw;
                    }
                }

                template <
                    typename I, // RAH2_STD::input_iterator
                    typename O, // no-throw-input-iterator
                    typename S, // no-throw-sentinel-for<O>
                    RAH2_STD::enable_if_t<
                        RAH2_NS::contiguous_iterator<I> and RAH2_NS::contiguous_iterator<O>
                        and RAH2_NS::is_trivially_copyable_v<RAH2_NS::iter_value_t<I>>
                        and RAH2_NS::is_same_v<RAH2_NS::iter_value_t<I>, RAH2_NS::iter_value_t<O>>>* = nullptr>
                RAH2_NS::ranges::uninitialized_copy_n_result<I, O>
                operator()(I ifirst, RAH2_NS::iter_difference_t<I> count, O ofirst, S olast) const
                {
                    auto len = RAH2_STD::min(count, ranges::distance(ofirst, olast));
                    memcpy(&(*ofirst), &(*ifirst), len * sizeof(RAH2_NS::iter_value_t<I>));
                    return {ifirst + count, ofirst + count};
                }
            };
        } // namespace niebloids
        constexpr niebloids::uninitialized_move_n_fn uninitialized_move_n{};
        namespace niebloids
        {
            struct uninitialized_default_construct_fn
            {
                template <
                    typename I, // no-throw-forward-iterator
                    typename S, // no-throw-sentinel-for<I>
                    RAH2_STD::enable_if_t<RAH2_NS::is_trivially_default_constructible_v<
                        RAH2_STD::remove_reference_t<RAH2_NS::iter_reference_t<I>>>>* = nullptr>
                // requires RAH2_STD::default_initializable<RAH2_STD::iter_value_t<I>>
                I operator()(I first, S last) const
                {
                    return RAH2_NS::ranges::next(first, last); // skip initialization
                }
                template <
                    typename I, // no-throw-forward-iterator
                    typename S, // no-throw-sentinel-for<I>
                    RAH2_STD::enable_if_t<!RAH2_NS::is_trivially_default_constructible_v<
                        RAH2_STD::remove_reference_t<RAH2_NS::iter_reference_t<I>>>>* = nullptr>
                // requires RAH2_STD::default_initializable<RAH2_STD::iter_value_t<I>>
                I operator()(I first, S last) const
                {
                    using ValueType = RAH2_STD::remove_reference_t<RAH2_NS::iter_reference_t<I>>;
                    I rollback{first};
                    try
                    {
                        for (; !(first == last); ++first)
                            ::new (const_cast<void*>(static_cast<void const volatile*>(
                                RAH2_STD::addressof(*first)))) ValueType;
                        return first;
                    }
                    catch (...) // rollback: destroy constructed elements
                    {
                        for (; rollback != first; ++rollback)
                            RAH2_NS::ranges::destroy_at(RAH2_STD::addressof(*rollback));
                        throw;
                    }
                }

                template <typename R // no-throw-forward-range
                          >
                // requires RAH2_STD::default_initializable<ranges::range_value_t<R>>
                RAH2_NS::ranges::borrowed_iterator_t<R> operator()(R&& r) const
                {
                    return (*this)(RAH2_NS::ranges::begin(r), RAH2_NS::ranges::end(r));
                }
            };
        } // namespace niebloids
        constexpr niebloids::uninitialized_default_construct_fn uninitialized_default_construct{};
        namespace niebloids
        {
            struct uninitialized_default_construct_n_fn
            {
                template <
                    typename I, // no-throw-forward-iterator
                    RAH2_STD::enable_if_t<RAH2_NS::is_trivially_default_constructible_v<
                        RAH2_STD::remove_reference_t<RAH2_NS::iter_reference_t<I>>>>* = nullptr>
                // requires RAH2_STD::default_initializable<RAH2_STD::iter_value_t<I>>
                I operator()(I first, RAH2_NS::iter_difference_t<I> n) const
                {
                    return RAH2_NS::ranges::next(first, n); // skip initialization
                }

                template <
                    typename I, // no-throw-forward-iterator
                    RAH2_STD::enable_if_t<!RAH2_NS::is_trivially_default_constructible_v<
                        RAH2_STD::remove_reference_t<RAH2_NS::iter_reference_t<I>>>>* = nullptr>
                // requires RAH2_STD::default_initializable<RAH2_STD::iter_value_t<I>>
                I operator()(I first, RAH2_NS::iter_difference_t<I> n) const
                {
                    using ValueType = RAH2_STD::remove_reference_t<RAH2_NS::iter_reference_t<I>>;
                    I rollback{first};
                    try
                    {
                        for (; n-- > 0; ++first)
                            ::new (const_cast<void*>(static_cast<void const volatile*>(
                                RAH2_STD::addressof(*first)))) ValueType;
                        return first;
                    }
                    catch (...) // rollback: destroy constructed elements
                    {
                        for (; rollback != first; ++rollback)
                            RAH2_NS::ranges::destroy_at(RAH2_STD::addressof(*rollback));
                        throw;
                    }
                }
            };
        } // namespace niebloids
        constexpr niebloids::uninitialized_default_construct_n_fn uninitialized_default_construct_n{};

        namespace niebloids
        {
            struct uninitialized_value_construct_n_fn
            {
                template <
                    typename I, // no-throw-forward-iterator
                    typename T = RAH2_STD::remove_reference_t<RAH2_NS::iter_reference_t<I>>,
                    RAH2_STD::enable_if_t<
                        (RAH2_NS::is_trivial_v<T> && RAH2_NS::is_copy_assignable_v<T>)>* = nullptr>
                I operator()(I first, RAH2_NS::iter_difference_t<I> n) const
                {
                    return RAH2_NS::ranges::fill_n(first, n, T());
                }

                template <
                    typename I, // no-throw-forward-iterator
                    typename T = RAH2_STD::remove_reference_t<RAH2_NS::iter_reference_t<I>>,
                    RAH2_STD::enable_if_t<
                        !(RAH2_NS::is_trivial_v<T> && RAH2_NS::is_copy_assignable_v<T>)>* = nullptr>
                I operator()(I first, RAH2_NS::iter_difference_t<I> n) const
                {
                    I rollback{first};
                    try
                    {
                        for (; n-- > 0; ++first)
                            ::new (const_cast<void*>(
                                static_cast<void const volatile*>(RAH2_STD::addressof(*first)))) T();
                        return first;
                    }
                    catch (...) // rollback: destroy constructed elements
                    {
                        for (; rollback != first; ++rollback)
                            RAH2_NS::ranges::destroy_at(RAH2_STD::addressof(*rollback));
                        throw;
                    }
                }
            };
        } // namespace niebloids
        constexpr niebloids::uninitialized_value_construct_n_fn uninitialized_value_construct_n{};

        namespace niebloids
        {
            struct uninitialized_value_construct_fn
            {
                template <
                    typename I,
                    typename S,
                    typename T = RAH2_STD::remove_reference_t<RAH2_NS::iter_reference_t<I>>,
                    RAH2_STD::enable_if_t<RAH2_NS::is_trivial_v<T> && RAH2_NS::is_copy_assignable_v<T>>* = nullptr>
                I impl(I first, S last) const
                {
                    return RAH2_NS::ranges::fill(first, last, T());
                }

                template <
                    typename I,
                    typename S,
                    typename T = RAH2_STD::remove_reference_t<RAH2_NS::iter_reference_t<I>>,
                    RAH2_STD::enable_if_t<
                        !(RAH2_NS::is_trivial_v<T> && RAH2_NS::is_copy_assignable_v<T>)>* = nullptr>
                I impl(I first, S last) const
                {
                    I rollback{first};
                    try
                    {
                        for (; !(first == last); ++first)
                            ::new (const_cast<void*>(
                                static_cast<void const volatile*>(RAH2_STD::addressof(*first)))) T();
                        return first;
                    }
                    catch (...) // rollback: destroy constructed elements
                    {
                        for (; rollback != first; ++rollback)
                            RAH2_NS::ranges::destroy_at(RAH2_STD::addressof(*rollback));
                        throw;
                    }
                }

                template <typename I, typename S, RAH2_STD::enable_if_t<!RAH2_NS::sized_sentinel_for<S, I>>* = nullptr>
                I operator()(I first, S last) const
                {
                    return impl(RAH2_MOV(first), RAH2_MOV(last));
                }

                template <typename I, typename S, RAH2_STD::enable_if_t<RAH2_NS::sized_sentinel_for<S, I>>* = nullptr>
                I operator()(I first, S last) const
                {
                    return uninitialized_value_construct_n(
                        RAH2_MOV(first), RAH2_NS::ranges::distance(first, last));
                }

                template <typename R, RAH2_STD::enable_if_t<!RAH2_NS::ranges::sized_range<R>>* = nullptr>
                RAH2_NS::ranges::borrowed_iterator_t<R> operator()(R&& r) const
                {
                    return (*this)(RAH2_NS::ranges::begin(r), RAH2_NS::ranges::end(r));
                }

                template <typename R, RAH2_STD::enable_if_t<RAH2_NS::ranges::sized_range<R>>* = nullptr>
                RAH2_NS::ranges::borrowed_iterator_t<R> operator()(R&& r) const
                {
                    return uninitialized_value_construct_n(
                        RAH2_NS::ranges::begin(r), RAH2_NS::ranges::size(r));
                }
            };
        } // namespace niebloids
        constexpr niebloids::uninitialized_value_construct_fn uninitialized_value_construct{};

        namespace niebloids
        {
            struct shuffle
            {
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
                ///     struct Rand{ size_t operator()(size_t n) { return (size_t)(rand() % n); } }; // Note: The C rand function is poor and slow.
                ///     Rand randInstance;
                ///     shuffle(pArrayBegin, pArrayEnd, randInstance);
                ///
                // See the C++11 Standard, 26.5.1.3, Uniform random number generator requirements.
                // Also http://en.cppreference.com/w/cpp/numeric/random/uniform_int_distribution
                template <typename DistibType, typename RandomAccessIterator, typename Sentinel, typename UniformRandomNumberGenerator>
                void
                impl(RandomAccessIterator first, Sentinel last, UniformRandomNumberGenerator&& urng) const
                {
                    if (first != last)
                    {
                        using uniform_int_distrib = RAH2_STD::uniform_int_distribution<DistibType>;
                        using uniform_int_distribution_param_type =
                            typename uniform_int_distrib::param_type;

                        uniform_int_distrib uid;

                        for (RandomAccessIterator i = first + 1; i != last; ++i)
                        {
                            auto const random_index =
                                static_cast<iter_difference_t<RandomAccessIterator>>(uid(
                                    urng,
                                    uniform_int_distribution_param_type(0u, DistibType(i - first))));
                            RAH2_NS::ranges::iter_swap(i, first + random_index);
                        }
                    }
                }

                template <typename D, typename RandomAccessIterator, typename N, typename UniformRandomNumberGenerator>
                void impl_n(RandomAccessIterator first, N len, UniformRandomNumberGenerator&& urng) const
                {
                    if (len != 0)
                    {
                        using distrib = RAH2_STD::uniform_int_distribution<D>;
                        RandomAccessIterator i = first + 1;
                        --len;
                        for (auto u = len / 4; u != 0; --u)
                        {
                            RAH2_NS::ranges::iter_swap(i, first + distrib(0, D(i - first))(urng));
                            ++i;
                            RAH2_NS::ranges::iter_swap(i, first + distrib(0, D(i - first))(urng));
                            ++i;
                            RAH2_NS::ranges::iter_swap(i, first + distrib(0, D(i - first))(urng));
                            ++i;
                            RAH2_NS::ranges::iter_swap(i, first + distrib(0, D(i - first))(urng));
                            ++i;
                        }
                        for (auto u = len % 4; u != 0; --u)
                        {
                            RAH2_NS::ranges::iter_swap(i, first + distrib(0, D(i - first))(urng));
                            ++i;
                        }
                    }
                }

                template <typename RandomAccessIterator, typename Sentinel, typename UniformRandomNumberGenerator>
                void dispatch(
                    RandomAccessIterator first, Sentinel last, UniformRandomNumberGenerator&& urng) const
                {
                    auto first_last = details::unwrap(RAH2_STD::move(first), RAH2_STD::move(last));
                    impl<uint32_t>(
                        RAH2_MOV(first_last.iterator), RAH2_MOV(first_last.sentinel), RAH2_FWD(urng));
                }

                template <typename RandomAccessIterator, typename N, typename UniformRandomNumberGenerator>
                void dispatch_n(
                    RandomAccessIterator first, N input_size, UniformRandomNumberGenerator&& urng) const
                {
                    auto first_last = details::unwrap_begin(RAH2_STD::move(first));
                    if (input_size >= RAH2_STD::numeric_limits<uint16_t>::max())
                    {
                        impl_n<uint32_t>(RAH2_MOV(first_last.iterator), input_size, RAH2_FWD(urng));
                    }
                    else
                    {
                        impl_n<uint16_t>(RAH2_MOV(first_last.iterator), input_size, RAH2_FWD(urng));
                    }
                }

                template <
                    typename RandomAccessIterator,
                    typename Sentinel,
                    typename UniformRandomNumberGenerator,
                    std::enable_if_t<not RAH2_NS::sized_sentinel_for<Sentinel, RandomAccessIterator>>* = nullptr>
                void operator()(
                    RandomAccessIterator first, Sentinel last, UniformRandomNumberGenerator&& urng) const
                {
                    dispatch(RAH2_MOV(first), RAH2_MOV(last), RAH2_FWD(urng));
                }
                template <
                    typename RandomAccessIterator,
                    typename Sentinel,
                    typename UniformRandomNumberGenerator,
                    std::enable_if_t<RAH2_NS::sized_sentinel_for<Sentinel, RandomAccessIterator>>* = nullptr>
                void operator()(
                    RandomAccessIterator first, Sentinel last, UniformRandomNumberGenerator&& urng) const
                {
                    dispatch_n(
                        RAH2_MOV(first), RAH2_NS::ranges::distance(first, last), RAH2_FWD(urng));
                }

                template <
                    typename RandomRange,
                    typename UniformRandomNumberGenerator,
                    std::enable_if_t<RAH2_NS::ranges::sized_range<RandomRange>>* = nullptr>
                void operator()(RandomRange&& range, UniformRandomNumberGenerator&& urng) const
                {
                    dispatch_n(
                        RAH2_NS::ranges::begin(range), RAH2_NS::ranges::size(range), RAH2_FWD(urng));
                }
                template <
                    typename RandomRange,
                    typename UniformRandomNumberGenerator,
                    std::enable_if_t<not RAH2_NS::ranges::sized_range<RandomRange>>* = nullptr>
                void operator()(RandomRange&& range, UniformRandomNumberGenerator&& urng) const
                {
                    (*this)(
                        RAH2_NS::ranges::begin(range), RAH2_NS::ranges::end(range), RAH2_FWD(urng));
                }
            };
        } // namespace niebloids
        constexpr niebloids::shuffle shuffle;
    } // namespace ranges
} // namespace RAH2_NS
