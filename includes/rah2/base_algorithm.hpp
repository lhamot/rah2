#pragma once

#include "range_bases.hpp"

#ifdef RAH2_USE_EASTL

#include <EASTL/string.h>
#include <EASTL/utility.h>
#include <EASTL/functional.h>

#else

#include <string.h> // memcmp
#include <utility> // std::move
#include <functional> // std::ref

#endif

namespace RAH2_NS
{
    namespace ranges
    {
        // ************************************** algorithm results ***********************************

        // TODO : Move into algorithm.hpp
        template <class I, class O>
        struct in_out_result
        {
            I in;
            O out;
        };
        template <class I1, class I2, class O>
        struct in_in_out_result
        {
            I1 in1;
            I2 in2;
            O out;
        };

        template <class I, class O1, class O2>
        struct in_out_out_result
        {
            I in;
            O1 out1;
            O2 out2;
        };

        template <class I, class F>
        struct in_fun_result
        {
            I in;
            F fun;
        };

        template <class I1, class I2>
        struct in_in_result
        {
            I1 in1;
            I2 in2;
        };

        template <class T>
        struct min_max_result
        {
            T min;
            T max;
        };

        template <class I>
        struct in_found_result
        {
            I in;
            bool found;
        };

        template <class I, class T>
        struct in_value_result
        {
            I in;
            T value;
        };

        template <class O, class T>
        struct out_value_result
        {
            O out;
            T value;
        };

        // ************************************** algorithms ******************************************

        template <class I, class O>
        using move_result = RAH2_NS::ranges::in_out_result<I, O>;

        namespace niebloids
        {
            struct move_fn
            {
                template <
                    typename I,
                    typename S,
                    typename O,
                    RAH2_STD::enable_if_t<
                        RAH2_NS::input_iterator<I> && RAH2_NS::sentinel_for<S, I>
                        && RAH2_NS::weakly_incrementable<O>>* = nullptr>
                constexpr RAH2_NS::ranges::move_result<I, O> operator()(I first, S last, O result) const
                {
                    for (; first != last; ++first, ++result)
                        *result = RAH2_NS::ranges::iter_move(first);
                    return {RAH2_STD::move(first), RAH2_STD::move(result)};
                }
                template <
                    typename R,
                    typename O,
                    RAH2_STD::enable_if_t<
                        RAH2_NS::ranges::input_range<R> && RAH2_NS::weakly_incrementable<O>>* = nullptr>
                constexpr RAH2_NS::ranges::move_result<RAH2_NS::ranges::borrowed_iterator_t<R>, O>
                operator()(R&& r, O result) const
                {
                    return (*this)(
                        RAH2_NS::ranges::begin(r), RAH2_NS::ranges::end(r), RAH2_STD::move(result));
                }
            };
        } // namespace niebloids
        constexpr niebloids::move_fn move{};

        template <class I, class O>
        using copy_result = RAH2_NS::ranges::in_out_result<I, O>;

        namespace niebloids
        {
            struct copy_fn
            {
                template <typename I, typename S, typename O>
                constexpr RAH2_NS::ranges::copy_result<I, O> operator()(I first, S last, O result) const
                {
                    for (; first != last; ++first, (void)++result)
                        *result = *first;
                    return {RAH2_STD::move(first), RAH2_STD::move(result)};
                }

                template <typename R, typename O>
                RAH2_NS::ranges::copy_result<RAH2_NS::ranges::iterator_t<R>, O>
                operator()(R&& r, O result) const
                {
                    return (*this)(
                        RAH2_NS::ranges::begin(r), RAH2_NS::ranges::end(r), RAH2_STD::move(result));
                }
            };
        } // namespace niebloids
        constexpr niebloids::copy_fn copy;

        template <class I, class O>
        using copy_if_result = RAH2_NS::ranges::in_out_result<I, O>;

        namespace niebloids
        {
            struct copy_if_fn
            {
                template <
                    typename I,
                    typename S,
                    typename O,
                    class Proj = RAH2_NS::details::identity,
                    typename Pred,
                    RAH2_STD::enable_if_t<input_iterator<I> && sentinel_for<S, I>>* = nullptr>
                constexpr RAH2_NS::ranges::copy_if_result<I, O>
                operator()(I first, S last, O result, Pred pred, Proj proj = {}) const
                {
                    for (; first != last; ++first)
                        if (pred(proj(*first)))
                        {
                            *result = *first;
                            ++result;
                        }
                    return {RAH2_STD::move(first), RAH2_STD::move(result)};
                }

                template <
                    typename R,
                    typename O,
                    class Proj = RAH2_NS::details::identity,
                    typename Pred,
                    RAH2_STD::enable_if_t<input_range<R>>* = nullptr>
                constexpr RAH2_NS::ranges::copy_if_result<RAH2_NS::ranges::iterator_t<R>, O>
                operator()(R&& r, O result, Pred pred, Proj proj = {}) const
                {
                    return (*this)(
                        RAH2_NS::ranges::begin(r),
                        RAH2_NS::ranges::end(r),
                        RAH2_STD::move(result),
                        RAH2_STD::ref(pred),
                        RAH2_STD::ref(proj));
                }
            };
        } // namespace niebloids
        constexpr niebloids::copy_if_fn copy_if;

        namespace niebloids
        {
            struct min_element
            {
                template <
                    typename ForwardIterator,
                    typename Sentinel,
                    RAH2_STD::enable_if_t<
                        forward_iterator<ForwardIterator> && sentinel_for<Sentinel, ForwardIterator>>* = nullptr>
                ForwardIterator operator()(ForwardIterator first, Sentinel last) const
                {
                    if (first != last)
                    {
                        ForwardIterator currentMin = first;

                        while (++first != last)
                        {
                            if (*first < *currentMin)
                                currentMin = first;
                        }
                        return currentMin;
                    }
                    return first;
                }

                template <typename ForwardRange>
                auto operator()(ForwardRange&& range) const
                {
                    return (*this)(RAH2_NS::ranges::begin(range), RAH2_NS::ranges::end(range));
                }

                template <typename ForwardIterator, typename ForwardSentinel, typename Compare>
                ForwardIterator
                operator()(ForwardIterator first, ForwardSentinel last, Compare compare) const
                {
                    if (first != last)
                    {
                        ForwardIterator currentMin = first;

                        while (++first != last)
                        {
                            if (compare(*first, *currentMin))
                                currentMin = first;
                        }
                        return currentMin;
                    }
                    return first;
                }

                template <
                    typename ForwardRange,
                    typename Compare,
                    RAH2_STD::enable_if_t<forward_range<ForwardRange>>* = nullptr>
                auto operator()(ForwardRange&& range, Compare compare) const
                {
                    return (*this)(
                        RAH2_NS::ranges::begin(range),
                        RAH2_NS::ranges::end(range),
                        RAH2_STD::move(compare));
                }
            };
        } // namespace niebloids

        /// min_element
        ///
        /// min_element finds the smallest element in the range [first, last).
        /// It returns the first iterator i in [first, last) such that no other
        /// iterator in [first, last) points to a value smaller than *i.
        /// The return value is last if and only if [first, last) is an empty range.
        ///
        /// Returns: The first iterator i in the range [first, last) such that
        /// for any iterator j in the range [first, last) the following corresponding
        /// condition holds: !(*j < *i).
        ///
        /// Complexity: Exactly 'max((last - first) - 1, 0)' applications of the
        /// corresponding comparisons.
        ///
        constexpr niebloids::min_element min_element;

        namespace niebloids
        {
            struct max_element
            {
                /// max_element
                ///
                /// max_element finds the largest element in the range [first, last).
                /// It returns the first iterator i in [first, last) such that no other
                /// iterator in [first, last) points to a value greater than *i.
                /// The return value is last if and only if [first, last) is an empty range.
                ///
                /// Returns: The first iterator i in the range [first, last) such that
                /// for any iterator j in the range [first, last) the following corresponding
                /// condition holds: !(*i < *j).
                ///
                /// Complexity: Exactly 'max((last - first) - 1, 0)' applications of the
                /// corresponding comparisons.
                ///
                template <
                    typename ForwardIterator,
                    typename ForwardSentinel,
                    RAH2_STD::enable_if_t<
                        forward_iterator<ForwardIterator>
                        && sentinel_for<ForwardSentinel, ForwardIterator>>* = nullptr>
                ForwardIterator operator()(ForwardIterator first, ForwardSentinel last) const
                {
                    if (first != last)
                    {
                        ForwardIterator currentMax = first;

                        while (++first != last)
                        {
                            if (*currentMax < *first)
                                currentMax = first;
                        }
                        return currentMax;
                    }
                    return first;
                }

                template <typename ForwardRange>
                iterator_t<ForwardRange> operator()(ForwardRange&& range) const
                {
                    return (*this)(RAH2_NS::ranges::begin(range), RAH2_NS::ranges::end(range));
                }

                /// max_element
                ///
                /// max_element finds the largest element in the range [first, last).
                /// It returns the first iterator i in [first, last) such that no other
                /// iterator in [first, last) points to a value greater than *i.
                /// The return value is last if and only if [first, last) is an empty range.
                ///
                /// Returns: The first iterator i in the range [first, last) such that
                /// for any iterator j in the range [first, last) the following corresponding
                /// condition holds: compare(*i, *j) == false.
                ///
                /// Complexity: Exactly 'max((last - first) - 1, 0)' applications of the
                /// corresponding comparisons.
                ///
                template <typename ForwardIterator, typename ForwardSentinel, typename Compare>
                ForwardIterator
                operator()(ForwardIterator first, ForwardSentinel last, Compare compare) const
                {
                    if (first != last)
                    {
                        ForwardIterator currentMax = first;

                        while (++first != last)
                        {
                            if (compare(*currentMax, *first))
                                currentMax = first;
                        }
                        return currentMax;
                    }
                    return first;
                }

                template <
                    typename ForwardRange,
                    typename Compare,
                    RAH2_STD::enable_if_t<forward_range<ForwardRange>>* = nullptr>
                iterator_t<ForwardRange> operator()(ForwardRange&& range, Compare compare) const
                {
                    return (*this)(
                        RAH2_NS::ranges::begin(range),
                        RAH2_NS::ranges::end(range),
                        RAH2_STD::move(compare));
                }
            };
        } // namespace niebloids
        constexpr niebloids::max_element max_element;

        template <class I>
        using minmax_element_result = RAH2_NS::ranges::min_max_result<I>;

        namespace niebloids
        {
            struct minmax_element
            {
                /// minmax_element
                ///
                /// Returns: make_pair(first, first) if [first, last) is empty, otherwise make_pair(m, M),
                /// where m is the first iterator in [first,last) such that no iterator in the range
                /// refers to a smaller element, and where M is the last iterator in [first,last) such
                /// that no iterator in the range refers to a larger element.
                ///
                /// Complexity: At most max([(3/2)*(N - 1)], 0) applications of the corresponding predicate,
                /// where N is distance(first, last).
                ///
                template <
                    typename ForwardIterator,
                    typename ForwardSentinel,
                    typename Compare = RAH2_NS::ranges::less,
                    RAH2_STD::enable_if_t<
                        forward_iterator<ForwardIterator>
                        && sentinel_for<ForwardSentinel, ForwardIterator>>* = nullptr>
                constexpr minmax_element_result<ForwardIterator>
                operator()(ForwardIterator first, ForwardSentinel last, Compare compare = {}) const
                {
                    minmax_element_result<ForwardIterator> result{first, first};

                    if (!(first == last) && !(++first == last))
                    {
                        if (compare(*first, *result.min))
                        {
                            result.max = result.min;
                            result.min = first;
                        }
                        else
                            result.max = first;

                        while (++first != last)
                        {
                            ForwardIterator i = first;

                            if (++first == last)
                            {
                                if (compare(*i, *result.min))
                                    result.min = i;
                                else if (!compare(*i, *result.max))
                                    result.max = i;
                                break;
                            }
                            else
                            {
                                if (compare(*first, *i))
                                {
                                    if (compare(*first, *result.min))
                                        result.min = first;

                                    if (!compare(*i, *result.max))
                                        result.max = i;
                                }
                                else
                                {
                                    if (compare(*i, *result.min))
                                        result.min = i;

                                    if (!compare(*first, *result.max))
                                        result.max = first;
                                }
                            }
                        }
                    }

                    return result;
                }

                template <
                    typename ForwardRange,
                    typename Compare = RAH2_NS::ranges::less,
                    RAH2_STD::enable_if_t<forward_range<ForwardRange>>* = nullptr>
                constexpr minmax_element_result<borrowed_iterator_t<ForwardRange>>
                operator()(ForwardRange&& range, Compare compare = {}) const
                {
                    return (*this)(
                        RAH2_NS::ranges::begin(range),
                        RAH2_NS::ranges::end(range),
                        RAH2_STD::move(compare));
                }
            };
        } // namespace niebloids
        constexpr niebloids::minmax_element minmax_element;

        namespace niebloids
        {
            struct median
            {
                template <typename T>
                T&& median_impl(T&& a, T&& b, T&& c) const
                {
                    if (RAH2_STD::less<T>()(a, b))
                    {
                        if (RAH2_STD::less<T>()(b, c))
                            return RAH2_STD::forward<T>(b);
                        else if (RAH2_STD::less<T>()(a, c))
                            return RAH2_STD::forward<T>(c);
                        else
                            return RAH2_STD::forward<T>(a);
                    }
                    else if (RAH2_STD::less<T>()(a, c))
                        return RAH2_STD::forward<T>(a);
                    else if (RAH2_STD::less<T>()(b, c))
                        return RAH2_STD::forward<T>(c);
                    return RAH2_STD::forward<T>(b);
                }

                template <typename T, typename Compare>
                T&& median_impl(T&& a, T&& b, T&& c, Compare compare) const
                {
                    if (compare(a, b))
                    {
                        if (compare(b, c))
                            return RAH2_STD::forward<T>(b);
                        else if (compare(a, c))
                            return RAH2_STD::forward<T>(c);
                        else
                            return RAH2_STD::forward<T>(a);
                    }
                    else if (compare(a, c))
                        return RAH2_STD::forward<T>(a);
                    else if (compare(b, c))
                        return RAH2_STD::forward<T>(c);
                    return RAH2_STD::forward<T>(b);
                }

                /// median
                ///
                /// median finds which element of three (a, b, d) is in-between the other two.
                /// If two or more elements are equal, the first (e.g. a before b) is chosen.
                ///
                /// Complexity: Either two or three comparisons will be required, depending
                /// on the values.
                ///
                template <typename T>
                T const& operator()(T const& a, T const& b, T const& c) const
                {
                    return median_impl(a, b, c);
                }

                /// median
                ///
                /// median finds which element of three (a, b, d) is in-between the other two.
                /// If two or more elements are equal, the first (e.g. a before b) is chosen.
                ///
                /// Complexity: Either two or three comparisons will be required, depending
                /// on the values.
                ///
                template <typename T>
                T&& operator()(T&& a, T&& b, T&& c) const
                {
                    return RAH2_STD::forward<T>(median_impl(
                        RAH2_STD::forward<T>(a), RAH2_STD::forward<T>(b), RAH2_STD::forward<T>(c)));
                }

                /// median
                ///
                /// median finds which element of three (a, b, d) is in-between the other two.
                /// If two or more elements are equal, the first (e.g. a before b) is chosen.
                ///
                /// Complexity: Either two or three comparisons will be required, depending
                /// on the values.
                ///
                template <typename T, typename Compare>
                T const& operator()(T const& a, T const& b, T const& c, Compare compare) const
                {
                    return median_impl<T const&, Compare>(a, b, c, compare);
                }

                /// median
                ///
                /// median finds which element of three (a, b, d) is in-between the other two.
                /// If two or more elements are equal, the first (e.g. a before b) is chosen.
                ///
                /// Complexity: Either two or three comparisons will be required, depending
                /// on the values.
                ///
                template <typename T, typename Compare>
                T&& operator()(T&& a, T&& b, T&& c, Compare compare) const
                {
                    return RAH2_STD::forward<T>(median_impl<T&&, Compare>(
                        RAH2_STD::forward<T>(a),
                        RAH2_STD::forward<T>(b),
                        RAH2_STD::forward<T>(c),
                        compare));
                }
            };
        } // namespace niebloids
        constexpr niebloids::median median;

        namespace niebloids
        {
            struct all_of
            {
            private:
                template <
                    typename I,
                    typename S,
                    typename Predicate,
                    RAH2_STD::enable_if_t<not sized_sentinel_for<S, I>>* = nullptr>
                RAH2_CONSTEXPR20 bool impl(I first, S last, Predicate pred) const
                {
                    while (first != last && pred(*first))
                    {
                        ++first;
                    }
                    return first == last;
                }

                /// This is an overload used by find algos for the RAI case.
                /// TODO : Use factorize with find_if(_not)
                template <
                    typename S,
                    typename I,
                    typename Predicate,
                    RAH2_STD::enable_if_t<sized_sentinel_for<S, I>>* = nullptr>
                RAH2_CONSTEXPR20 bool impl(I first, S last, Predicate pred) const
                {
                    typename RAH2_STD::iterator_traits<I>::difference_type trip_count =
                        (last - first) >> 2;

                    for (; trip_count > 0; --trip_count)
                    {
                        if (!pred(*first))
                            return false;
                        ++first;

                        if (!pred(*first))
                            return false;
                        ++first;

                        if (!pred(*first))
                            return false;
                        ++first;

                        if (!pred(*first))
                            return false;
                        ++first;
                    }

                    switch (last - first)
                    {
                    case 3:
                        if (!pred(*first))
                            return false;
                        ++first;
                        RAH2_FALLTHROUGH;
                    case 2:
                        if (!pred(*first))
                            return false;
                        ++first;
                        RAH2_FALLTHROUGH;
                    case 1:
                        if (!pred(*first))
                            return false;
                        ++first;
                        RAH2_FALLTHROUGH;
                    case 0:
                    default: return true;
                    }
                }

            public:
                /// all_of
                ///
                /// Returns: true if the unary predicate p returns true for all elements in the range [first, last)
                ///
                template <
                    typename InputIterator,
                    typename InputSentinel,
                    typename Predicate,
                    typename Proj = RAH2_NS::details::identity,
                    RAH2_STD::enable_if_t<
                        RAH2_NS::input_iterator<InputIterator>
                        && RAH2_NS::sentinel_for<InputSentinel, InputIterator>>* = nullptr>
                RAH2_CONSTEXPR20 bool operator()(
                    InputIterator first_w, InputSentinel last_w, Predicate pred, Proj proj = {}) const
                {
                    auto first_last =
                        details::unwrap(RAH2_STD::move(first_w), RAH2_STD::move(last_w));
                    return impl(
                        RAH2_STD::move(first_last.iterator),
                        RAH2_STD::move(first_last.sentinel),
                        details::wrap_pred_proj(RAH2_STD::move(pred), RAH2_STD::move(proj)));
                }

                template <
                    typename InputRange,
                    typename Predicate,
                    typename Proj = RAH2_NS::details::identity,
                    RAH2_STD::enable_if_t<RAH2_NS::ranges::input_range<InputRange>>* = nullptr>
                RAH2_CONSTEXPR20 bool operator()(InputRange&& range, Predicate pred, Proj proj = {}) const
                {
                    return (*this)(
                        RAH2_NS::ranges::begin(range),
                        RAH2_NS::ranges::end(range),
                        RAH2_STD::move(pred),
                        RAH2_STD::move(proj));
                }
            };
        } // namespace niebloids
        constexpr niebloids::all_of all_of;

        namespace niebloids
        {
            struct any_of
            {
            private:
                template <
                    typename I,
                    typename S,
                    typename Predicate,
                    RAH2_STD::enable_if_t<not sized_sentinel_for<S, I>>* = nullptr>
                RAH2_CONSTEXPR20 bool impl(I first, S last, Predicate p) const
                {
                    for (; first != last; ++first)
                    {
                        if (p(*first))
                            return true;
                    }
                    return false;
                }

                /// This is an overload used by find algos for the RAI case.
                /// TODO : Use factorize with find_if(_not)
                template <
                    typename I,
                    typename S,
                    typename Predicate,
                    RAH2_STD::enable_if_t<sized_sentinel_for<S, I>>* = nullptr>
                RAH2_CONSTEXPR20 bool impl(I first, S last, Predicate pred) const
                {
                    typename RAH2_STD::iterator_traits<I>::difference_type trip_count =
                        (last - first) >> 2;

                    for (; trip_count > 0; --trip_count)
                    {
                        if (pred(*first))
                            return true;
                        ++first;

                        if (pred(*first))
                            return true;
                        ++first;

                        if (pred(*first))
                            return true;
                        ++first;

                        if (pred(*first))
                            return true;
                        ++first;
                    }

                    switch (last - first)
                    {
                    case 3:
                        if (pred(*first))
                            return true;
                        ++first;
                        RAH2_FALLTHROUGH;
                    case 2:
                        if (pred(*first))
                            return true;
                        ++first;
                        RAH2_FALLTHROUGH;
                    case 1:
                        if (pred(*first))
                            return true;
                        ++first;
                        RAH2_FALLTHROUGH;
                    case 0:
                    default: return false;
                    }
                }

            public:
                /// any_of
                ///
                /// Returns: true if the unary predicate p returns true for any of the elements in the range [first, last)
                ///
                template <
                    typename InputIterator,
                    typename InputSentinel,
                    typename Predicate,
                    typename Proj = RAH2_NS::details::identity,
                    RAH2_STD::enable_if_t<
                        RAH2_NS::input_iterator<InputIterator>
                        && RAH2_NS::sentinel_for<InputSentinel, InputIterator>>* = nullptr>
                bool operator()(
                    InputIterator first_w, InputSentinel last_w, Predicate pred, Proj proj = {}) const
                {
                    auto first_last =
                        details::unwrap(RAH2_STD::move(first_w), RAH2_STD::move(last_w));
                    return impl(
                        RAH2_STD::move(first_last.iterator),
                        RAH2_STD::move(first_last.sentinel),
                        details::wrap_pred_proj(RAH2_STD::move(pred), RAH2_STD::move(proj)));
                }

                template <
                    typename InputRange,
                    typename Predicate,
                    typename Proj = RAH2_NS::details::identity,
                    RAH2_STD::enable_if_t<RAH2_NS::ranges::input_range<InputRange>>* = nullptr>
                bool operator()(InputRange&& range, Predicate pred, Proj proj = {}) const
                {
                    return (*this)(
                        RAH2_NS::ranges::begin(range),
                        RAH2_NS::ranges::end(range),
                        RAH2_STD::move(pred),
                        RAH2_STD::move(proj));
                }
            };
        } // namespace niebloids
        constexpr niebloids::any_of any_of;

        namespace niebloids
        {
            struct none_of
            {
            public:
                template <
                    typename InputIterator,
                    typename InputSentinel,
                    typename Predicate,
                    typename Proj = RAH2_NS::details::identity,
                    RAH2_STD::enable_if_t<
                        input_iterator<InputIterator> && sentinel_for<InputSentinel, InputIterator>>* = nullptr>
                bool operator()(
                    InputIterator first, InputSentinel last, Predicate pred, Proj proj = {}) const
                {
                    return not RAH2_NS::ranges::any_of(
                        RAH2_STD::move(first),
                        RAH2_STD::move(last),
                        RAH2_STD::move(pred),
                        RAH2_STD::move(proj));
                }

                template <
                    typename InputRange,
                    typename Predicate,
                    typename Proj = RAH2_NS::details::identity,
                    RAH2_STD::enable_if_t<input_range<InputRange>>* = nullptr>
                bool operator()(InputRange&& range, Predicate pred, Proj proj = {}) const
                {
                    return not RAH2_NS::ranges::any_of(
                        RAH2_STD::forward<InputRange>(range),
                        RAH2_STD::move(pred),
                        RAH2_STD::move(proj));
                }
            };
        } // namespace niebloids
        constexpr niebloids::none_of none_of;

        namespace niebloids
        {
            struct adjacent_find_fn
            {
                template <
                    typename I,
                    typename S,
                    class Proj = RAH2_NS::details::identity,
                    typename Pred = RAH2_NS::ranges::equal_to,
                    RAH2_STD::enable_if_t<forward_iterator<I> && sentinel_for<S, I>>* = nullptr>
                constexpr I operator()(I first, S last, Pred pred = {}, Proj proj = {}) const
                {
                    if (first == last)
                        return first;
                    auto next = RAH2_NS::ranges::next(first);
                    for (; next != last; ++next, ++first)
                    {
                        if (pred(proj(*first), proj(*next)))
                            return first;
                    }
                    return next;
                }

                template <
                    typename R,
                    class Proj = RAH2_NS::details::identity,
                    typename Pred = RAH2_NS::ranges::equal_to,
                    RAH2_STD::enable_if_t<forward_range<R>>* = nullptr>
                constexpr RAH2_NS::ranges::borrowed_iterator_t<R>
                operator()(R&& r, Pred pred = {}, Proj proj = {}) const
                {
                    return (*this)(
                        RAH2_NS::ranges::begin(r),
                        RAH2_NS::ranges::end(r),
                        RAH2_STD::ref(pred),
                        RAH2_STD::ref(proj));
                }
            };
        } // namespace niebloids
        /// adjacent_find
        ///
        /// Returns: The first iterator i such that both i and i + 1 are in the range
        /// [first, last) for which the following corresponding conditions hold: *i == *(i + 1).
        /// Returns last if no such iterator is found.
        ///
        /// Complexity: Exactly 'find(first, last, value) - first' applications of the corresponding predicate.
        ///
        constexpr niebloids::adjacent_find_fn adjacent_find;

        template <class I, class O>
        using copy_n_result = in_out_result<I, O>;

        namespace niebloids
        {
            struct copy_n_fn
            {
                template <
                    typename I,
                    typename O,
                    RAH2_STD::enable_if_t<
                        RAH2_NS::input_iterator<I> && !RAH2_NS::random_access_iterator<I>
                        && RAH2_NS::weakly_incrementable<O>>* = nullptr>
                constexpr RAH2_NS::ranges::copy_n_result<I, O>
                operator()(I first, iter_difference_t<I> n, O result) const
                {
                    for (RAH2_NS::iter_difference_t<I> i{}; i != n; ++i, ++first, ++result)
                        *result = *first;
                    return {RAH2_STD::move(first), RAH2_STD::move(result)};
                }

                template <
                    typename I,
                    typename O,
                    RAH2_STD::enable_if_t<
                        RAH2_NS::input_iterator<I> && RAH2_NS::random_access_iterator<I>
                        && RAH2_NS::weakly_incrementable<O>>* = nullptr>
                constexpr RAH2_NS::ranges::copy_n_result<I, O>
                operator()(I first, iter_difference_t<I> n, O result) const
                {
                    return RAH2_NS::ranges::copy(first, first + n, RAH2_STD::move(result));
                }
            };
        } // namespace niebloids
        /// copy_n
        ///
        /// Same as copy(InputIterator, InputIterator, OutputIterator) except based on count instead of iterator range.
        /// Effects: Copies exactly count values from the range beginning at first to the range beginning at result, if count > 0. Does nothing otherwise.
        /// Returns: Iterator in the destination range, pointing past the last element copied if count>0 or first otherwise.
        /// Complexity: Exactly count assignments, if count > 0.
        ///
        constexpr niebloids::copy_n_fn copy_n{};

        template <class I, class O>
        using move_backward_result = RAH2_NS::ranges::in_out_result<I, O>;

        namespace niebloids
        {
            struct move_backward_fn
            {
                template <
                    typename I1,
                    typename S1,
                    typename I2,
                    RAH2_STD::enable_if_t<
                        RAH2_NS::bidirectional_iterator<I1> && RAH2_NS::sentinel_for<S1, I1>
                        && RAH2_NS::bidirectional_iterator<I2>>* = nullptr>
                constexpr RAH2_NS::ranges::move_backward_result<I1, I2>
                operator()(I1 first, S1 last, I2 result) const
                {
                    auto last2 = RAH2_NS::ranges::next(first, last);
                    auto i = last2;
                    for (; i != first; *--result = RAH2_NS::ranges::iter_move(--i))
                    {
                    }
                    return {RAH2_STD::move(last2), RAH2_STD::move(result)};
                }

                template <
                    typename R,
                    typename I,
                    RAH2_STD::enable_if_t<
                        RAH2_NS::ranges::bidirectional_range<R> && RAH2_NS::bidirectional_iterator<I>>* = nullptr>
                constexpr RAH2_NS::ranges::move_backward_result<RAH2_NS::ranges::borrowed_iterator_t<R>, I>
                operator()(R&& r, I result) const
                {
                    return (*this)(
                        RAH2_NS::ranges::begin(r), RAH2_NS::ranges::end(r), RAH2_STD::move(result));
                }
            };
        } // namespace niebloids
        /// move_backward
        ///
        /// The elements are moved in reverse order (the last element is moved first), but their relative order is preserved.
        /// After this operation the elements in the moved-from range will still contain valid values of the
        /// appropriate type, but not necessarily the same values as before the move.
        /// Returns the beginning of the result range.
        /// Note: When moving between containers, the dest range must be valid; this function doesn't resize containers.
        /// Note: If result is within [first, last), move must be used instead of move_backward.
        ///
        /// Example usage:
        ///     RAH2_NS::move_backward(myArray.begin(), myArray.end(), myDestArray.end());
        ///
        /// Reference implementation:
        ///     template <typename BidirectionalIterator1, typename Sentinel1, typename BidirectionalIterator2 >
        ///     BidirectionalIterator2 move_backward(BidirectionalIterator1 first, Sentinel1 last, BidirectionalIterator2 resultEnd)
        ///     {
        ///         while(last != first)
        ///             *--resultEnd = RAH2_STD::move(*--last);
        ///         return resultEnd;
        ///     }
        constexpr niebloids::move_backward_fn move_backward{};

        template <class I1, class I2>
        using copy_backward_result = in_out_result<I1, I2>;

        namespace niebloids
        {
            struct copy_backward_fn
            {
                template <
                    typename I1,
                    typename S1,
                    typename I2,
                    RAH2_STD::enable_if_t<
                        RAH2_NS::bidirectional_iterator<I1> && RAH2_NS::sentinel_for<S1, I1>
                        && RAH2_NS::bidirectional_iterator<I2>>* = nullptr>
                constexpr copy_backward_result<I1, I2> operator()(I1 first, S1 last, I2 result) const
                {
                    I1 last1{RAH2_NS::ranges::next(first, RAH2_STD::move(last))};
                    for (I1 i{last1}; i != first;)
                        *--result = *--i;
                    return {RAH2_STD::move(last1), RAH2_STD::move(result)};
                }

                template <
                    typename R,
                    typename I,
                    RAH2_STD::enable_if_t<bidirectional_range<R> && bidirectional_iterator<I>>* = nullptr>
                constexpr copy_backward_result<borrowed_iterator_t<R>, I> operator()(R&& r, I result) const
                {
                    return (*this)(
                        RAH2_NS::ranges::begin(r), RAH2_NS::ranges::end(r), RAH2_STD::move(result));
                }
            };
        } // namespace niebloids

        /// copy_backward
        ///
        /// copies memory in the range of [first, last) to the range *ending* with result.
        ///
        /// Effects: Copies elements in the range [first, last) into the range
        /// [result - (last - first), result) starting from last 1 and proceeding to first.
        /// For each positive integer n <= (last - first), performs *(result n) = *(last - n).
        ///
        /// Requires: result shall not be in the range [first, last).
        ///
        /// Returns: result - (last - first). That is, returns the beginning of the result range.
        ///
        /// Complexity: Exactly 'last - first' assignments.
        ///
        constexpr niebloids::copy_backward_fn copy_backward{};

        namespace niebloids
        {
            struct count_fn
            {
                template <
                    typename I,
                    typename S,
                    class T,
                    class Proj = RAH2_NS::details::identity,
                    RAH2_STD::enable_if_t<input_iterator<I> && sentinel_for<S, I>>* = nullptr>
                constexpr RAH2_NS::iter_difference_t<I>
                operator()(I first_w, S last_w, T const& value, Proj proj) const
                {
                    auto first_last =
                        details::unwrap(RAH2_STD::move(first_w), RAH2_STD::move(last_w));
                    auto first = RAH2_STD::move(first_last.iterator);
                    auto last = RAH2_STD::move(first_last.sentinel);
                    RAH2_NS::iter_difference_t<I> counter = 0;
                    for (; first != last; ++first)
                    {
                        if (RAH2_INVOKE_1(proj, *first) == value)
                            ++counter;
                    }
                    return counter;
                }

                template <
                    typename R,
                    class T,
                    class Proj = RAH2_NS::details::identity,
                    RAH2_STD::enable_if_t<input_range<R>>* = nullptr>
                constexpr range_difference_t<R> operator()(R&& r, T const& value, Proj proj) const
                {
                    return (*this)(
                        RAH2_NS::ranges::begin(r),
                        RAH2_NS::ranges::end(r),
                        value,
                        RAH2_STD::move(proj));
                }

                template <
                    typename I,
                    typename S,
                    class T,
                    RAH2_STD::enable_if_t<input_iterator<I> && sentinel_for<S, I>>* = nullptr>
                constexpr RAH2_NS::iter_difference_t<I>
                operator()(I first_w, S last_w, T const& value) const
                {
                    auto first_last =
                        details::unwrap(RAH2_STD::move(first_w), RAH2_STD::move(last_w));
                    auto first = RAH2_STD::move(first_last.iterator);
                    auto last = RAH2_STD::move(first_last.sentinel);
                    RAH2_NS::iter_difference_t<I> counter = 0;
                    for (; first != last; ++first)
                        if (*first == value)
                            ++counter;
                    return counter;
                }

                template <typename R, class T, RAH2_STD::enable_if_t<input_range<R>>* = nullptr>
                constexpr range_difference_t<R> operator()(R&& r, T const& value) const
                {
                    return (*this)(RAH2_NS::ranges::begin(r), RAH2_NS::ranges::end(r), value);
                }
            };
        } // namespace niebloids

        /// count
        ///
        /// Counts the number of items in the range of [first, last) which equal the input value.
        ///
        /// Effects: Returns the number of iterators i in the range [first, last) for which the
        /// following corresponding conditions hold: *i == value.
        ///
        /// Complexity: At most 'last - first' applications of the corresponding predicate.
        ///
        /// Note: The predicate version of count is count_if and not another variation of count.
        /// This is because both versions would have three parameters and there could be ambiguity.
        ///
        constexpr niebloids::count_fn count;

        namespace niebloids
        {
            struct count_if_fn
            {
            private:
                template <
                    typename I,
                    typename S,
                    typename Pred,
                    RAH2_STD::enable_if_t<input_iterator<I> && sentinel_for<S, I>>* = nullptr>
                RAH2_NODISCARD RAH2_CONSTEXPR20 RAH2_NS::iter_difference_t<I>
                impl(I first, S last, Pred pred) const
                {
                    RAH2_NS::iter_difference_t<I> counter = 0;
                    for (; first != last; ++first)
                    {
                        if (pred(*first))
                            ++counter;
                    }
                    return counter;
                }

            public:
                template <
                    typename I,
                    typename S,
                    class Proj = RAH2_NS::details::identity,
                    typename Pred,
                    RAH2_STD::enable_if_t<input_iterator<I> && sentinel_for<S, I>>* = nullptr>
                RAH2_NODISCARD RAH2_CONSTEXPR20 RAH2_NS::iter_difference_t<I>
                operator()(I first_w, S last_w, Pred pred, Proj proj = {}) const
                {
                    auto first_last =
                        details::unwrap(RAH2_STD::move(first_w), RAH2_STD::move(last_w));
                    auto first = RAH2_STD::move(first_last.iterator);
                    auto last = RAH2_STD::move(first_last.sentinel);
                    return impl(
                        first,
                        last,
                        details::wrap_pred_proj(RAH2_STD::move(pred), RAH2_STD::move(proj)));
                }

                template <
                    typename R,
                    class Proj = RAH2_NS::details::identity,
                    typename Pred,
                    RAH2_STD::enable_if_t<input_range<R>>* = nullptr>
                RAH2_NODISCARD RAH2_CONSTEXPR20 range_difference_t<R>
                operator()(R&& r, Pred pred, Proj proj = {}) const
                {
                    return (*this)(
                        RAH2_NS::ranges::begin(r),
                        RAH2_NS::ranges::end(r),
                        RAH2_STD::move(pred),
                        RAH2_STD::move(proj));
                }
            };
        } // namespace niebloids

        /// count_if
        ///
        /// Counts the number of items in the range of [first, last) which match
        /// the input value as defined by the input predicate function.
        ///
        /// Effects: Returns the number of iterators i in the range [first, last) for which the
        /// following corresponding conditions hold: predicate(*i) != false.
        ///
        /// Complexity: At most 'last - first' applications of the corresponding predicate.
        ///
        /// Note: The non-predicate version of count_if is count and not another variation of count_if.
        /// This is because both versions would have three parameters and there could be ambiguity.
        ///
        constexpr niebloids::count_if_fn count_if;

        namespace details
        {
            template <typename I, typename Diff, typename P>
            RAH2_CONSTEXPR20 I find_if_n(I first, Diff count, P pred)
            {
                for (; count > 3; count -= 4)
                {
                    if (pred(*first))
                        return first;
                    ++first;

                    if (pred(*first))
                        return first;
                    ++first;

                    if (pred(*first))
                        return first;
                    ++first;

                    if (pred(*first))
                        return first;
                    ++first;
                }

                switch (count)
                {
                case 3:
                    if (pred(*first))
                        return first;
                    ++first;
                    RAH2_FALLTHROUGH;
                case 2:
                    if (pred(*first))
                        return first;
                    ++first;
                    RAH2_FALLTHROUGH;
                case 1:
                    if (pred(*first))
                        return first;
                    ++first;
                    RAH2_FALLTHROUGH;
                case 0:
                default: return first;
                }
            }

            template <typename I, typename Diff, typename P>
            RAH2_CONSTEXPR20 I find_if_not_n(I first, Diff count, P pred)
            {
                for (; count > 3; count -= 4)
                {
                    if (!pred(*first))
                        return first;
                    ++first;

                    if (!pred(*first))
                        return first;
                    ++first;

                    if (!pred(*first))
                        return first;
                    ++first;

                    if (!pred(*first))
                        return first;
                    ++first;
                }

                switch (count)
                {
                case 3:
                    if (!pred(*first))
                        return first;
                    ++first;
                    RAH2_FALLTHROUGH;
                case 2:
                    if (!pred(*first))
                        return first;
                    ++first;
                    RAH2_FALLTHROUGH;
                case 1:
                    if (!pred(*first))
                        return first;
                    ++first;
                    RAH2_FALLTHROUGH;
                case 0:
                default: return first;
                }
            }
        } // namespace details
        namespace niebloids
        {
            struct find_fn
            {
            private:
                template <typename I, typename S, class T, class Proj>
                RAH2_CONSTEXPR20 I impl(I first, S last, T const& value, Proj proj = {}) const
                {
                    while ((first != last) && (proj(*first) != value))
                    {
                        ++first;
                    }
                    return first;
                }

            public:
                template <
                    typename I,
                    typename S,
                    class T,
                    class Proj = RAH2_NS::details::identity,
                    RAH2_STD::enable_if_t<
                        input_iterator<I> && sentinel_for<S, I> && not sized_sentinel_for<S, I>>* = nullptr>
                RAH2_CONSTEXPR20 I operator()(I first, S last, T const& value, Proj proj = {}) const
                {
                    auto first_last = details::unwrap(RAH2_STD::move(first), RAH2_STD::move(last));
                    auto iter = impl(
                        RAH2_STD::move(first_last.iterator),
                        RAH2_STD::move(first_last.sentinel),
                        value,
                        RAH2_NS::ranges::details::move_unary(proj));
                    return first_last.wrap_iterator(RAH2_STD::move(iter));
                }

                template <
                    typename I,
                    typename S,
                    class T,
                    class Proj = RAH2_NS::details::identity,
                    RAH2_STD::enable_if_t<input_iterator<I> && sized_sentinel_for<S, I>>* = nullptr>
                RAH2_CONSTEXPR20 I operator()(I first, S last, T const& value, Proj proj = {}) const
                {
                    auto const diff = last - first;
                    auto first_last = details::unwrap(RAH2_STD::move(first), RAH2_STD::move(last));
                    auto iter = details::find_if_n(
                        RAH2_STD::move(first_last.iterator),
                        diff,
                        details::wrap_pred_proj(
                            [&value](auto&& val) { return val == value; }, RAH2_STD::move(proj)));
                    return first_last.wrap_iterator(RAH2_STD::move(iter));
                }

                template <
                    typename R,
                    class T,
                    class Proj = RAH2_NS::details::identity,
                    RAH2_STD::enable_if_t<input_range<R> && not sized_range<R>>* = nullptr>
                RAH2_CONSTEXPR20 borrowed_iterator_t<R>
                operator()(R&& r, T const& value, Proj proj = {}) const
                {
                    return (*this)(
                        RAH2_NS::ranges::begin(r),
                        RAH2_NS::ranges::end(r),
                        value,
                        RAH2_STD::move(proj));
                }

                template <
                    typename R,
                    class T,
                    class Proj = RAH2_NS::details::identity,
                    RAH2_STD::enable_if_t<input_range<R> && sized_range<R>>* = nullptr>
                RAH2_CONSTEXPR20 borrowed_iterator_t<R>
                operator()(R&& r, T const& value, Proj proj = {}) const
                {
                    auto first_last =
                        details::unwrap(RAH2_NS::ranges::begin(r), RAH2_NS::ranges::end(r));
                    auto iter = details::find_if_n(
                        RAH2_STD::move(first_last.iterator),
                        RAH2_NS::ranges::size(r),
                        details::wrap_pred_proj(
                            [&value](auto&& val) { return val == value; }, RAH2_STD::move(proj)));
                    return first_last.wrap_iterator(RAH2_STD::move(iter));
                }
            };
        } // namespace niebloids
        constexpr niebloids::find_fn find;

        namespace niebloids
        {
            struct find_if_fn
            {
            private:
                template <typename I, typename S, class Pred>
                RAH2_CONSTEXPR20 I impl(I first, S last, Pred pred) const
                {
                    while ((first != last) && !pred(*first))
                    {
                        ++first;
                    }
                    return first;
                }

            public:
                template <
                    typename I,
                    typename S,
                    class Pred,
                    class Proj = RAH2_NS::details::identity,
                    RAH2_STD::enable_if_t<
                        input_iterator<I> && sentinel_for<S, I> && not sized_sentinel_for<S, I>>* = nullptr>
                RAH2_CONSTEXPR20 I operator()(I first, S last, Pred pred, Proj proj = {}) const
                {
                    auto first_last = details::unwrap(RAH2_STD::move(first), RAH2_STD::move(last));
                    auto iter = impl(
                        RAH2_STD::move(first_last.iterator),
                        RAH2_STD::move(first_last.sentinel),
                        details::wrap_pred_proj(RAH2_STD::move(pred), RAH2_STD::move(proj)));
                    return first_last.wrap_iterator(RAH2_STD::move(iter));
                }

                template <
                    typename I,
                    typename S,
                    class Pred,
                    class Proj = RAH2_NS::details::identity,
                    RAH2_STD::enable_if_t<input_iterator<I> && sized_sentinel_for<S, I>>* = nullptr>
                RAH2_CONSTEXPR20 I operator()(I first, S last, Pred pred, Proj proj = {}) const
                {
                    auto const diff = last - first;
                    auto first_last = details::unwrap(RAH2_STD::move(first), RAH2_STD::move(last));
                    auto iter = details::find_if_n(
                        RAH2_STD::move(first_last.iterator),
                        diff,
                        details::wrap_pred_proj(RAH2_STD::move(pred), RAH2_STD::move(proj)));
                    return first_last.wrap_iterator(RAH2_STD::move(iter));
                }

                template <
                    typename R,
                    class Pred,
                    class Proj = RAH2_NS::details::identity,
                    RAH2_STD::enable_if_t<input_range<R> && not sized_range<R>>* = nullptr>
                RAH2_CONSTEXPR20 borrowed_iterator_t<R> operator()(R&& r, Pred pred, Proj proj = {}) const
                {
                    return (*this)(
                        RAH2_NS::ranges::begin(r),
                        RAH2_NS::ranges::end(r),
                        RAH2_STD::move(pred),
                        RAH2_STD::move(proj));
                }

                template <
                    typename R,
                    class Pred,
                    class Proj = RAH2_NS::details::identity,
                    RAH2_STD::enable_if_t<input_range<R> && sized_range<R>>* = nullptr>
                RAH2_CONSTEXPR20 borrowed_iterator_t<R> operator()(R&& r, Pred pred, Proj proj = {}) const
                {
                    auto first_last =
                        details::unwrap(RAH2_NS::ranges::begin(r), RAH2_NS::ranges::end(r));
                    auto iter = details::find_if_n(
                        RAH2_STD::move(first_last.iterator),
                        RAH2_NS::ranges::size(r),
                        details::wrap_pred_proj(RAH2_STD::move(pred), RAH2_STD::move(proj)));
                    return first_last.wrap_iterator(RAH2_STD::move(iter));
                }
            };
        } // namespace niebloids

        /// find_if
        ///
        /// finds the value within the unsorted range of [first, last).
        ///
        /// Returns: The first iterator i in the range [first, last) for which
        /// the following corresponding conditions hold: pred(*i) != false.
        /// Returns last if no such iterator is found.
        /// If the sequence of elements to search for (i.e. first2 - last2) is empty,
        /// the find always fails and last1 will be returned.
        ///
        /// Complexity: At most 'last - first' applications of the corresponding predicate.
        ///
        /// Note: The non-predicate version of find_if is find and not another variation of find_if.
        /// This is because both versions would have three parameters and there could be ambiguity.
        constexpr niebloids::find_if_fn find_if;

        namespace niebloids
        {
            struct find_if_not_fn
            {
            private:
                template <typename I, typename S, class Pred>
                RAH2_CONSTEXPR20 I impl(I first, S last, Pred pred) const
                {
                    while ((first != last) && pred(*first))
                    {
                        ++first;
                    }
                    return first;
                }

            public:
                template <
                    typename I,
                    typename S,
                    class Pred,
                    class Proj = RAH2_NS::details::identity,
                    RAH2_STD::enable_if_t<
                        input_iterator<I> && sentinel_for<S, I> && not sized_sentinel_for<S, I>>* = nullptr>
                RAH2_CONSTEXPR20 I operator()(I first, S last, Pred pred, Proj proj = {}) const
                {
                    auto first_last = details::unwrap(RAH2_STD::move(first), RAH2_STD::move(last));
                    auto iter = impl(
                        RAH2_STD::move(first_last.iterator),
                        RAH2_STD::move(first_last.sentinel),
                        details::wrap_pred_proj(RAH2_STD::move(pred), RAH2_STD::move(proj)));
                    return first_last.wrap_iterator(RAH2_STD::move(iter));
                }

                template <
                    typename I,
                    typename S,
                    class Pred,
                    class Proj = RAH2_NS::details::identity,
                    RAH2_STD::enable_if_t<input_iterator<I> && sized_sentinel_for<S, I>>* = nullptr>
                RAH2_CONSTEXPR20 I operator()(I first, S last, Pred pred, Proj proj = {}) const
                {
                    auto const diff = last - first;
                    auto first_last = details::unwrap(RAH2_STD::move(first), RAH2_STD::move(last));
                    auto iter = details::find_if_not_n(
                        RAH2_STD::move(first_last.iterator),
                        diff,
                        details::wrap_pred_proj(RAH2_STD::move(pred), RAH2_STD::move(proj)));
                    return first_last.wrap_iterator(RAH2_STD::move(iter));
                }

                template <
                    typename R,
                    class Pred,
                    class Proj = RAH2_NS::details::identity,
                    RAH2_STD::enable_if_t<input_range<R> && not sized_range<R>>* = nullptr>
                RAH2_CONSTEXPR20 borrowed_iterator_t<R> operator()(R&& r, Pred pred, Proj proj = {}) const
                {
                    return (*this)(
                        RAH2_NS::ranges::begin(r),
                        RAH2_NS::ranges::end(r),
                        RAH2_STD::move(pred),
                        RAH2_STD::move(proj));
                }

                template <
                    typename R,
                    class Pred,
                    class Proj = RAH2_NS::details::identity,
                    RAH2_STD::enable_if_t<input_range<R> && sized_range<R>>* = nullptr>
                RAH2_CONSTEXPR20 borrowed_iterator_t<R> operator()(R&& r, Pred pred, Proj proj = {}) const
                {
                    auto first_last =
                        details::unwrap(RAH2_NS::ranges::begin(r), RAH2_NS::ranges::end(r));
                    auto iter = details::find_if_not_n(
                        RAH2_STD::move(first_last.iterator),
                        RAH2_NS::ranges::size(r),
                        details::wrap_pred_proj(RAH2_STD::move(pred), RAH2_STD::move(proj)));
                    return first_last.wrap_iterator(RAH2_STD::move(iter));
                }
            };
        } // namespace niebloids

        /// find_if_not
        ///
        /// find_if_not works the same as find_if except it tests for if the predicate
        /// returns false for the elements instead of true.
        ///
        constexpr niebloids::find_if_not_fn find_if_not;

        namespace niebloids
        {
            struct find_first_of_fn
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
                        input_iterator<I1> && sentinel_for<S1, I1> && forward_iterator<I2>
                        && sentinel_for<S2, I2>>* = nullptr>
                constexpr I1 operator()(
                    I1 first1,
                    S1 last1,
                    I2 first2,
                    S2 last2,
                    Pred pred = {},
                    Proj1 proj1 = {},
                    Proj2 proj2 = {}) const
                {
                    for (; first1 != last1; ++first1)
                        for (auto i = first2; i != last2; ++i)
                            if (RAH2_INVOKE_2(
                                    pred, (RAH2_INVOKE_1(proj1, *first1)), (RAH2_INVOKE_1(proj2, *i))))
                                return first1;
                    return first1;
                }

                template <
                    typename R1,
                    typename R2,
                    class Pred = RAH2_NS::ranges::equal_to,
                    class Proj1 = RAH2_NS::details::identity,
                    class Proj2 = RAH2_NS::details::identity,
                    RAH2_STD::enable_if_t<input_range<R1> && forward_range<R2>>* = nullptr>
                constexpr borrowed_iterator_t<R1>
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

        /// find_first_of
        ///
        /// find_first_of is similar to find in that it performs linear search through
        /// a range of ForwardIterators. The difference is that while find searches
        /// for one particular value, find_first_of searches for any of several values.
        /// Specifically, find_first_of searches for the first occurrance in the
        /// range [first1, last1) of any of the elements in [first2, last2).
        /// This function is thus similar to the strpbrk standard C string function.
        /// If the sequence of elements to search for (i.e. first2-last2) is empty,
        /// the find always fails and last1 will be returned.
        ///
        /// Effects: Finds an element that matches one of a set of values.
        ///
        /// Returns: The first iterator i in the range [first1, last1) such that for some
        /// integer j in the range [first2, last2) the following conditions hold: *i == *j.
        /// Returns last1 if no such iterator is found.
        ///
        /// Complexity: At most '(last1 - first1) * (last2 - first2)' applications of the
        /// corresponding predicate.
        ///
        constexpr niebloids::find_first_of_fn find_first_of{};

        template <class I, class F>
        using for_each_result = ranges::in_fun_result<I, F>;

        namespace niebloids
        {
            struct for_each
            {
            private:
                template <typename InputIterator, typename InputSentinel, typename Function>
                for_each_result<InputIterator, Function>
                impl(InputIterator first, InputSentinel last, Function function) const
                {
                    for (; first != last; ++first)
                        function(*first);
                    return {RAH2_STD::move(first), RAH2_STD::move(function)};
                }

            public:
                template <
                    typename InputIterator,
                    typename InputSentinel,
                    typename Function,
                    class Proj = RAH2_NS::details::identity,
                    RAH2_STD::enable_if_t<
                        RAH2_NS::input_iterator<InputIterator>
                        && RAH2_NS::sentinel_for<InputSentinel, InputIterator>>* = nullptr>
                for_each_result<InputIterator, Function> operator()(
                    InputIterator first_w, InputSentinel last_w, Function function, Proj proj = {}) const
                {
                    auto first_last =
                        details::unwrap(RAH2_STD::move(first_w), RAH2_STD::move(last_w));
                    auto result = impl(
                        RAH2_STD::move(first_last.iterator),
                        RAH2_STD::move(first_last.sentinel),
                        details::wrap_pred_proj(RAH2_STD::move(function), RAH2_STD::move(proj)));
                    return {
                        first_last.wrap_iterator(RAH2_STD::move(result.in)),
                        Function(RAH2_STD::move(result.fun))};
                }

                template <
                    typename InputRange,
                    typename Function,
                    class Proj = RAH2_NS::details::identity,
                    RAH2_STD::enable_if_t<RAH2_NS::ranges::input_range<InputRange>>* = nullptr>
                for_each_result<borrowed_iterator_t<InputRange>, Function>
                operator()(InputRange&& range, Function function, Proj proj = {}) const
                {
                    return (*this)(
                        RAH2_NS::ranges::begin(range),
                        RAH2_NS::ranges::end(range),
                        RAH2_STD::move(function),
                        RAH2_STD::move(proj));
                }
            };
        } // namespace niebloids
        constexpr niebloids::for_each for_each;

        template <class I, class F>
        using for_each_n_result = in_fun_result<I, F>;

        namespace niebloids
        {
            struct for_each_n_fn
            {
            private:
                template <typename I, typename Fun>
                RAH2_CONSTEXPR20 for_each_n_result<I, Fun>
                impl(I first, RAH2_NS::iter_difference_t<I> n, Fun fun) const
                {
                    for (; n-- > 0; ++first)
                        fun(*first);
                    return {RAH2_STD::move(first), RAH2_STD::move(fun)};
                }

            public:
                template <typename I, class Proj = RAH2_NS::details::identity, typename Fun>
                RAH2_CONSTEXPR20 for_each_n_result<I, Fun> operator()(
                    I first_w, RAH2_NS::iter_difference_t<I> n, Fun fun, Proj proj = Proj{}) const
                {
                    auto first_last = details::unwrap(RAH2_STD::move(first_w), nullptr);
                    auto result = impl(
                        RAH2_STD::move(first_last.iterator),
                        n,
                        details::wrap_pred_proj(RAH2_STD::move(fun), RAH2_STD::move(proj)));
                    return {
                        first_last.wrap_iterator(RAH2_STD::move(result.in)),
                        Fun(RAH2_STD::move(result.fun))};
                }
            };
        } // namespace niebloids

        /// for_each_n
        ///
        /// Calls the Function function for each value in the range [first, first + n).
        /// Function takes a single parameter: the current value.
        ///
        /// Effects: Applies function to the result of dereferencing every iterator in
        /// the range [first, first + n), starting from first and proceeding to last 1.
        ///
        /// Returns: first + n.
        ///
        /// Complexity: Applies function exactly 'first + n' times.
        ///
        /// Note:
        ////  * If function returns a result, the result is ignored.
        ////  * If n < 0, behaviour is undefined.
        ///
        constexpr niebloids::for_each_n_fn for_each_n{};

        namespace niebloids
        {
            struct generate_fn
            {
                template <typename O, typename S, typename F>
                constexpr O operator()(O first, S last, F gen) const
                {
                    for (; first != last; *first = RAH2_INVOKE_0(gen), ++first)
                    {
                    }
                    return first;
                }

                template <typename R, typename F>
                constexpr borrowed_iterator_t<R> operator()(R&& r, F gen) const
                {
                    return (*this)(
                        RAH2_NS::ranges::begin(r), RAH2_NS::ranges::end(r), RAH2_STD::move(gen));
                }
            };
        } // namespace niebloids

        /// generate
        ///
        /// Iterates the range of [first, last) and assigns to each element the
        /// result of the function generator. Generator is a function which takes
        /// no arguments.
        ///
        /// Complexity: Exactly 'last - first' invocations of generator and assignments.
        ///
        constexpr niebloids::generate_fn generate{};

        namespace niebloids
        {
            struct generate_n_fn
            {
                template <
                    typename O,
                    typename F,
                    RAH2_STD::enable_if_t<
                        RAH2_NS::input_or_output_iterator<O> && RAH2_NS::copy_constructible<F>>* = nullptr>
                constexpr O operator()(O first, RAH2_NS::iter_difference_t<O> n, F gen) const
                {
                    for (; n-- > 0; *first = gen(), ++first)
                    {
                    }
                    return first;
                }
            };
        } // namespace niebloids

        /// generate_n
        ///
        /// Iterates an interator n times and assigns the result of generator
        /// to each succeeding element. Generator is a function which takes
        /// no arguments.
        ///
        /// Complexity: Exactly n invocations of generator and assignments.
        ///
        constexpr niebloids::generate_n_fn generate_n{};

        template <class I, class O>
        using unary_transform_result = RAH2_NS::ranges::in_out_result<I, O>;

        template <class I1, class I2, class O>
        using binary_transform_result = RAH2_NS::ranges::in_in_out_result<I1, I2, O>;

        namespace niebloids
        {
            struct transform
            {

                /// transform
                ///
                /// Iterates the input range of [first, last) and the output iterator result
                /// and assigns the result of unaryOperation(input) to result.
                ///
                /// Effects: Assigns through every iterator i in the range [result, result + (last1 - first1))
                /// a new corresponding value equal to unaryOperation(*(first1 + (i - result)).
                ///
                /// Requires: op shall not have any side effects.
                ///
                /// Returns: result + (last1 - first1). That is, returns the end of the output range.
                ///
                /// Complexity: Exactly 'last1 - first1' applications of unaryOperation.
                ///
                /// Note: result may be equal to first.
                ///
                template <
                    typename InputIterator,
                    typename InputSentinel,
                    typename OutputIterator,
                    typename UnaryOperation,
                    RAH2_STD::enable_if_t<sentinel_for<InputSentinel, InputIterator>>* = nullptr>
                unary_transform_result<InputIterator, OutputIterator> operator()(
                    InputIterator first,
                    InputSentinel last,
                    OutputIterator result,
                    UnaryOperation unaryOperation) const
                {
                    for (; first != last; ++first, ++result)
                        *result = unaryOperation(*first);
                    return {first, result};
                }
                template <typename Range, typename OutputIterator, typename UnaryOperation>
                unary_transform_result<iterator_t<Range>, OutputIterator>
                operator()(Range&& range, OutputIterator result, UnaryOperation unaryOperation) const
                {
                    return (*this)(
                        RAH2_NS::ranges::begin(range),
                        RAH2_NS::ranges::end(range),
                        RAH2_STD::move(result),
                        RAH2_STD::move(unaryOperation));
                }

                /// transform
                ///
                /// Iterates the input range of [first, last) and the output iterator result
                /// and assigns the result of binaryOperation(input1, input2) to result.
                ///
                /// Effects: Assigns through every iterator i in the range [result, result + (last1 - first1))
                /// a new corresponding value equal to binaryOperation(*(first1 + (i - result), *(first2 + (i - result))).
                ///
                /// Requires: binaryOperation shall not have any side effects.
                ///
                /// Returns: result + (last1 - first1). That is, returns the end of the output range.
                ///
                /// Complexity: Exactly 'last1 - first1' applications of binaryOperation.
                ///
                /// Note: result may be equal to first1 or first2.
                ///
                template <
                    typename InputIterator1,
                    typename InputSentinel1,
                    typename InputIterator2,
                    typename InputSentinel2,
                    typename OutputIterator,
                    typename BinaryOperation,
                    RAH2_STD::enable_if_t<
                        sentinel_for<InputSentinel1, InputIterator1>
                        && sentinel_for<InputSentinel2, InputIterator2>>* = nullptr>
                OutputIterator operator()(
                    InputIterator1 first1,
                    InputSentinel1 last1,
                    InputIterator2 first2,
                    InputSentinel2 last2,
                    OutputIterator result,
                    BinaryOperation binaryOperation) const
                {
                    for (; (first1 != last1) && (first2 != last2); ++first1, ++first2, ++result)
                        *result = binaryOperation(*first1, *first2);
                    return result;
                }

                template <
                    typename InputRange1,
                    typename InputRange2,
                    typename OutputIterator,
                    typename BinaryOperation,
                    RAH2_STD::enable_if_t<input_range<InputRange1> && input_range<InputRange2>>* = nullptr>
                OutputIterator operator()(
                    InputRange1&& range1,
                    InputRange2 range2,
                    OutputIterator result,
                    BinaryOperation binaryOperation) const
                {
                    return (*this)(
                        RAH2_NS::ranges::begin(range1),
                        RAH2_NS::ranges::end(range1),
                        RAH2_NS::ranges::begin(range2),
                        RAH2_NS::ranges::end(range2),
                        RAH2_STD::move(result),
                        RAH2_STD::move(binaryOperation));
                }
            };
        } // namespace niebloids
        constexpr niebloids::transform transform;

        namespace niebloids
        {
            struct equal
            {

                /// equal
                ///
                /// Returns: true if for every iterator i in the range [first1, last1) the
                /// following corresponding conditions hold: predicate(*i, *(first2 + (i - first1))) != false.
                /// Otherwise, returns false.
                ///
                /// Complexity: At most last1 first1 applications of the corresponding predicate.
                ///
                /// To consider: Make specializations of this for scalar types and random access
                /// iterators that uses memcmp or some trick memory comparison function.
                /// We should verify that such a thing results in an improvement.
                ///
                template <
                    typename InputIterator1,
                    typename InputSentinel1,
                    typename InputIterator2,
                    typename InputSentinel2,
                    RAH2_STD::enable_if_t<
                        input_iterator<InputIterator1> && input_iterator<InputIterator2>
                        && sentinel_for<InputSentinel1, InputIterator1>
                        && sentinel_for<InputSentinel2, InputIterator2>>* = nullptr>
                constexpr bool operator()(
                    InputIterator1 first1,
                    InputSentinel1 last1,
                    InputIterator2 first2,
                    InputSentinel2 last2) const
                {
                    for (; first1 != last1; ++first1, ++first2)
                    {
                        if (!(*first1 == *first2)) // Note that we always express value comparisons in terms of < or ==.
                            return false;
                    }
                    return first2 == last2;
                }

                template <
                    typename InputRange1,
                    typename InputRange2,
                    RAH2_STD::enable_if_t<input_range<InputRange1> && input_range<InputRange2>>* = nullptr>
                constexpr bool operator()(InputRange1&& range1, InputRange2&& range2) const
                {
                    return (*this)(
                        RAH2_NS::ranges::begin(range1),
                        RAH2_NS::ranges::end(range1),
                        RAH2_NS::ranges::begin(range2),
                        RAH2_NS::ranges::end(range2));
                }

                /// equal
                ///
                /// Returns: true if for every iterator i in the range [first1, last1) the
                /// following corresponding conditions hold: pred(*i, *(first2 + (i first1))) != false.
                /// Otherwise, returns false.
                ///
                /// Complexity: At most last1 first1 applications of the corresponding predicate.
                ///
                template <
                    typename InputIterator1,
                    typename InputSentinel1,
                    typename InputIterator2,
                    typename InputSentinel2,
                    typename BinaryPredicate>
                bool operator()(
                    InputIterator1 first1,
                    InputSentinel1 last1,
                    InputIterator2 first2,
                    InputSentinel2 last2,
                    BinaryPredicate&& predicate) const
                {
                    for (; first1 != last1; ++first1, ++first2)
                    {
                        if (!predicate(*first1, *first2))
                            return false;
                    }
                    return first2 == last2;
                }

                template <
                    typename InputRange1,
                    typename InputRange2,
                    typename BinaryPredicate,
                    RAH2_STD::enable_if_t<input_range<InputRange1> && input_range<InputRange2>>* = nullptr>
                constexpr bool operator()(
                    InputRange1&& range1, InputRange2&& range2, BinaryPredicate&& predicate) const
                {
                    return (*this)(
                        RAH2_NS::ranges::begin(range1),
                        RAH2_NS::ranges::end(range1),
                        RAH2_NS::ranges::begin(range2),
                        RAH2_NS::ranges::end(range2),
                        RAH2_STD::forward<BinaryPredicate>(predicate));
                }
            };
        } // namespace niebloids
        constexpr niebloids::equal equal;

        namespace niebloids
        {
            struct lexicographical_compare_fn
            {
            private:
                template <
                    typename I1,
                    typename S1,
                    typename I2,
                    typename S2,
                    class Proj1 = RAH2_NS::details::identity,
                    class Proj2 = RAH2_NS::details::identity,
                    typename Comp = RAH2_NS::ranges::less,
                    RAH2_STD::enable_if_t<
                        input_iterator<I1> && sentinel_for<S1, I1> && input_iterator<I2>
                        && sentinel_for<S2, I2>>* = nullptr>
                constexpr bool impl(I1 first1, S1 last1, I2 first2, S2 last2, Comp comp = {}) const
                {
                    for (; (first1 != last1) && (first2 != last2); ++first1, (void)++first2)
                    {
                        if (comp(*first1, *first2))
                            return true;

                        if (comp(*first2, *first1))
                            return false;
                    }
                    return (first1 == last1) && (first2 != last2);
                }

            public:
                bool // Specialization for const char*.
                operator()(char const* first1, char const* last1, char const* first2, char const* last2) const
                {
                    ptrdiff_t const n1(last1 - first1), n2(last2 - first2);
                    int const result =
                        memcmp(first1, first2, static_cast<size_t>(RAH2_NS::details::min(n1, n2)));
                    return result != 0 ? (result < 0) : (n1 < n2);
                }

                bool // Specialization for char*.
                operator()(char* first1, char const* last1, char* first2, char const* last2) const
                {
                    ptrdiff_t const n1(last1 - first1), n2(last2 - first2);
                    int const result =
                        memcmp(first1, first2, static_cast<size_t>(RAH2_NS::details::min(n1, n2)));
                    return result != 0 ? (result < 0) : (n1 < n2);
                }

                bool // Specialization for const unsigned char*.
                operator()(
                    unsigned char const* first1,
                    unsigned char const* last1,
                    unsigned char const* first2,
                    unsigned char const* last2) const
                {
                    ptrdiff_t const n1(last1 - first1), n2(last2 - first2);
                    int const result =
                        memcmp(first1, first2, static_cast<size_t>(RAH2_NS::details::min(n1, n2)));
                    return result != 0 ? (result < 0) : (n1 < n2);
                }

                bool // Specialization for unsigned char*.
                operator()(
                    unsigned char* first1,
                    unsigned char const* last1,
                    unsigned char* first2,
                    unsigned char const* last2) const
                {
                    ptrdiff_t const n1(last1 - first1), n2(last2 - first2);
                    int const result =
                        memcmp(first1, first2, static_cast<size_t>(RAH2_NS::details::min(n1, n2)));
                    return result != 0 ? (result < 0) : (n1 < n2);
                }

                bool // Specialization for const signed char*.
                operator()(
                    signed char const* first1,
                    signed char const* last1,
                    signed char const* first2,
                    signed char const* last2) const
                {
                    ptrdiff_t const n1(last1 - first1), n2(last2 - first2);
                    int const result =
                        memcmp(first1, first2, static_cast<size_t>(RAH2_NS::details::min(n1, n2)));
                    return result != 0 ? (result < 0) : (n1 < n2);
                }

                bool // Specialization for signed char*.
                operator()(
                    signed char* first1,
                    signed char const* last1,
                    signed char* first2,
                    signed char const* last2) const
                {
                    ptrdiff_t const n1(last1 - first1), n2(last2 - first2);
                    int const result =
                        memcmp(first1, first2, static_cast<size_t>(RAH2_NS::details::min(n1, n2)));
                    return result != 0 ? (result < 0) : (n1 < n2);
                }

                template <
                    typename I1,
                    typename S1,
                    typename I2,
                    typename S2,
                    class Proj1 = RAH2_NS::details::identity,
                    class Proj2 = RAH2_NS::details::identity,
                    typename Comp = RAH2_NS::ranges::less,
                    RAH2_STD::enable_if_t<
                        input_iterator<I1> && sentinel_for<S1, I1> && input_iterator<I2>
                        && sentinel_for<S2, I2>>* = nullptr>
                constexpr bool operator()(
                    I1 first1,
                    S1 last1,
                    I2 first2,
                    S2 last2,
                    Comp comp = {},
                    Proj1 proj1 = {},
                    Proj2 proj2 = {}) const
                {
                    auto first_last = details::unwrap(RAH2_STD::move(first1), RAH2_STD::move(last1));
                    auto first2_last2 =
                        details::unwrap(RAH2_STD::move(first2), RAH2_STD::move(last2));
                    return impl(
                        RAH2_STD::move(first_last.iterator),
                        RAH2_STD::move(first_last.sentinel),
                        RAH2_STD::move(first2_last2.iterator),
                        RAH2_STD::move(first2_last2.sentinel),
                        details::wrap_pred_proj(
                            RAH2_STD::move(comp), RAH2_STD::move(proj1), RAH2_STD::move(proj2)));
                }
                template <
                    typename R1,
                    typename R2,
                    class Proj1 = RAH2_NS::details::identity,
                    class Proj2 = RAH2_NS::details::identity,
                    typename Comp = RAH2_NS::ranges::less,
                    RAH2_STD::enable_if_t<input_range<R1> && input_range<R2>>* = nullptr>
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

        /// lexicographical_compare
        ///
        /// Returns: true if the sequence of elements defined by the range
        /// [first1, last1) is lexicographically less than the sequence of
        /// elements defined by the range [first2, last2). Returns false otherwise.
        ///
        /// Complexity: At most 'min((last1 - first1), (last2 - first2))' applications
        /// of the corresponding comparison.
        ///
        /// Note: If two sequences have the same number of elements and their
        /// corresponding elements are equivalent, then neither sequence is
        /// lexicographically less than the other. If one sequence is a prefix
        /// of the other, then the shorter sequence is lexicographically less
        /// than the longer sequence. Otherwise, the lexicographical comparison
        /// of the sequences yields the same result as the comparison of the first
        /// corresponding pair of elements that are not equivalent.
        ///
        constexpr niebloids::lexicographical_compare_fn lexicographical_compare;

        template <class I1, class I2>
        using mismatch_result = in_in_result<I1, I2>;

        namespace niebloids
        {
            struct mismatch_fn
            {
            private:
                template <
                    typename I1,
                    typename S1,
                    typename I2,
                    typename S2,
                    class Pred = RAH2_NS::ranges::equal_to,
                    RAH2_STD::enable_if_t<sized_sentinel_for<S1, I1> && sized_sentinel_for<S2, I2>>* = nullptr>
                RAH2_CONSTEXPR20 mismatch_result<I1, I2>
                impl(I1 first1, S1 last1, I2 first2, S2 last2, Pred pred = {}) const
                {
                    auto const count1 = last1 - first1;
                    auto const count2 = last2 - first2;
                    auto n =
                        RAH2_STD::min(static_cast<intptr_t>(count1), static_cast<intptr_t>(count2));
                    for (; n != 0; ++first1, ++first2, --n)
                    {
                        if (!pred(*first1, *first2))
                        {
                            break;
                        }
                    }

                    return {first1, first2};
                }

                template <
                    typename I1,
                    typename S1,
                    typename I2,
                    typename S2,
                    class Pred = RAH2_NS::ranges::equal_to,
                    RAH2_STD::enable_if_t<not(sized_sentinel_for<S1, I1> && sized_sentinel_for<S2, I2>)>* = nullptr>
                RAH2_CONSTEXPR20 mismatch_result<I1, I2>
                impl(I1 first1, S1 last1, I2 first2, S2 last2, Pred pred = {}) const
                {
                    while (first1 != last1 && first2 != last2 && pred(*first1, *first2))
                    {
                        ++first1;
                        ++first2;
                    }

                    return {first1, first2};
                }

            public:
                template <
                    typename I1,
                    typename S1,
                    typename I2,
                    typename S2,
                    class Pred = RAH2_NS::ranges::equal_to,
                    class Proj1 = RAH2_NS::details::identity,
                    class Proj2 = RAH2_NS::details::identity,
                    RAH2_STD::enable_if_t<
                        input_iterator<I1> && sentinel_for<S1, I1> && input_iterator<I2>
                        && sentinel_for<S2, I2>>* = nullptr>
                RAH2_CONSTEXPR20 mismatch_result<I1, I2> operator()(
                    I1 first1,
                    S1 last1,
                    I2 first2,
                    S2 last2,
                    Pred pred = {},
                    Proj1 proj1 = {},
                    Proj2 proj2 = {}) const
                {
                    auto first_last = details::unwrap(RAH2_STD::move(first1), RAH2_STD::move(last1));
                    auto first2_last2 =
                        details::unwrap(RAH2_STD::move(first2), RAH2_STD::move(last2));

                    auto i1_i2 = impl(
                        RAH2_STD::move(first_last.iterator),
                        RAH2_STD::move(first_last.sentinel),
                        RAH2_STD::move(first2_last2.iterator),
                        RAH2_STD::move(first2_last2.sentinel),
                        details::wrap_pred_proj(
                            RAH2_STD::move(pred), RAH2_STD::move(proj1), RAH2_STD::move(proj2)));
                    return {
                        first_last.wrap_iterator(RAH2_STD::move(i1_i2.in1)),
                        first2_last2.wrap_iterator(RAH2_STD::move(i1_i2.in2))};
                }

                template <
                    typename R1,
                    typename R2,
                    class Pred = RAH2_NS::ranges::equal_to,
                    class Proj1 = RAH2_NS::details::identity,
                    class Proj2 = RAH2_NS::details::identity,
                    RAH2_STD::enable_if_t<input_range<R1> && input_range<R2>>* = nullptr>
                RAH2_CONSTEXPR20 mismatch_result<borrowed_iterator_t<R1>, borrowed_iterator_t<R2>>
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

        /// mismatch
        ///
        /// Finds the first position where the two ranges [first1, last1) and
        /// [first2, first2 + (last1 - first1)) differ. The two versions of
        /// mismatch use different tests for whether elements differ.
        ///
        /// Returns: A pair of iterators i and j such that j == first2 + (i - first1)
        /// and i is the first iterator in the range [first1, last1) for which the
        /// following corresponding condition holds: pred(*i, *(first2 + (i - first1))) == false.
        /// Returns the pair last1 and first2 + (last1 - first1) if such an iterator
        /// i is not found.
        ///
        /// Complexity: At most last1 first1 applications of the corresponding predicate.
        ///
        constexpr niebloids::mismatch_fn mismatch;

        namespace niebloids
        {
            struct lower_bound
            {
                /// lower_bound
                ///
                /// Finds the position of the first element in a sorted range that has a value
                /// greater than or equivalent to a specified value.
                ///
                /// Effects: Finds the first position into which value can be inserted without
                /// violating the ordering.
                ///
                /// Returns: The furthermost iterator i in the range [first, last) such that
                /// for any iterator j in the range [first, i) the following corresponding
                /// condition holds: *j < value.
                ///
                /// Complexity: At most 'log(last - first) + 1' comparisons.
                ///
                /// Optimizations: We have no need to specialize this implementation for random
                /// access iterators (e.g. contiguous array), as the code below will already
                /// take advantage of them.
                ///
                template <typename ForwardIterator, typename Sentinel, typename T>
                ForwardIterator operator()(ForwardIterator first, Sentinel last, T const& value) const
                {
                    using DifferenceType =
                        typename RAH2_STD::iterator_traits<ForwardIterator>::difference_type;

                    DifferenceType d = RAH2_NS::ranges::distance(
                        first,
                        last); // This will be efficient for a random access iterator such as an array.

                    while (d > 0)
                    {
                        ForwardIterator i = first;
                        DifferenceType d2 =
                            d >> 1; // We use '>>1' here instead of '/2' because MSVC++ for some reason generates significantly worse code for '/2'. Go figure.

                        RAH2_NS::ranges::advance(
                            i, d2); // This will be efficient for a random access iterator such as an array.

                        if (*i < value)
                        {
                            // Disabled because RAH2_STD::lower_bound doesn't specify (23.3.3.3, p3) this can be done: RAH2_VALIDATE_COMPARE(!(value < *i)); // Validate that the compare function is sane.
                            first = ++i;
                            d -= d2 + 1;
                        }
                        else
                            d = d2;
                    }
                    return first;
                }

                template <typename ForwardRange, typename T>
                iterator_t<ForwardRange> operator()(ForwardRange&& range, T const& value) const
                {
                    return (*this)(RAH2_NS::ranges::begin(range), RAH2_NS::ranges::end(range), value);
                }

                /// lower_bound
                ///
                /// Finds the position of the first element in a sorted range that has a value
                /// greater than or equivalent to a specified value. The input Compare function
                /// takes two arguments and returns true if the first argument is less than
                /// the second argument.
                ///
                /// Effects: Finds the first position into which value can be inserted without
                /// violating the ordering.
                ///
                /// Returns: The furthermost iterator i in the range [first, last) such that
                /// for any iterator j in the range [first, i) the following corresponding
                /// condition holds: compare(*j, value) != false.
                ///
                /// Complexity: At most 'log(last - first) + 1' comparisons.
                ///
                /// Optimizations: We have no need to specialize this implementation for random
                /// access iterators (e.g. contiguous array), as the code below will already
                /// take advantage of them.
                ///
                template <typename ForwardIterator, typename ForwardSentinel, typename T, typename Compare>
                ForwardIterator operator()(
                    ForwardIterator first, ForwardSentinel last, T const& value, Compare compare) const
                {
                    using DifferenceType =
                        typename RAH2_STD::iterator_traits<ForwardIterator>::difference_type;

                    DifferenceType d = RAH2_NS::ranges::distance(
                        first,
                        last); // This will be efficient for a random access iterator such as an array.

                    while (d > 0)
                    {
                        ForwardIterator i = first;
                        DifferenceType d2 =
                            d >> 1; // We use '>>1' here instead of '/2' because MSVC++ for some reason generates significantly worse code for '/2'. Go figure.

                        RAH2_NS::ranges::advance(
                            i, d2); // This will be efficient for a random access iterator such as an array.

                        if (compare(*i, value))
                        {
                            // Disabled because RAH2_STD::lower_bound doesn't specify (23.3.3.1, p3) this can be done: RAH2_VALIDATE_COMPARE(!compare(value, *i)); // Validate that the compare function is sane.
                            first = ++i;
                            d -= d2 + 1;
                        }
                        else
                            d = d2;
                    }
                    return first;
                }

                template <typename ForwardRange, typename T, typename Compare>
                iterator_t<ForwardRange>
                operator()(ForwardRange&& range, T const& value, Compare compare) const
                {
                    return (*this)(
                        RAH2_NS::ranges::begin(range),
                        RAH2_NS::ranges::end(range),
                        value,
                        RAH2_STD::move(compare));
                }
            };
        } // namespace niebloids
        constexpr niebloids::lower_bound lower_bound;

        namespace niebloids
        {
            struct upper_bound
            {

                /// upper_bound
                ///
                /// Finds the position of the first element in a sorted range that has a
                /// value that is greater than a specified value.
                ///
                /// Effects: Finds the furthermost position into which value can be inserted
                /// without violating the ordering.
                ///
                /// Returns: The furthermost iterator i in the range [first, last) such that
                /// for any iterator j in the range [first, i) the following corresponding
                /// condition holds: !(value < *j).
                ///
                /// Complexity: At most 'log(last - first) + 1' comparisons.
                ///
                template <typename ForwardIterator, typename Sentinel, typename T>
                ForwardIterator operator()(ForwardIterator first, Sentinel last, T const& value) const
                {
                    using DifferenceType =
                        typename RAH2_STD::iterator_traits<ForwardIterator>::difference_type;

                    DifferenceType len = RAH2_NS::ranges::distance(first, last);

                    while (len > 0)
                    {
                        ForwardIterator i = first;
                        DifferenceType len2 =
                            len >> 1; // We use '>>1' here instead of '/2' because MSVC++ for some reason generates significantly worse code for '/2'. Go figure.

                        RAH2_NS::ranges::advance(i, len2);

                        if (!(value < *i)) // Note that we always express value comparisons in terms of < or ==.
                        {
                            first = ++i;
                            len -= len2 + 1;
                        }
                        else
                        {
                            // Disabled because RAH2_STD::upper_bound doesn't specify (23.3.3.2, p3) this can be done: RAH2_VALIDATE_COMPARE(!(*i < value)); // Validate that the compare function is sane.
                            len = len2;
                        }
                    }
                    return first;
                }

                template <typename ForwardRange, typename T>
                iterator_t<ForwardRange> operator()(ForwardRange&& range, T const& value) const
                {
                    return (*this)(RAH2_NS::ranges::begin(range), RAH2_NS::ranges::end(range), value);
                }

                /// upper_bound
                ///
                /// Finds the position of the first element in a sorted range that has a
                /// value that is greater than a specified value. The input Compare function
                /// takes two arguments and returns true if the first argument is less than
                /// the second argument.
                ///
                /// Effects: Finds the furthermost position into which value can be inserted
                /// without violating the ordering.
                ///
                /// Returns: The furthermost iterator i in the range [first, last) such that
                /// for any iterator j in the range [first, i) the following corresponding
                /// condition holds: compare(value, *j) == false.
                ///
                /// Complexity: At most 'log(last - first) + 1' comparisons.
                ///
                template <typename ForwardIterator, typename ForwardSentinel, typename T, typename Compare>
                ForwardIterator operator()(
                    ForwardIterator first, ForwardSentinel last, T const& value, Compare compare) const
                {
                    using DifferenceType =
                        typename RAH2_STD::iterator_traits<ForwardIterator>::difference_type;

                    DifferenceType len = RAH2_NS::ranges::distance(first, last);

                    while (len > 0)
                    {
                        ForwardIterator i = first;
                        DifferenceType len2 =
                            len >> 1; // We use '>>1' here instead of '/2' because MSVC++ for some reason generates significantly worse code for '/2'. Go figure.

                        RAH2_NS::ranges::advance(i, len2);

                        if (!compare(value, *i))
                        {
                            first = ++i;
                            len -= len2 + 1;
                        }
                        else
                        {
                            // Disabled because RAH2_STD::upper_bound doesn't specify (23.3.3.2, p3) this can be done: RAH2_VALIDATE_COMPARE(!compare(*i, value)); // Validate that the compare function is sane.
                            len = len2;
                        }
                    }
                    return first;
                }

                template <typename ForwardRange, typename T, typename Compare>
                iterator_t<ForwardRange>
                operator()(ForwardRange&& range, T const& value, Compare compare) const
                {
                    return (*this)(
                        RAH2_NS::ranges::begin(range),
                        RAH2_NS::ranges::end(range),
                        value,
                        RAH2_STD::move(compare));
                }
            };
        } // namespace niebloids
        constexpr niebloids::upper_bound upper_bound;

        namespace niebloids
        {
            struct equal_range
            {
                /// equal_range
                ///
                /// Effects: Finds the largest subrange [i, j) such that the value can be inserted
                /// at any iterator k in it without violating the ordering. k satisfies the
                /// corresponding conditions: !(*k < value) && !(value < *k).
                ///
                /// Complexity: At most '2 * log(last - first) + 1' comparisons.
                ///
                template <
                    typename ForwardIterator,
                    typename Sentinel,
                    typename T,
                    RAH2_STD::enable_if_t<sentinel_for<Sentinel, ForwardIterator>>* = nullptr>
                subrange<ForwardIterator>
                operator()(ForwardIterator first, Sentinel last, T const& value) const
                {
                    using DifferenceType =
                        typename RAH2_STD::iterator_traits<ForwardIterator>::difference_type;

                    DifferenceType d = RAH2_NS::ranges::distance(first, last);

                    while (d > 0)
                    {
                        ForwardIterator i(first);
                        DifferenceType d2 =
                            d >> 1; // We use '>>1' here instead of '/2' because MSVC++ for some reason generates significantly worse code for '/2'. Go figure.

                        RAH2_NS::ranges::advance(i, d2);

                        if (*i < value)
                        {
                            RAH2_ASSERT(!(value < *i)); // Validate that the compare function is sane.
                            first = ++i;
                            d -= d2 + 1;
                        }
                        else if (value < *i)
                        {
                            RAH2_ASSERT(!(*i < value)); // Validate that the compare function is sane.
                            d = d2;
                            last = i;
                        }
                        else
                        {
                            ForwardIterator j(i);

                            return {
                                RAH2_NS::ranges::lower_bound(first, i, value),
                                RAH2_NS::ranges::upper_bound(++j, last, value)};
                        }
                    }
                    return {first, first};
                }

                template <typename ForwardRange, typename T>
                borrowed_subrange_t<ForwardRange> operator()(ForwardRange&& range, T const& value) const
                {
                    return (*this)(RAH2_NS::ranges::begin(range), RAH2_NS::ranges::end(range), value);
                }

                /// equal_range
                ///
                /// Effects: Finds the largest subrange [i, j) such that the value can be inserted
                /// at any iterator k in it without violating the ordering. k satisfies the
                /// corresponding conditions: compare(*k, value) == false && compare(value, *k) == false.
                ///
                /// Complexity: At most '2 * log(last - first) + 1' comparisons.
                ///
                template <
                    typename ForwardIterator,
                    typename Sentinel,
                    typename T,
                    typename Compare,
                    RAH2_STD::enable_if_t<sentinel_for<Sentinel, ForwardIterator>>* = nullptr>
                subrange<ForwardIterator, ForwardIterator>
                operator()(ForwardIterator first, Sentinel last, T const& value, Compare compare) const
                {
                    using DifferenceType =
                        typename RAH2_STD::iterator_traits<ForwardIterator>::difference_type;

                    DifferenceType d = RAH2_NS::ranges::distance(first, last);

                    while (d > 0)
                    {
                        ForwardIterator i(first);
                        DifferenceType d2 =
                            d >> 1; // We use '>>1' here instead of '/2' because MSVC++ for some reason generates significantly worse code for '/2'. Go figure.

                        RAH2_NS::ranges::advance(i, d2);

                        if (compare(*i, value))
                        {
                            RAH2_ASSERT(
                                !compare(value, *i)); // Validate that the compare function is sane.
                            first = ++i;
                            d -= d2 + 1;
                        }
                        else if (compare(value, *i))
                        {
                            RAH2_ASSERT(
                                !compare(*i, value)); // Validate that the compare function is sane.
                            d = d2;
                            last = i;
                        }
                        else
                        {
                            ForwardIterator j(i);

                            return {
                                RAH2_NS::ranges::lower_bound(first, i, value, compare),
                                RAH2_NS::ranges::upper_bound(++j, last, value, compare)};
                        }
                    }
                    return {first, first};
                }

                template <typename ForwardRange, typename T, typename Compare>
                subrange<iterator_t<ForwardRange>, sentinel_t<ForwardRange>>
                operator()(ForwardRange&& range, T const& value, Compare compare) const
                {
                    return (*this)(
                        RAH2_NS::ranges::begin(range),
                        RAH2_NS::ranges::end(range),
                        value,
                        RAH2_STD::move(compare));
                }
            };
        } // namespace niebloids
        constexpr niebloids::equal_range equal_range;

        namespace niebloids
        {
            struct replace_fn
            {
                template <
                    typename I,
                    typename S,
                    class T1,
                    class T2,
                    RAH2_STD::enable_if_t<
                        input_iterator<I> && sentinel_for<S, I> && indirectly_writable<I, T2 const&>>* = nullptr>
                constexpr I operator()(I first, S last, T1 const& old_value, T2 const& new_value) const
                {
                    for (; first != last; ++first)
                        if (old_value == *first)
                            *first = new_value;
                    return first;
                }

                template <
                    typename R, // input_range
                    class T1,
                    class T2,
                    RAH2_STD::enable_if_t<
                        input_range<R> && indirectly_writable<iterator_t<R>, T2 const&>>* = nullptr>
                constexpr borrowed_iterator_t<R>
                operator()(R&& r, T1 const& old_value, T2 const& new_value) const
                {
                    return (*this)(
                        RAH2_NS::ranges::begin(r), RAH2_NS::ranges::end(r), old_value, new_value);
                }
            };
        } // namespace niebloids

        /// replace
        ///
        /// Effects: Substitutes elements referred by the iterator i in the range [first, last)
        /// with new_value, when the following corresponding conditions hold: *i == old_value.
        ///
        /// Complexity: Exactly 'last - first' applications of the corresponding predicate.
        ///
        /// Note: The predicate version of replace is replace_if and not another variation of replace.
        /// This is because both versions would have the same parameter count and there could be ambiguity.
        ///
        constexpr niebloids::replace_fn replace{};

        namespace niebloids
        {
            struct replace_if_fn
            {
                template <
                    typename I, // input_iterator
                    typename S, // RAH2_STD::sentinel_for<I>
                    class T,
                    typename Pred, // indirect_unary_predicate<I>
                    RAH2_STD::enable_if_t<
                        input_iterator<I> && sentinel_for<S, I> && indirectly_writable<I, T const&>>* = nullptr>
                constexpr I operator()(I first, S last, Pred pred, T const& new_value) const
                {
                    for (; first != last; ++first)
                        if (pred(*first))
                            *first = new_value;
                    return RAH2_STD::move(first);
                }

                template <
                    typename R, // input_range
                    class T,
                    typename Pred, // indirect_unary_predicate<iterator_t<R>>>
                    RAH2_STD::enable_if_t<input_range<R> && output_range<R, T>>* = nullptr>
                constexpr borrowed_iterator_t<R> operator()(R&& r, Pred pred, T const& new_value) const
                {
                    return (*this)(
                        RAH2_NS::ranges::begin(r),
                        RAH2_NS::ranges::end(r),
                        RAH2_STD::move(pred),
                        new_value);
                }
            };
        } // namespace niebloids

        /// replace_if
        ///
        /// Effects: Substitutes elements referred by the iterator i in the range [first, last)
        /// with new_value, when the following corresponding conditions hold: predicate(*i) != false.
        ///
        /// Complexity: Exactly 'last - first' applications of the corresponding predicate.
        ///
        /// Note: The predicate version of replace_if is replace and not another variation of replace_if.
        /// This is because both versions would have the same parameter count and there could be ambiguity.
        ///
        constexpr niebloids::replace_if_fn replace_if{};

        template <class I, class O>
        using remove_copy_result = RAH2_NS::ranges::in_out_result<I, O>;

        namespace niebloids
        {
            struct remove_copy_fn
            {
                template <
                    typename I,
                    typename S,
                    typename O,
                    class T,
                    RAH2_STD::enable_if_t<
                        RAH2_NS::input_iterator<I> && RAH2_NS::sentinel_for<S, I>
                        && RAH2_NS::weakly_incrementable<O> && RAH2_NS::indirectly_copyable<I, O>>* = nullptr>
                constexpr RAH2_NS::ranges::remove_copy_result<I, O>
                operator()(I first, S last, O result, T const& value) const
                {
                    for (; !(first == last); ++first)
                    {
                        if (value != *first)
                        {
                            *result = *first;
                            ++result;
                        }
                    }
                    return {RAH2_STD::move(first), RAH2_STD::move(result)};
                }

                template <
                    typename R,
                    typename O,
                    class T,
                    RAH2_STD::enable_if_t<
                        RAH2_NS::ranges::input_range<R> && RAH2_NS::weakly_incrementable<O>
                        && RAH2_NS::indirectly_copyable<RAH2_NS::ranges::iterator_t<R>, O>>* = nullptr>
                constexpr RAH2_NS::ranges::remove_copy_result<RAH2_NS::ranges::borrowed_iterator_t<R>, O>
                operator()(R&& r, O result, T const& value) const
                {
                    return (*this)(
                        RAH2_NS::ranges::begin(r),
                        RAH2_NS::ranges::end(r),
                        RAH2_STD::move(result),
                        value);
                }
            };
        } // namespace niebloids

        /// remove_copy
        ///
        /// Effects: Copies all the elements referred to by the iterator i in the range
        /// [first, last) for which the following corresponding condition does not hold:
        /// *i == value.
        ///
        /// Requires: The ranges [first, last) and [result, result + (last - first)) shall not overlap.
        ///
        /// Returns: The end of the resulting range.
        ///
        /// Complexity: Exactly 'last - first' applications of the corresponding predicate.
        ///
        constexpr niebloids::remove_copy_fn remove_copy{};

        template <class I, class O>
        using remove_copy_if_result = RAH2_NS::ranges::in_out_result<I, O>;

        namespace niebloids
        {
            struct remove_copy_if_fn
            {
                template <
                    typename I, // RAH2_STD::input_iterator
                    typename S, // RAH2_STD::sentinel_for<I>
                    typename O, // RAH2_STD::weakly_incrementable
                    typename Pred, // RAH2_STD::indirect_unary_predicate<RAH2_STD::projected<I, Proj>>
                    RAH2_STD::enable_if_t<
                        input_iterator<I> && sentinel_for<S, I> && weakly_incrementable<O>
                        && indirectly_copyable<I, O>>* = nullptr>
                constexpr RAH2_NS::ranges::remove_copy_if_result<I, O>
                operator()(I first, S last, O result, Pred pred) const
                {
                    for (; first != last; ++first)
                    {
                        if (!pred(*first))
                        {
                            *result = *first;
                            ++result;
                        }
                    }
                    return {RAH2_STD::move(first), RAH2_STD::move(result)};
                }

                template <
                    typename R, // RAH2_NS::input_range
                    typename O, // RAH2_NS::weakly_incrementable
                    typename Pred,
                    RAH2_STD::enable_if_t<
                        input_range<R> && weakly_incrementable<O>
                        && indirectly_copyable<iterator_t<R>, O>>* = nullptr>
                constexpr RAH2_NS::ranges::remove_copy_if_result<RAH2_NS::ranges::borrowed_iterator_t<R>, O>
                operator()(R&& r, O result, Pred pred) const
                {
                    return (*this)(
                        RAH2_NS::ranges::begin(r),
                        RAH2_NS::ranges::end(r),
                        RAH2_STD::move(result),
                        RAH2_STD::move(pred));
                }
            };
        } // namespace niebloids

        /// remove_copy_if
        ///
        /// Effects: Copies all the elements referred to by the iterator i in the range
        /// [first, last) for which the following corresponding condition does not hold:
        /// predicate(*i) != false.
        ///
        /// Requires: The ranges [first, last) and [result, result + (last - first)) shall not overlap.
        ///
        /// Returns: The end of the resulting range.
        ///
        /// Complexity: Exactly 'last - first' applications of the corresponding predicate.
        ///
        constexpr niebloids::remove_copy_if_fn remove_copy_if{};

        namespace niebloids
        {
            struct remove
            {
                /// remove
                ///
                /// Effects: Eliminates all the elements referred to by iterator i in the
                /// range [first, last) for which the following corresponding condition
                /// holds: *i == value.
                ///
                /// Returns: The end of the resulting range.
                ///
                /// Complexity: Exactly 'last - first' applications of the corresponding predicate.
                ///
                /// Note: The predicate version of remove is remove_if and not another variation of remove.
                /// This is because both versions would have the same parameter count and there could be ambiguity.
                ///
                /// Note: Since this function moves the element to the back of the heap and
                /// doesn't actually remove it from the given container, the user must call
                /// the container erase function if the user wants to erase the element
                /// from the container.
                ///
                /// Example usage:
                ///    vector<int> intArray;
                ///    ...
                ///    intArray.erase(remove(intArray.begin(), intArray.end(), 4), intArray.end()); // Erase all elements of value 4.
                ///
                template <typename ForwardIterator, typename ForwardSentinel, typename T>
                subrange<ForwardIterator>
                operator()(ForwardIterator first, ForwardSentinel last, T const& value) const
                {
                    first = RAH2_NS::ranges::find(first, last, value);
                    if (first != last)
                    {
                        ForwardIterator i(first);
                        return {RAH2_NS::ranges::remove_copy(++i, last, first, value).out, last};
                    }
                    return {first, last};
                }

                template <typename ForwardRange, typename T>
                borrowed_subrange_t<ForwardRange> operator()(ForwardRange&& range, T const& value) const
                {
                    return (*this)(RAH2_NS::ranges::begin(range), RAH2_NS::ranges::end(range), value);
                }
            };
        } // namespace niebloids
        constexpr niebloids::remove remove;
        namespace niebloids
        {
            struct remove_if
            {
                /// remove_if
                ///
                /// Effects: Eliminates all the elements referred to by iterator i in the
                /// range [first, last) for which the following corresponding condition
                /// holds: predicate(*i) != false.
                ///
                /// Returns: The end of the resulting range.
                ///
                /// Complexity: Exactly 'last - first' applications of the corresponding predicate.
                ///
                /// Note: The predicate version of remove_if is remove and not another variation of remove_if.
                /// This is because both versions would have the same parameter count and there could be ambiguity.
                ///
                /// Note: Since this function moves the element to the back of the heap and
                /// doesn't actually remove it from the given container, the user must call
                /// the container erase function if the user wants to erase the element
                /// from the container.
                ///
                /// Example usage:
                ///    vector<int> intArray;
                ///    ...
                ///    intArray.erase(remove(intArray.begin(), intArray.end(), bind2nd(less<int>(), (int)3)), intArray.end()); // Erase all elements less than 3.
                ///
                template <typename ForwardIterator, typename ForwardSentinel, typename Predicate>
                subrange<ForwardIterator>
                operator()(ForwardIterator first, ForwardSentinel last, Predicate predicate) const
                {
                    first = RAH2_NS::ranges::find_if(first, last, predicate);
                    if (first != last)
                    {
                        ForwardIterator i(first);
                        ++i;
                        for (; i != last; ++i)
                        {
                            if (!predicate(*i))
                            {
                                *first = RAH2_STD::move(*i);
                                ++first;
                            }
                        }
                        return {RAH2_STD::move(first), RAH2_STD::move(last)};
                    }
                    return {first, last};
                }

                template <typename ForwardRange, typename Predicate>
                borrowed_subrange_t<ForwardRange>
                operator()(ForwardRange&& range, Predicate predicate) const
                {
                    return (*this)(
                        RAH2_NS::ranges::begin(range),
                        RAH2_NS::ranges::end(range),
                        RAH2_STD::move(predicate));
                }
            };
        } // namespace niebloids
        constexpr niebloids::remove_if remove_if;

        template <class I, class O>
        using replace_copy_result = RAH2_NS::ranges::in_out_result<I, O>;

        namespace niebloids
        {
            struct replace_copy
            {
                /// replace_copy
                ///
                /// Effects: Assigns to every iterator i in the range [result, result + (last - first))
                /// either new_value or *(first + (i - result)) depending on whether the following
                /// corresponding conditions hold: *(first + (i - result)) == old_value.
                ///
                /// Requires: The ranges [first, last) and [result, result + (last - first)) shall not overlap.
                ///
                /// Returns: result + (last - first).
                ///
                /// Complexity: Exactly 'last - first' applications of the corresponding predicate.
                ///
                /// Note: The predicate version of replace_copy is replace_copy_if and not another variation of replace_copy.
                /// This is because both versions would have the same parameter count and there could be ambiguity.
                ///
                template <typename InputIterator, typename InputSentinel, typename OutputIterator, typename T>
                replace_copy_result<InputIterator, OutputIterator> operator()(
                    InputIterator first,
                    InputSentinel last,
                    OutputIterator result,
                    T const& old_value,
                    T const& new_value) const
                {
                    for (; first != last; ++first, ++result)
                        *result = (*first == old_value) ? new_value : *first;
                    return {first, result};
                }

                template <typename InputRange, typename OutputIterator, typename T>
                replace_copy_result<RAH2_NS::ranges::borrowed_iterator_t<InputRange>, OutputIterator>
                operator()(
                    InputRange&& range, OutputIterator result, T const& old_value, T const& new_value) const
                {
                    return (*this)(
                        RAH2_NS::ranges::begin(range),
                        RAH2_NS::ranges::end(range),
                        RAH2_STD::move(result),
                        old_value,
                        new_value);
                }
            };
        } // namespace niebloids
        constexpr niebloids::replace_copy replace_copy;

        template <class I, class O>
        using replace_copy_if_result = RAH2_NS::ranges::in_out_result<I, O>;

        namespace niebloids
        {
            struct replace_copy_if
            {
                /// replace_copy_if
                ///
                /// Effects: Assigns to every iterator i in the range [result, result + (last - first))
                /// either new_value or *(first + (i - result)) depending on whether the following
                /// corresponding conditions hold: predicate(*(first + (i - result))) != false.
                ///
                /// Requires: The ranges [first, last) and [result, result+(lastfirst)) shall not overlap.
                ///
                /// Returns: result + (last - first).
                ///
                /// Complexity: Exactly 'last - first' applications of the corresponding predicate.
                ///
                /// Note: The predicate version of replace_copy_if is replace_copy and not another variation of replace_copy_if.
                /// This is because both versions would have the same parameter count and there could be ambiguity.
                ///
                template <typename InputIterator, typename InputSentinel, typename OutputIterator, typename Predicate, typename T>
                replace_copy_if_result<InputIterator, OutputIterator> operator()(
                    InputIterator first,
                    InputSentinel last,
                    OutputIterator result,
                    Predicate predicate,
                    T const& new_value) const
                {
                    for (; first != last; ++first, ++result)
                        *result = predicate(*first) ? new_value : *first;
                    return {first, result};
                }

                template <typename InputRange, typename OutputIterator, typename Predicate, typename T>
                replace_copy_if_result<RAH2_NS::ranges::borrowed_iterator_t<InputRange>, OutputIterator>
                operator()(
                    InputRange&& range,
                    OutputIterator result,
                    Predicate predicate,
                    T const& new_value) const
                {
                    return (*this)(
                        RAH2_NS::ranges::begin(range),
                        RAH2_NS::ranges::end(range),
                        RAH2_STD::move(result),
                        RAH2_STD::move(predicate),
                        new_value);
                }
            };
        } // namespace niebloids
        constexpr niebloids::replace_copy_if replace_copy_if;

        // reverse
        //
        // We provide helper functions which allow reverse to be implemented more
        // efficiently for some types of iterators and types.
        //
        namespace niebloids
        {
            struct reverse
            {
                template <typename BidirectionalIterator, typename Sentinel>
                static BidirectionalIterator reverse_impl(
                    BidirectionalIterator first, Sentinel last, RAH2_ITC_NS::bidirectional_iterator_tag)
                {
                    for (; (first != last) && (first != --last);
                         ++first) // We are not allowed to use operator <, <=, >, >= with a
                        RAH2_NS::ranges::iter_swap(
                            first, last); // generic (bidirectional or otherwise) iterator.
                    return first;
                }

                template <typename RandomAccessIterator, typename Sentinel>
                static RandomAccessIterator reverse_impl(
                    RandomAccessIterator first, Sentinel last, RAH2_ITC_NS::random_access_iterator_tag)
                {
                    if (first != last)
                    {
                        for (; first < --last;
                             ++first) // With a random access iterator, we can use operator < to more efficiently implement
                            RAH2_NS::ranges::iter_swap(
                                first,
                                last); // this algorithm. A generic iterator doesn't necessarily have an operator < defined.
                    }
                    return first;
                }

                /// reverse
                ///
                /// Reverses the values within the range [first, last).
                ///
                /// Effects: For each nonnegative integer i <= (last - first) / 2,
                /// applies swap to all pairs of iterators first + i, (last i) - 1.
                ///
                /// Complexity: Exactly '(last - first) / 2' swaps.
                ///
                template <typename BidirectionalIterator, typename Sentinel>
                BidirectionalIterator operator()(BidirectionalIterator first, Sentinel last) const
                {
                    using IC = RAH2_NS::details::iterator_concept<BidirectionalIterator>;
                    return reverse_impl(first, last, IC());
                }

                template <typename BidirectionalRange>
                borrowed_iterator_t<BidirectionalRange> operator()(BidirectionalRange&& range) const
                {
                    using IC = details::range_iter_categ_t<BidirectionalRange>;
                    return reverse_impl(
                        RAH2_NS::ranges::begin(range), RAH2_NS::ranges::end(range), IC());
                }
            };
        } // namespace niebloids
        constexpr niebloids::reverse reverse;

        template <class I, class O>
        using reverse_copy_result = RAH2_NS::ranges::in_out_result<I, O>;

        namespace niebloids
        {
            struct reverse_copy
            {
                /// reverse_copy
                ///
                /// Copies the range [first, last) in reverse order to the result.
                ///
                /// Effects: Copies the range [first, last) to the range
                /// [result, result + (last - first)) such that for any nonnegative
                /// integer i < (last - first) the following assignment takes place:
                /// *(result + (last - first) - i) = *(first + i)
                ///
                /// Requires: The ranges [first, last) and [result, result + (last - first))
                /// shall not overlap.
                ///
                /// Returns: result + (last - first). That is, returns the end of the output range.
                ///
                /// Complexity: Exactly 'last - first' assignments.
                ///
                template <typename BidirectionalIterator, typename Sentinel, typename OutputIterator>
                reverse_copy_result<BidirectionalIterator, OutputIterator>
                operator()(BidirectionalIterator first, Sentinel last, OutputIterator result) const
                {
                    auto ret = RAH2_NS::ranges::next(first, last);
                    for (; last != first; ++result)
                        *result = *--last;
                    return {RAH2_STD::move(ret), RAH2_STD::move(result)};
                }

                template <typename BidirectionalRange, typename OutputIterator>
                reverse_copy_result<RAH2_NS::ranges::borrowed_iterator_t<BidirectionalRange>, OutputIterator>
                operator()(BidirectionalRange&& range, OutputIterator result) const
                {
                    return (*this)(
                        RAH2_NS::ranges::begin(range),
                        RAH2_NS::ranges::end(range),
                        RAH2_STD::move(result));
                }
            };
        } // namespace niebloids
        constexpr niebloids::reverse_copy reverse_copy;

        namespace niebloids
        {
            struct search_fn
            {
                template <
                    typename I1,
                    typename S1,
                    typename I2,
                    typename S2,
                    class Pred = RAH2_NS::ranges::equal_to,
                    class Proj1 = RAH2_NS::details::identity,
                    class Proj2 = RAH2_NS::details::identity,
                    RAH2_STD::enable_if_t<forward_iterator<I1> && forward_iterator<I2>>* = nullptr>
                constexpr subrange<I1> operator()(
                    I1 first1,
                    S1 last1,
                    I2 first2,
                    S2 last2,
                    Pred pred = {},
                    Proj1 proj1 = {},
                    Proj2 proj2 = {}) const
                {
                    for (;; ++first1)
                    {
                        I1 it1 = first1;
                        for (I2 it2 = first2;; ++it1, ++it2)
                        {
                            if (it2 == last2)
                                return {first1, it1};
                            if (it1 == last1)
                                return {it1, it1};
                            if (!pred(proj1(*it1), proj2(*it2)))
                                break;
                        }
                    }
                }

                template <
                    typename R1,
                    typename R2,
                    class Pred = RAH2_NS::ranges::equal_to,
                    class Proj1 = RAH2_NS::details::identity,
                    class Proj2 = RAH2_NS::details::identity,
                    RAH2_STD::enable_if_t<forward_range<R1> && forward_range<R2>>* = nullptr>
                constexpr RAH2_STD::enable_if_t<forward_range<R1>, subrange<iterator_t<R1>>>
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

        /// search
        ///
        /// Search finds a subsequence within the range [first1, last1) that is identical to [first2, last2)
        /// when compared element-by-element. It returns an iterator pointing to the beginning of that
        /// subsequence, or else last1 if no such subsequence exists. As such, it is very much like
        /// the C strstr function, with the primary difference being that strstr uses 0-terminated strings
        /// whereas search uses an end iterator to specify the end of a string.
        ///
        /// Returns: The first iterator i in the range [first1, last1 - (last2 - first2)) such that for
        /// any nonnegative integer n less than 'last2 - first2' the following corresponding condition holds:
        /// *(i + n) == *(first2 + n). Returns last1 if no such iterator is found.
        ///
        /// Complexity: At most (last1 first1) * (last2 first2) applications of the corresponding predicate.
        ///
        constexpr niebloids::search_fn search{};

        namespace niebloids
        {
            struct search_n_fn
            {
                // TODO : Make a faster random_access version like in EASTL
                template <
                    typename I,
                    typename S,
                    class T,
                    class Pred = RAH2_NS::ranges::equal_to,
                    class Proj = RAH2_NS::details::identity,
                    RAH2_STD::enable_if_t<forward_iterator<I> && sentinel_for<S, I>>* = nullptr>
                constexpr RAH2_NS::ranges::subrange<I> operator()(
                    I first,
                    S last,
                    RAH2_NS::iter_difference_t<I> count,
                    T const& value,
                    Pred pred = {},
                    Proj proj = {}) const
                {
                    if (count <= 0)
                        return {first, first};
                    for (; first != last; ++first)
                    {
                        if (RAH2_INVOKE_2(pred, RAH2_INVOKE_1(proj, *first), value))
                        {
                            I start = first;
                            RAH2_NS::iter_difference_t<I> n{1};
                            for (;;)
                            {
                                if (n++ == count)
                                    return {start, RAH2_STD::next(first)}; // found
                                if (++first == last)
                                    return {first, first}; // not found
                                if (!RAH2_INVOKE_2(pred, RAH2_INVOKE_1(proj, *first), value))
                                    break; // not equ to value
                            }
                        }
                    }
                    return {first, first};
                }

                template <
                    typename R,
                    class T,
                    class Pred = RAH2_NS::ranges::equal_to,
                    class Proj = RAH2_NS::details::identity,
                    RAH2_STD::enable_if_t<forward_range<R>>* = nullptr>
                constexpr RAH2_NS::ranges::borrowed_subrange_t<R> operator()(
                    R&& r, range_difference_t<R> count, T const& value, Pred pred = {}, Proj proj = {}) const
                {
                    return (*this)(
                        RAH2_NS::ranges::begin(r),
                        RAH2_NS::ranges::end(r),
                        count,
                        value,
                        RAH2_STD::move(pred),
                        RAH2_STD::move(proj));
                }
            };
        } // namespace niebloids

        /// search_n
        ///
        /// Returns: The first iterator i in the range [first, last count) such that
        /// for any nonnegative integer n less than count the following corresponding
        /// conditions hold: *(i + n) == value, pred(*(i + n),value) != false.
        /// Returns last if no such iterator is found.
        ///
        /// Complexity: At most '(last1 - first1) * count' applications of the corresponding predicate.
        ///
        constexpr niebloids::search_n_fn search_n{};

        namespace niebloids
        {
            struct binary_search
            {
                /// binary_search
                ///
                /// Returns: true if there is an iterator i in the range [first last) that
                /// satisfies the corresponding conditions: !(*i < value) && !(value < *i).
                ///
                /// Complexity: At most 'log(last - first) + 2' comparisons.
                ///
                /// Note: The reason binary_search returns bool instead of an iterator is
                /// that search_n, lower_bound, or equal_range already return an iterator.
                /// However, there are arguments that binary_search should return an iterator.
                /// Note that we provide binary_search_i (STL extension) to return an iterator.
                ///
                /// To use search_n to find an item, do this:
                ///     iterator i = search_n(begin, end, 1, value);
                /// To use lower_bound to find an item, do this:
                ///     iterator i = lower_bound(begin, end, value);
                ///     if((i != last) && !(value < *i))
                ///         <use the iterator>
                /// It turns out that the above lower_bound method is as fast as binary_search
                /// would be if it returned an iterator.
                ///
                template <
                    typename ForwardIterator,
                    typename Sentinel,
                    typename T,
                    RAH2_STD::enable_if_t<
                        forward_iterator<ForwardIterator> && sentinel_for<Sentinel, ForwardIterator>>* = nullptr>
                bool operator()(ForwardIterator first, Sentinel last, T const& value) const
                {
                    // To do: This can be made slightly faster by not using lower_bound.
                    ForwardIterator i(RAH2_NS::ranges::lower_bound(first, last, value));
                    return (
                        (i != last)
                        && !(value < *i)); // Note that we always express value comparisons in terms of < or ==.
                }

                template <typename Range, typename T>
                bool operator()(Range&& range, T const& value) const
                {
                    return (*this)(RAH2_NS::ranges::begin(range), RAH2_NS::ranges::end(range), value);
                }

                /// binary_search
                ///
                /// Returns: true if there is an iterator i in the range [first last) that
                /// satisfies the corresponding conditions: compare(*i, value) == false &&
                /// compare(value, *i) == false.
                ///
                /// Complexity: At most 'log(last - first) + 2' comparisons.
                ///
                /// Note: See comments above regarding the bool return value of binary_search.
                ///
                template <
                    typename ForwardIterator,
                    typename Sentinel,
                    typename T,
                    typename Compare,
                    RAH2_STD::enable_if_t<sentinel_for<Sentinel, ForwardIterator>>* = nullptr>
                bool operator()(ForwardIterator first, Sentinel last, T const& value, Compare compare) const
                {
                    // To do: This can be made slightly faster by not using lower_bound.
                    ForwardIterator i(RAH2_NS::ranges::lower_bound(first, last, value, compare));
                    return ((i != last) && !compare(value, *i));
                }

                template <
                    typename Range,
                    typename T,
                    typename Compare,
                    RAH2_STD::enable_if_t<forward_range<Range>>* = nullptr>
                bool operator()(Range&& range, T const& value, Compare compare) const
                {
                    return (*this)(
                        RAH2_NS::ranges::begin(range),
                        RAH2_NS::ranges::end(range),
                        value,
                        RAH2_STD::move(compare));
                }
            };
        } // namespace niebloids
        constexpr niebloids::binary_search binary_search;

        namespace niebloids
        {
            struct unique_fn
            {
                template <
                    typename I,
                    typename S,
                    class Proj = RAH2_NS::details::identity,
                    typename C = RAH2_NS::ranges::equal_to,
                    RAH2_STD::enable_if_t<forward_iterator<I> && sentinel_for<S, I>>* = nullptr>
                constexpr subrange<I> operator()(I first, S last, C comp = {}, Proj proj = {}) const
                {
                    first = RAH2_NS::ranges::adjacent_find(first, last, comp, proj);
                    if (first == last)
                        return {first, first};
                    auto i{first};
                    ++first;
                    while (++first != last)
                    {
                        if (!comp(proj(*i), proj(*first)))
                            *++i = RAH2_NS::ranges::iter_move(first);
                    }
                    return {++i, first};
                }

                template <
                    typename R,
                    class Proj = RAH2_NS::details::identity,
                    typename C = RAH2_NS::ranges::equal_to,
                    RAH2_STD::enable_if_t<forward_range<R>>* = nullptr>
                constexpr borrowed_subrange_t<R> operator()(R&& r, C comp = {}, Proj proj = {}) const
                {
                    return (*this)(
                        RAH2_NS::ranges::begin(r),
                        RAH2_NS::ranges::end(r),
                        RAH2_STD::move(comp),
                        RAH2_STD::move(proj));
                }
            };
        } // namespace niebloids

        /// unique
        ///
        /// Given a sorted range, this function removes duplicated items.
        /// Note that if you have a container then you will probably want
        /// to call erase on the container with the return value if your
        /// goal is to remove the duplicated items from the container.
        ///
        /// Effects: Eliminates all but the first element from every consecutive
        /// group of equal elements referred to by the iterator i in the range
        /// [first, last) for which the following corresponding condition holds:
        /// *i == *(i - 1).
        ///
        /// Returns: The end of the resulting range.
        ///
        /// Complexity: If the range (last - first) is not empty, exactly (last - first)
        /// applications of the corresponding predicate, otherwise no applications of the predicate.
        ///
        /// Example usage:
        ///    vector<int> intArray;
        ///    ...
        ///    intArray.erase(unique(intArray.begin(), intArray.end()), intArray.end());
        ///
        constexpr niebloids::unique_fn unique{};

        namespace niebloids
        {
            struct find_end_fn
            {
                //TODO search from end when reversible
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
                constexpr RAH2_NS::ranges::subrange<I1> operator()(
                    I1 first1,
                    S1 last1,
                    I2 first2,
                    S2 last2,
                    Pred pred = {},
                    Proj1 proj1 = {},
                    Proj2 proj2 = {}) const
                {
                    if (first2 == last2)
                    {
                        auto last_it = RAH2_NS::ranges::next(first1, last1);
                        return {last_it, last_it};
                    }
                    auto result = RAH2_NS::ranges::search(
                        RAH2_STD::move(first1), last1, first2, last2, pred, proj1, proj2);

                    if (result.empty())
                        return result;

                    for (;;)
                    {
                        auto new_result = RAH2_NS::ranges::search(
                            RAH2_STD::next(result.begin()), last1, first2, last2, pred, proj1, proj2);
                        if (new_result.empty())
                            return result;
                        else
                            result = RAH2_STD::move(new_result);
                    }
                }

                template <
                    typename R1,
                    typename R2,
                    class Pred = RAH2_NS::ranges::equal_to,
                    class Proj1 = RAH2_NS::details::identity,
                    class Proj2 = RAH2_NS::details::identity,
                    RAH2_STD::enable_if_t<forward_range<R1> && forward_range<R2>>* = nullptr>
                constexpr RAH2_NS::ranges::borrowed_subrange_t<R1>
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

        // find_end
        //
        // We provide two versions here, one for a bidirectional iterators and one for
        // regular forward iterators. Given that we are searching backward, it's a bit
        // more efficient if we can use backwards iteration to implement our search,
        // though this requires an iterator that can be reversed.
        //
        constexpr niebloids::find_end_fn find_end{};

        template <class I, class O>
        using set_difference_result = in_out_result<I, O>;

        namespace niebloids
        {
            struct set_difference
            {
                /// set_difference
                ///
                /// set_difference iterates over both input ranges and copies elements present
                /// in the first range but not the second to the output range.
                ///
                /// Effects: Copies the elements of the range [first1, last1) which are not
                /// present in the range [first2, last2) to the range beginning at result.
                /// The elements in the constructed range are sorted.
                ///
                /// Requires: The input ranges must be sorted.
                /// Requires: The output range shall not overlap with either of the original ranges.
                ///
                /// Returns: The end of the output range.
                ///
                /// Complexity: At most (2 * ((last1 - first1) + (last2 - first2)) - 1) comparisons.
                ///
                template <
                    typename InputIterator1,
                    typename Sentinel1,
                    typename InputIterator2,
                    typename Sentinel2,
                    typename OutputIterator,
                    RAH2_STD::enable_if_t<
                        input_iterator<InputIterator1> && sentinel_for<Sentinel1, InputIterator1>
                        && input_iterator<InputIterator2> && sentinel_for<Sentinel2, InputIterator2>>* = nullptr>
                set_difference_result<InputIterator1, OutputIterator> operator()(
                    InputIterator1 first1,
                    Sentinel1 last1,
                    InputIterator2 first2,
                    Sentinel2 last2,
                    OutputIterator result) const
                {
                    while ((first1 != last1) && (first2 != last2))
                    {
                        if (*first1 < *first2)
                        {
                            *result = *first1;
                            ++first1;
                            ++result;
                        }
                        else if (*first2 < *first1)
                            ++first2;
                        else
                        {
                            ++first1;
                            ++first2;
                        }
                    }

                    return RAH2_NS::ranges::copy(first1, last1, result);
                }

                template <typename InputRange1, typename InputRange2, typename OutputIterator>
                set_difference_result<iterator_t<InputRange1>, OutputIterator>
                operator()(InputRange1&& range1, InputRange2&& range2, OutputIterator result) const
                {
                    return (*this)(
                        RAH2_NS::ranges::begin(range1),
                        RAH2_NS::ranges::end(range1),
                        RAH2_NS::ranges::begin(range2),
                        RAH2_NS::ranges::end(range2),
                        RAH2_STD::move(result));
                }

                template <
                    typename InputIterator1,
                    typename Sentinel1,
                    typename InputIterator2,
                    typename Sentinel2,
                    typename OutputIterator,
                    typename Compare>
                OutputIterator operator()(
                    InputIterator1 first1,
                    Sentinel1 last1,
                    InputIterator2 first2,
                    Sentinel2 last2,
                    OutputIterator result,
                    Compare compare) const
                {
                    while ((first1 != last1) && (first2 != last2))
                    {
                        if (compare(*first1, *first2))
                        {
                            RAH2_VALIDATE_COMPARE(!compare(
                                *first2, *first1)); // Validate that the compare function is sane.
                            *result = *first1;
                            ++first1;
                            ++result;
                        }
                        else if (compare(*first2, *first1))
                        {
                            RAH2_VALIDATE_COMPARE(!compare(
                                *first1, *first2)); // Validate that the compare function is sane.
                            ++first2;
                        }
                        else
                        {
                            ++first1;
                            ++first2;
                        }
                    }

                    return RAH2_NS::ranges::copy(first1, last1, result);
                }
            };
        } // namespace niebloids
        constexpr niebloids::set_difference set_difference;

        template <class I1, class I2, class O>
        using set_symmetric_difference_result = RAH2_NS::ranges::in_in_out_result<I1, I2, O>;

        namespace niebloids
        {
            struct set_symmetric_difference
            {
                /// set_symmetric_difference
                ///
                /// set_difference iterates over both input ranges and copies elements present
                /// in the either range but not the other to the output range.
                ///
                /// Effects: Copies the elements of the range [first1, last1) which are not
                /// present in the range [first2, last2), and the elements of the range [first2, last2)
                /// which are not present in the range [first1, last1) to the range beginning at result.
                /// The elements in the constructed range are sorted.
                ///
                /// Requires: The input ranges must be sorted.
                /// Requires: The resulting range shall not overlap with either of the original ranges.
                ///
                /// Returns: The end of the constructed range.
                ///
                /// Complexity: At most (2 * ((last1 - first1) + (last2 - first2)) - 1) comparisons.
                ///
                template <typename InputIterator1, typename Sentinel1, typename InputIterator2, typename Sentinel2, typename OutputIterator>
                set_symmetric_difference_result<InputIterator1, InputIterator2, OutputIterator>
                operator()(
                    InputIterator1 first1,
                    Sentinel1 last1,
                    InputIterator2 first2,
                    Sentinel2 last2,
                    OutputIterator result) const
                {
                    while ((first1 != last1) && (first2 != last2))
                    {
                        if (*first1 < *first2)
                        {
                            *result = *first1;
                            ++first1;
                            ++result;
                        }
                        else if (*first2 < *first1)
                        {
                            *result = *first2;
                            ++first2;
                            ++result;
                        }
                        else
                        {
                            ++first1;
                            ++first2;
                        }
                    }
                    auto res1 = RAH2_NS::ranges::copy(
                        RAH2_STD::move(first1), RAH2_STD::move(last1), RAH2_STD::move(result));
                    auto res2 = RAH2_NS::ranges::copy(
                        RAH2_STD::move(first2), RAH2_STD::move(last2), RAH2_STD::move(res1.out));
                    return {
                        RAH2_STD::move(res1.in), RAH2_STD::move(res2.in), RAH2_STD::move(res2.out)};
                }

                template <typename InputRange1, typename InputRange2, typename OutputIterator>
                set_symmetric_difference_result<
                    RAH2_NS::ranges::borrowed_iterator_t<InputRange1>,
                    RAH2_NS::ranges::borrowed_iterator_t<InputRange2>,
                    OutputIterator>
                operator()(InputRange1&& range1, InputRange2&& range2, OutputIterator result) const
                {
                    return (*this)(
                        RAH2_NS::ranges::begin(range1),
                        RAH2_NS::ranges::end(range1),
                        RAH2_NS::ranges::begin(range2),
                        RAH2_NS::ranges::end(range2),
                        RAH2_STD::move(result));
                }

                template <
                    typename InputIterator1,
                    typename Sentinel1,
                    typename InputIterator2,
                    typename Sentinel2,
                    typename OutputIterator,
                    typename Compare>
                set_symmetric_difference_result<InputIterator1, InputIterator2, OutputIterator>
                operator()(
                    InputIterator1 first1,
                    Sentinel1 last1,
                    InputIterator2 first2,
                    Sentinel2 last2,
                    OutputIterator result,
                    Compare compare) const
                {
                    while ((first1 != last1) && (first2 != last2))
                    {
                        if (compare(*first1, *first2))
                        {
                            RAH2_VALIDATE_COMPARE(!compare(
                                *first2, *first1)); // Validate that the compare function is sane.
                            *result = *first1;
                            ++first1;
                            ++result;
                        }
                        else if (compare(*first2, *first1))
                        {
                            RAH2_VALIDATE_COMPARE(!compare(
                                *first1, *first2)); // Validate that the compare function is sane.
                            *result = *first2;
                            ++first2;
                            ++result;
                        }
                        else
                        {
                            ++first1;
                            ++first2;
                        }
                    }

                    auto res1 = RAH2_NS::ranges::copy(
                        RAH2_STD::move(first1), RAH2_STD::move(last1), RAH2_STD::move(result));
                    auto res2 = RAH2_NS::ranges::copy(
                        RAH2_STD::move(first2), RAH2_STD::move(last2), RAH2_STD::move(res1.out));
                    return {
                        RAH2_STD::move(res1.in), RAH2_STD::move(res2.in), RAH2_STD::move(res2.out)};
                }

                template <typename InputRange1, typename InputRange2, typename OutputIterator, typename Compare>
                set_symmetric_difference_result<
                    RAH2_NS::ranges::borrowed_iterator_t<InputRange1>,
                    RAH2_NS::ranges::borrowed_iterator_t<InputRange2>,
                    OutputIterator>
                operator()(
                    InputRange1&& range1,
                    InputRange2&& range2,
                    OutputIterator result,
                    Compare compare) const
                {
                    return (*this)(
                        RAH2_NS::ranges::begin(range1),
                        RAH2_NS::ranges::end(range1),
                        RAH2_NS::ranges::begin(range2),
                        RAH2_NS::ranges::end(range2),
                        RAH2_STD::move(result),
                        RAH2_STD::move(compare));
                }
            };
        } // namespace niebloids
        constexpr niebloids::set_symmetric_difference set_symmetric_difference;

        template <class I1, class I2, class O>
        using set_intersection_result = in_in_out_result<I1, I2, O>;

        namespace niebloids
        {
            struct set_intersection
            {
                /// set_intersection
                ///
                /// set_intersection over both ranges and copies elements present in
                /// both ranges to the output range.
                ///
                /// Effects: Constructs a sorted intersection of the elements from the
                /// two ranges; that is, the set of elements that are present in both of the ranges.
                ///
                /// Requires: The input ranges must be sorted.
                /// Requires: The resulting range shall not overlap with either of the original ranges.
                ///
                /// Returns: The end of the constructed range.
                ///
                /// Complexity: At most 2 * ((last1 - first1) + (last2 - first2)) - 1)  comparisons.
                ///
                /// Note: The copying operation is stable; if an element is present in both ranges,
                /// the one from the first range is copied.
                ///
                template <typename InputIterator1, typename Sentinel1, typename InputIterator2, typename Sentinel2, typename OutputIterator>
                set_intersection_result<InputIterator1, InputIterator2, OutputIterator> operator()(
                    InputIterator1 first1,
                    Sentinel1 last1,
                    InputIterator2 first2,
                    Sentinel2 last2,
                    OutputIterator result) const
                {
                    while ((first1 != last1) && (first2 != last2))
                    {
                        if (*first1 < *first2)
                            ++first1;
                        else if (*first2 < *first1)
                            ++first2;
                        else
                        {
                            *result = *first1;
                            ++first1;
                            ++first2;
                            ++result;
                        }
                    }

                    return {first1, first2, result};
                }

                template <typename InputRange1, typename InputRange2, typename OutputIterator>
                set_intersection_result<iterator_t<InputRange1>, iterator_t<InputRange2>, OutputIterator>
                operator()(InputRange1&& range1, InputRange2&& range2, OutputIterator result) const
                {
                    return (*this)(
                        RAH2_NS::ranges::begin(range1),
                        RAH2_NS::ranges::end(range1),
                        RAH2_NS::ranges::begin(range2),
                        RAH2_NS::ranges::end(range2),
                        RAH2_STD::move(result));
                }

                template <
                    typename InputIterator1,
                    typename Sentinel1,
                    typename InputIterator2,
                    typename Sentinel2,
                    typename OutputIterator,
                    typename Compare>
                OutputIterator operator()(
                    InputIterator1 first1,
                    Sentinel1 last1,
                    InputIterator2 first2,
                    Sentinel2 last2,
                    OutputIterator result,
                    Compare compare) const
                {
                    while ((first1 != last1) && (first2 != last2))
                    {
                        if (compare(*first1, *first2))
                        {
                            RAH2_VALIDATE_COMPARE(!compare(
                                *first2, *first1)); // Validate that the compare function is sane.
                            ++first1;
                        }
                        else if (compare(*first2, *first1))
                        {
                            RAH2_VALIDATE_COMPARE(!compare(
                                *first1, *first2)); // Validate that the compare function is sane.
                            ++first2;
                        }
                        else
                        {
                            *result = *first1;
                            ++first1;
                            ++first2;
                            ++result;
                        }
                    }

                    return result;
                }
            };
        } // namespace niebloids
        constexpr niebloids::set_intersection set_intersection;

        template <class I1, class I2, class O>
        using set_union_result = RAH2_NS::ranges::in_in_out_result<I1, I2, O>;

        namespace niebloids
        {
            struct set_union
            {
                /// set_union
                ///
                /// set_union iterates over both ranges and copies elements present in
                /// both ranges to the output range.
                ///
                /// Effects: Constructs a sorted union of the elements from the two ranges;
                /// that is, the set of elements that are present in one or both of the ranges.
                ///
                /// Requires: The input ranges must be sorted.
                /// Requires: The resulting range shall not overlap with either of the original ranges.
                ///
                /// Returns: The end of the constructed range.
                ///
                /// Complexity: At most (2 * ((last1 - first1) + (last2 - first2)) - 1) comparisons.
                ///
                /// Note: The copying operation is stable; if an element is present in both ranges,
                /// the one from the first range is copied.
                ///
                template <typename InputIterator1, typename Sentinel1, typename InputIterator2, typename Sentinel2, typename OutputIterator>
                set_union_result<InputIterator1, InputIterator2, OutputIterator> operator()(
                    InputIterator1 first1,
                    Sentinel1 last1,
                    InputIterator2 first2,
                    Sentinel2 last2,
                    OutputIterator result) const
                {
                    while ((first1 != last1) && (first2 != last2))
                    {
                        if (*first1 < *first2)
                        {
                            *result = *first1;
                            ++first1;
                        }
                        else if (*first2 < *first1)
                        {
                            *result = *first2;
                            ++first2;
                        }
                        else
                        {
                            *result = *first1;
                            ++first1;
                            ++first2;
                        }
                        ++result;
                    }

                    auto res1 = RAH2_NS::ranges::copy(
                        RAH2_STD::move(first1), RAH2_STD::move(last1), RAH2_STD::move(result));
                    auto res2 = RAH2_NS::ranges::copy(
                        RAH2_STD::move(first2), RAH2_STD::move(last2), RAH2_STD::move(res1.out));
                    return {
                        RAH2_STD::move(res1.in), RAH2_STD::move(res2.in), RAH2_STD::move(res2.out)};
                }

                template <typename InputRange1, typename InputRange2, typename OutputIterator>
                set_union_result<
                    RAH2_NS::ranges::borrowed_iterator_t<InputRange1>,
                    RAH2_NS::ranges::borrowed_iterator_t<InputRange2>,
                    OutputIterator>
                operator()(InputRange1&& range1, InputRange2&& range2, OutputIterator result) const
                {
                    return (*this)(
                        RAH2_NS::ranges::begin(range1),
                        RAH2_NS::ranges::end(range1),
                        RAH2_NS::ranges::begin(range2),
                        RAH2_NS::ranges::end(range2),
                        RAH2_STD::move(result));
                }

                template <
                    typename InputIterator1,
                    typename Sentinel1,
                    typename InputIterator2,
                    typename Sentinel2,
                    typename OutputIterator,
                    typename Compare>
                set_union_result<InputIterator1, InputIterator2, OutputIterator> operator()(
                    InputIterator1 first1,
                    Sentinel1 last1,
                    InputIterator2 first2,
                    Sentinel2 last2,
                    OutputIterator result,
                    Compare compare) const
                {
                    while ((first1 != last1) && (first2 != last2))
                    {
                        if (compare(*first1, *first2))
                        {
                            RAH2_VALIDATE_COMPARE(!compare(
                                *first2, *first1)); // Validate that the compare function is sane.
                            *result = *first1;
                            ++first1;
                        }
                        else if (compare(*first2, *first1))
                        {
                            RAH2_VALIDATE_COMPARE(!compare(
                                *first1, *first2)); // Validate that the compare function is sane.
                            *result = *first2;
                            ++first2;
                        }
                        else
                        {
                            *result = *first1;
                            ++first1;
                            ++first2;
                        }
                        ++result;
                    }

                    auto res1 = RAH2_NS::ranges::copy(
                        RAH2_STD::move(first1), RAH2_STD::move(last1), RAH2_STD::move(result));
                    auto res2 = RAH2_NS::ranges::copy(
                        RAH2_STD::move(first2), RAH2_STD::move(last2), RAH2_STD::move(res1.out));
                    return {
                        RAH2_STD::move(res1.in), RAH2_STD::move(res2.in), RAH2_STD::move(res2.out)};
                }

                template <typename InputRange1, typename InputRange2, typename OutputIterator, typename Compare>
                set_union_result<
                    RAH2_NS::ranges::borrowed_iterator_t<InputRange1>,
                    RAH2_NS::ranges::borrowed_iterator_t<InputRange2>,
                    OutputIterator>
                operator()(
                    InputRange1&& range1,
                    InputRange2&& range2,
                    OutputIterator result,
                    Compare compare) const
                {
                    return (*this)(
                        RAH2_NS::ranges::begin(range1),
                        RAH2_NS::ranges::end(range1),
                        RAH2_NS::ranges::begin(range2),
                        RAH2_NS::ranges::end(range2),
                        RAH2_STD::move(result),
                        RAH2_STD::move(compare));
                }
            };
        } // namespace niebloids
        constexpr niebloids::set_union set_union;

        namespace niebloids
        {
            /// is_permutation
            ///
            struct is_permutation_fn
            {
                template <
                    typename I1,
                    typename S1,
                    typename I2,
                    typename S2,
                    class Proj1 = RAH2_NS::details::identity,
                    class Proj2 = RAH2_NS::details::identity,
                    typename Pred = RAH2_NS::ranges::equal_to,
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
                    // skip common prefix
                    auto ret = RAH2_NS::ranges::mismatch(
                        first1,
                        last1,
                        first2,
                        last2,
                        RAH2_STD::ref(pred),
                        RAH2_STD::ref(proj1),
                        RAH2_STD::ref(proj2));
                    first1 = ret.in1, first2 = ret.in2;

                    // iterate over the rest, counting how many times each element
                    // from [first1, last1) appears in [first2, last2)
                    for (auto i{first1}; i != last1; ++i)
                    {
                        auto const i_proj{proj1(*i)};
                        auto i_cmp = [&](auto&& t)
                        {
                            return pred(i_proj, RAH2_STD::forward<decltype(t)>(t));
                        };

                        if (i != RAH2_NS::ranges::find_if(first1, i, i_cmp, proj1))
                            continue; // this *i has been checked

                        auto const m{RAH2_NS::ranges::count_if(first2, last2, i_cmp, proj2)};
                        if (m == 0 or m != RAH2_NS::ranges::count_if(i, last1, i_cmp, proj1))
                            return false;
                    }
                    return true;
                }

                template <
                    typename R1,
                    typename R2,
                    class Proj1 = RAH2_NS::details::identity,
                    class Proj2 = RAH2_NS::details::identity,
                    typename Pred = RAH2_NS::ranges::equal_to,
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
        constexpr niebloids::is_permutation_fn is_permutation{};

        template <class I>
        using next_permutation_result = RAH2_NS::ranges::in_found_result<I>;

        namespace niebloids
        {
            struct next_permutation
            {
                /// next_permutation
                ///
                /// mutates the range [first, last) to the next permutation. Returns true if the
                /// new range is not the final permutation (sorted like the starting permutation).
                /// Permutations start with a sorted range, and false is returned when next_permutation
                /// results in the initial sorted range, or if the range has <= 1 element.
                /// Note that elements are compared by operator < (as usual) and that elements deemed
                /// equal via this are not rearranged.
                ///
                /// http://marknelson.us/2002/03/01/next-permutation/
                /// Basically we start with an ordered range and reverse it's order one specifically
                /// chosen swap and reverse at a time. It happens that this require going through every
                /// permutation of the range. We use the same variable names as the document above.
                ///
                /// To consider: Significantly improved permutation/combination functionality:
                ///    http://home.roadrunner.com/~hinnant/combinations.html
                ///
                /// Example usage:
                ///     vector<int> intArray;
                ///     // <populate intArray>
                ///     sort(intArray.begin(), intArray.end());
                ///     do {
                ///         // <do something with intArray>
                ///     } while(next_permutation(intArray.begin(), intArray.end()));
                ///

                template <
                    typename BidirectionalIterator,
                    typename Sentinel,
                    typename Compare = RAH2_NS::ranges::less,
                    RAH2_STD::enable_if_t<
                        bidirectional_iterator<BidirectionalIterator>
                        && sentinel_for<Sentinel, BidirectionalIterator>>* = nullptr>
                next_permutation_result<BidirectionalIterator>
                operator()(BidirectionalIterator first, Sentinel last, Compare compare = {}) const
                {
                    if (first != last) // If there is anything in the range...
                    {
                        auto i = RAH2_NS::ranges::next(first, last);
                        auto lasti = i;
                        if (first != --i) // If the range has more than one item...
                        {
                            for (;;)
                            {
                                BidirectionalIterator ii(i), j;

                                if (compare(*--i, *ii)) // Find two consecutive values where the first is less than the second.
                                {
                                    j = last;
                                    while (!compare(
                                        *i, *--j)) // Find the final value that's greater than the first (it may be equal to the second).
                                    {
                                    }
                                    RAH2_NS::ranges::iter_swap(i, j); // Swap the first and the final.
                                    RAH2_NS::ranges::reverse(
                                        ii, last); // Reverse the ranget from second to last.
                                    return {RAH2_STD::move(lasti), true};
                                }

                                if (i == first) // There are no two consecutive values where the first is less than the second, meaning the range is in reverse order. The reverse ordered range is always the last permutation.
                                {
                                    RAH2_NS::ranges::reverse(first, last);
                                    break; // We are done.
                                }
                            }
                        }
                    }

                    return {RAH2_STD::move(first), false};
                }

                template <
                    typename BidirectionalRange,
                    typename Compare = RAH2_NS::ranges::less,
                    RAH2_STD::enable_if_t<bidirectional_range<BidirectionalRange>>* = nullptr>
                constexpr next_permutation_result<borrowed_iterator_t<BidirectionalRange>>
                operator()(BidirectionalRange&& range, Compare compare = {}) const
                {
                    return (*this)(
                        RAH2_NS::ranges::begin(range),
                        RAH2_NS::ranges::end(range),
                        RAH2_STD::move(compare));
                }
            };
        } // namespace niebloids
        constexpr niebloids::next_permutation next_permutation;

        /// rotate
        ///
        /// Effects: For each non-negative integer i < (last - first), places the element from the
        /// position first + i into position first + (i + (last - middle)) % (last - first).
        ///
        /// Returns: first + (last - middle). That is, returns where first went to.
        ///
        /// Remarks: This is a left rotate.
        ///
        /// Requires: [first,middle) and [middle,last) shall be valid ranges. ForwardIterator shall
        /// satisfy the requirements of ValueSwappable (17.6.3.2). The type of *first shall satisfy
        /// the requirements of MoveConstructible (Table 20) and the requirements of MoveAssignable.
        ///
        /// Complexity: At most last - first swaps.
        ///
        /// Note: While rotate works on ForwardIterators (e.g. slist) and BidirectionalIterators (e.g. list),
        /// you can get much better performance (O(1) instead of O(n)) with slist and list rotation by
        /// doing splice operations on those lists instead of calling this rotate function.
        ///
        /// http://www.cs.bell-labs.com/cm/cs/pearls/s02b.pdf / http://books.google.com/books?id=kse_7qbWbjsC&pg=PA14&lpg=PA14&dq=Programming+Pearls+flipping+hands
        /// http://books.google.com/books?id=tjOlkl7ecVQC&pg=PA189&lpg=PA189&dq=stepanov+Elements+of+Programming+rotate
        /// http://stackoverflow.com/questions/21160875/why-is-stdrotate-so-fast
        ///
        /// Strategy:
        ///     - We handle the special case of (middle == first) and (middle == last) no-ops
        ///       up front in the main rotate entry point.
        ///     - There's a basic ForwardIterator implementation (rotate_general_impl) which is
        ///       a fallback implementation that's not as fast as others but works for all cases.
        ///     - There's a slightly better BidirectionalIterator implementation.
        ///     - We have specialized versions for rotating elements that are is_trivially_move_assignable.
        ///       These versions will use memmove for when we have a RandomAccessIterator.
        ///     - We have a specialized version for rotating by only a single position, as that allows us
        ///       (with any iterator type) to avoid a lot of logic involved with algorithms like "flipping hands"
        ///       and achieve near optimal O(n) behavior. it turns out that rotate-by-one is a common use
        ///       case in practice.
        ///
        namespace details
        {
            template <typename ForwardIterator, typename ForwardSentinel>
            subrange<ForwardIterator>
            rotate_general_impl(ForwardIterator first, ForwardIterator middle, ForwardSentinel last)
            {
                using RAH2_STD::swap;

                ForwardIterator current = middle;

                do
                {
                    swap(*first++, *current++);

                    if (first == middle)
                        middle = current;
                } while (current != last);

                ForwardIterator result = first;
                current = middle;

                while (current != last)
                {
                    swap(*first++, *current++);

                    if (first == middle)
                        middle = current;
                    else if (current == last)
                        current = middle;
                }

                return {result, current}; // result points to first + (last - middle).
            }

            template <typename ForwardIterator, typename ForwardSentinel>
            subrange<ForwardIterator> move_rotate_left_by_one(ForwardIterator first, ForwardSentinel last)
            {
                using value_type = typename RAH2_STD::iterator_traits<ForwardIterator>::value_type;

                value_type temp(RAH2_STD::move(*first));
                // Note that while our template type is BidirectionalIterator, if the actual
                auto result = RAH2_NS::ranges::move(RAH2_STD::next(first), last, first).out;
                // iterator is a RandomAccessIterator then this move will be a memmove for trivial types.
                *result = RAH2_STD::move(temp);

                auto back = result;
                ++back;
                return {result, back}; // result points to the final element in the range.
            }

            template <typename BidirectionalIterator, typename Sentinel>
            subrange<BidirectionalIterator>
            move_rotate_right_by_one(BidirectionalIterator first, Sentinel last)
            {
                using value_type =
                    typename RAH2_STD::iterator_traits<BidirectionalIterator>::value_type;

                auto last2 = RAH2_NS::ranges::next(first, last);
                BidirectionalIterator beforeLast = RAH2_STD::prev(last2);
                value_type temp(RAH2_STD::move(*beforeLast));
                // Note that while our template type is BidirectionalIterator, if the actual
                BidirectionalIterator result =
                    RAH2_NS::ranges::move_backward(first, beforeLast, last).out;
                // iterator is a RandomAccessIterator then this move will be a memmove for trivial types.
                *first = RAH2_STD::move(temp);

                // result points to the first element in the range.
                return {RAH2_STD::move(result), RAH2_STD::move(last2)};
            }

            template <typename /*IteratorCategory*/, bool /*is_trivially_move_assignable*/>
            struct rotate_helper
            {
                template <typename ForwardIterator, typename ForwardSentinel>
                static subrange<ForwardIterator>
                rotate_impl(ForwardIterator first, ForwardIterator middle, ForwardSentinel last)
                {
                    return rotate_general_impl(first, middle, last);
                }
            };

            template <>
            struct rotate_helper<RAH2_ITC_NS::forward_iterator_tag, true>
            {
                template <typename ForwardIterator, typename ForwardSentinel>
                static subrange<ForwardIterator>
                rotate_impl(ForwardIterator first, ForwardIterator middle, ForwardSentinel last)
                {
                    if (RAH2_STD::next(first)
                        == middle) // If moving trivial types by a single element, memcpy is fast for that case.
                        return move_rotate_left_by_one(first, last);
                    return rotate_general_impl(first, middle, last);
                }
            };

            template <>
            struct rotate_helper<RAH2_ITC_NS::bidirectional_iterator_tag, false>
            {
                template <typename BidirectionalIterator, typename Sentinel>
                static subrange<BidirectionalIterator>
                rotate_impl(BidirectionalIterator first, BidirectionalIterator middle, Sentinel last)
                {
                    return rotate_general_impl(first, middle, last);
                } // rotate_general_impl outperforms the flipping hands algorithm.
            };

            template <>
            struct rotate_helper<RAH2_ITC_NS::bidirectional_iterator_tag, true>
            {
                template <typename BidirectionalIterator, typename Sentinel>
                static subrange<BidirectionalIterator>
                rotate_impl(BidirectionalIterator first, BidirectionalIterator middle, Sentinel last)
                {
                    if (RAH2_STD::next(first)
                        == middle) // If moving trivial types by a single element, memcpy is fast for that case.
                        return move_rotate_left_by_one(first, last);
                    if (RAH2_STD::next(middle) == last)
                        return move_rotate_right_by_one(first, last);
                    return rotate_general_impl(first, middle, last);
                }
            };

            template <typename Integer>
            Integer greatest_common_divisor(Integer x, Integer y)
            {
                do
                {
                    Integer t = (x % y);
                    x = y;
                    y = t;
                } while (y);

                return x;
            }

            template <>
            struct rotate_helper<RAH2_ITC_NS::random_access_iterator_tag, false>
            {
                // This is the juggling algorithm, using move operations.
                // In practice this implementation is about 25% faster than rotate_general_impl. We may want to
                // consider sticking with just rotate_general_impl and avoid the code generation of this function.
                template <typename RandomAccessIterator, typename Sentinel>
                static subrange<RandomAccessIterator>
                rotate_impl(RandomAccessIterator first, RandomAccessIterator middle, Sentinel last)
                {
                    using difference_type =
                        typename RAH2_STD::iterator_traits<RandomAccessIterator>::difference_type;
                    using value_type =
                        typename RAH2_STD::iterator_traits<RandomAccessIterator>::value_type;

                    difference_type const m1 = (middle - first);
                    difference_type const m2 = (last - middle);
                    difference_type const g = greatest_common_divisor(m1, m2);
                    value_type temp;

                    for (RandomAccessIterator p = first + g; p != first;)
                    {
                        temp = RAH2_STD::move(*--p);
                        RandomAccessIterator p1 = p;
                        RandomAccessIterator p2 = p + m1;
                        do
                        {
                            *p1 = RAH2_STD::move(*p2);
                            p1 = p2;
                            difference_type const d = (last - p2);

                            if (m1 < d)
                                p2 += m1;
                            else
                                p2 = first + (m1 - d);
                        } while (p2 != p);

                        *p1 = RAH2_STD::move(temp);
                    }

                    return {first + m2, last};
                }
            };

            template <>
            struct rotate_helper<RAH2_ITC_NS::random_access_iterator_tag, true>
            {
                // Experiments were done which tested the performance of using an intermediate buffer
                // to do memcpy's to as opposed to executing a swapping algorithm. It turns out this is
                // actually slower than even rotate_general_impl, partly because the average case involves
                // memcpy'ing a quarter of the element range twice. Experiments were done with various kinds
                // of PODs with various element counts.

                template <typename RandomAccessIterator, typename Sentinel>
                static subrange<RandomAccessIterator>
                rotate_impl(RandomAccessIterator first, RandomAccessIterator middle, Sentinel last)
                {
                    if (RAH2_STD::next(first)
                        == middle) // If moving trivial types by a single element, memcpy is fast for that case.
                        return move_rotate_left_by_one(first, last);
                    if (RAH2_STD::next(middle) == last)
                        return move_rotate_right_by_one(first, last);
                    if ((last - first) < 32) // For small ranges rotate_general_impl is faster.
                        return rotate_general_impl(first, middle, last);
                    return rotate_helper<RAH2_ITC_NS::random_access_iterator_tag, false>::rotate_impl(
                        first, middle, last);
                }
            };

        } // namespace details

        namespace niebloids
        {
            struct rotate
            {
                template <typename ForwardIterator, typename ForwardSentinel>
                subrange<ForwardIterator>
                operator()(ForwardIterator first, ForwardIterator middle, ForwardSentinel last) const
                {
                    if (middle != first)
                    {
                        if (middle != last)
                        {
                            using IC = RAH2_NS::details::iterator_concept<ForwardIterator>;
                            using value_type =
                                typename RAH2_STD::iterator_traits<ForwardIterator>::value_type;

                            return details::rotate_helper < IC,
                                   RAH2_STD::is_trivially_move_assignable<value_type>::value
#if not RAH2_CPP20
                                       || // This is the best way of telling if we can move types via memmove, but without a conforming C++11 compiler it usually returns false.
                                       RAH2_STD::is_pod<value_type>::value
#endif
                                       || // This is a more conservative way of telling if we can move types via memmove, and most compilers support it, but it doesn't have as full of coverage as is_trivially_move_assignable.
                                       RAH2_NS::is_scalar<value_type>::value
                                           > // This is the most conservative means and works with all compilers, but works only for scalars.
                                           ::rotate_impl(first, middle, last);
                        }

                        return {RAH2_STD::move(first), RAH2_STD::move(middle)};
                    }
                    auto last_it = RAH2_NS::ranges::next(first, last);
                    return {last_it, last_it};
                }

                template <typename ForwardRange>
                borrowed_subrange_t<ForwardRange>
                operator()(ForwardRange&& range, iterator_t<ForwardRange> middle) const
                {
                    return (*this)(
                        RAH2_NS::ranges::begin(range), middle, RAH2_NS::ranges::end(range));
                }
            };
        } // namespace niebloids
        constexpr niebloids::rotate rotate;

        template <class I, class O>
        using rotate_copy_result = in_out_result<I, O>;

        namespace niebloids
        {
            struct rotate_copy_fn
            {
                template <
                    typename I,
                    typename S,
                    typename O,
                    RAH2_STD::enable_if_t<
                        RAH2_NS::forward_iterator<I> && RAH2_NS::sentinel_for<S, I>
                        && RAH2_NS::weakly_incrementable<O> && RAH2_NS::indirectly_copyable<I, O>>* = nullptr>
                constexpr RAH2_NS::ranges::rotate_copy_result<I, O>
                operator()(I first, I middle, S last, O result) const
                {
                    auto c1{RAH2_NS::ranges::copy(
                        middle, RAH2_STD::move(last), RAH2_STD::move(result))};
                    auto c2{RAH2_NS::ranges::copy(
                        RAH2_STD::move(first), RAH2_STD::move(middle), RAH2_STD::move(c1.out))};
                    return {RAH2_STD::move(c1.in), RAH2_STD::move(c2.out)};
                }

                template <
                    typename R,
                    typename O,
                    RAH2_STD::enable_if_t<
                        RAH2_NS::ranges::forward_range<R> && RAH2_NS::weakly_incrementable<O>
                        && RAH2_NS::indirectly_copyable<RAH2_NS::ranges::iterator_t<R>, O>>* = nullptr>
                constexpr RAH2_NS::ranges::rotate_copy_result<RAH2_NS::ranges::borrowed_iterator_t<R>, O>
                operator()(R&& r, RAH2_NS::ranges::iterator_t<R> middle, O result) const
                {
                    return (*this)(
                        RAH2_NS::ranges::begin(r),
                        RAH2_STD::move(middle),
                        RAH2_NS::ranges::end(r),
                        RAH2_STD::move(result));
                }
            };
        } // namespace niebloids

        /// rotate_copy
        ///
        /// Similar to rotate except writes the output to the OutputIterator and
        /// returns an OutputIterator to the element past the last element copied
        /// (i.e. result + (last - first))
        ///
        constexpr niebloids::rotate_copy_fn rotate_copy{};

        namespace niebloids
        {
            /// clamp
            ///
            /// Returns a reference to a clamped value within the range of [lo, hi].
            ///
            /// http://en.cppreference.com/w/cpp/algorithm/clamp
            ///
            struct clamp_fn
            {
                template <class T, class Proj = RAH2_NS::details::identity, typename Comp = RAH2_STD::less<>>
                constexpr T const&
                operator()(T const& v, T const& lo, T const& hi, Comp comp = {}, Proj proj = {}) const
                {
                    RAH2_ASSERT(!comp(hi, lo));
                    auto&& pv = proj(v);

                    return comp(RAH2_STD::forward<decltype(pv)>(pv), proj(lo)) ? lo :
                           comp(proj(hi), RAH2_STD::forward<decltype(pv)>(pv)) ? hi :
                                                                                 v;
                }
            };
        } // namespace niebloids
        constexpr niebloids::clamp_fn clamp;

        namespace niebloids
        {
            struct fill_fn
            {
                // TODO use the eastl version when possible

                template <
                    class T,
                    class O,
                    class S,
                    RAH2_STD::enable_if_t<output_iterator<O, T> && sentinel_for<S, O>>* = nullptr>
                constexpr O operator()(O first, S last, T const& value) const
                {
                    while (first != last)
                        *first++ = value;

                    return first;
                }

                template <class T, class R, RAH2_STD::enable_if_t<output_range<R, T>>* = nullptr>
                constexpr RAH2_NS::ranges::iterator_t<R> operator()(R&& r, T const& value) const
                {
                    return (*this)(RAH2_NS::ranges::begin(r), RAH2_NS::ranges::end(r), value);
                }
            };
        } // namespace niebloids
        constexpr niebloids::fill_fn fill;

        namespace niebloids
        {
            struct fill_n_fn
            {
                template <typename T, typename O, RAH2_STD::enable_if_t<output_iterator<O, T const&>>* = nullptr>
                constexpr O operator()(O first, RAH2_NS::iter_difference_t<O> n, T const& value) const
                {
                    for (RAH2_NS::iter_difference_t<O> i{}; i != n; ++first, ++i)
                        *first = value;
                    return first;
                }
            };
        } // namespace niebloids
        constexpr niebloids::fill_n_fn fill_n{};
        namespace niebloids
        {
            struct destroy_at_fn
            {
                template <
                    typename T, // RAH2_STD::destructible
                    RAH2_STD::enable_if_t<RAH2_STD::is_array<T>::value>* = nullptr>
                constexpr void operator()(T* p) const noexcept
                {
                    for (auto& elem : *p)
                        operator()(RAH2_STD::addressof(elem));
                }
                template <
                    typename T, // RAH2_STD::destructible
                    RAH2_STD::enable_if_t<!RAH2_STD::is_array<T>::value>* = nullptr>
                constexpr void operator()(T* p) const noexcept
                {
                    p->~T();
                }
            };
        } // namespace niebloids
        constexpr niebloids::destroy_at_fn destroy_at{};
        namespace niebloids
        {
            struct construct_at_fn
            {
                template <class T, class... Args>
                constexpr T* operator()(T* p, Args&&... args) const
                {
                    return ::new (static_cast<void*>(p)) T(RAH2_STD::forward<Args>(args)...);
                }
            };
        } // namespace niebloids
        constexpr niebloids::construct_at_fn construct_at{};
        namespace niebloids
        {
            struct destroy_fn
            {
                template <
                    typename I, // no-throw-input-iterator
                    typename S // no-throw-sentinel-for<I>
                    >
                // requires RAH2_STD::destructible<RAH2_STD::iter_value_t<I>>
                constexpr I operator()(I first, S last) const noexcept
                {
                    for (; first != last; ++first)
                        RAH2_NS::ranges::destroy_at(RAH2_STD::addressof(*first));
                    return first;
                }

                template <typename R // no-throw-input-range
                          >
                // requires RAH2_STD::destructible<RAH2_STD::ranges::range_value_t<R>>
                constexpr RAH2_NS::ranges::borrowed_iterator_t<R> operator()(R&& r) const noexcept
                {
                    return operator()(RAH2_NS::ranges::begin(r), RAH2_NS::ranges::end(r));
                }
            };
        } // namespace niebloids
        constexpr niebloids::destroy_fn destroy{};
        namespace niebloids
        {
            struct destroy_n_fn
            {
                template <typename I // no-throw-input-iterator
                          >
                // requires RAH2_STD::destructible<RAH2_STD::iter_value_t<I>>
                constexpr I operator()(I first, RAH2_NS::iter_difference_t<I> n) const noexcept
                {
                    for (; n != 0; (void)++first, --n)
                        RAH2_NS::ranges::destroy_at(RAH2_STD::addressof(*first));
                    return first;
                }
            };
        } // namespace niebloids
        constexpr niebloids::destroy_n_fn destroy_n{};
    } // namespace ranges
} // namespace RAH2_NS
