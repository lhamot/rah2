#pragma once

#include "range_bases.hpp"

#include <algorithm> // TODO : Remove this dependency
#include <numeric> // TODO : Remove this dependency
#include <iterator>

#include "eastl_algorithm.h"

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
} // namespace rah
