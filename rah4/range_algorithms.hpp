#pragma once

#include "range_bases.hpp"

#include <algorithm> // TODO : Remove this dependency
#include <numeric> // TODO : Remove this dependency
#include <iterator>

namespace rah
{

    // ****************************************** equal_range ***********************************************

    /// @brief Returns a range containing all elements equivalent to value in the range
    ///
    /// @snippet test.cpp rah::equal_range
    template <typename R, typename V>
    auto equal_range(R&& range, V&& value, RAH_STD::enable_if_t<RAH_NAMESPACE::range<R>, int> = 0)
    {
        auto pair = RAH_STD::equal_range(
            RAH_NAMESPACE::begin(range), RAH_NAMESPACE::end(range), RAH_STD::forward<V>(value));
        return make_subrange(RAH_STD::get<0>(pair), RAH_STD::get<1>(pair));
    }

    /// @brief Returns a range containing all elements equivalent to value in the range
    /// @remark pipeable syntax
    ///
    /// @snippet test.cpp rah::equal_range_pipeable
    template <typename V>
    auto equal_range(V&& value)
    {
        return make_pipeable(
            [=](auto&& range)
            { return equal_range(RAH_STD::forward<decltype(range)>(range), value); });
    }

    /// @brief Returns a range containing all elements equivalent to value in the range
    ///
    /// @snippet test.cpp rah::equal_range_pred_0
    /// @snippet test.cpp rah::equal_range_pred
    template <typename R, typename V, typename P>
    auto equal_range(R&& range, V&& value, P&& pred)
    {
        auto pair = RAH_STD::equal_range(
            RAH_NAMESPACE::begin(range),
            RAH_NAMESPACE::end(range),
            RAH_STD::forward<V>(value),
            RAH_STD::forward<P>(pred));
        return make_subrange(RAH_STD::get<0>(pair), RAH_STD::get<1>(pair));
    }

    /// @brief Returns a range containing all elements equivalent to value in the range
    /// @remark pipeable syntax
    ///
    /// @snippet test.cpp rah::equal_range_pred_0
    /// @snippet test.cpp rah::equal_range_pred_pipeable
    template <typename V, typename P>
    auto equal_range(V&& value, P&& pred, RAH_STD::enable_if_t<!range<V>, int> = 0)
    {
        return make_pipeable(
            [=](auto&& range)
            { return equal_range(RAH_STD::forward<decltype(range)>(range), value, pred); });
    }

    // ****************************************** binary_search ***********************************************

    /// @brief Checks if an element equivalent to value appears within the range
    ///
    /// @snippet test.cpp rah::binary_search
    template <typename R, typename V>
    auto binary_search(R&& range, V&& value)
    {
        return RAH_STD::binary_search(
            RAH_NAMESPACE::begin(range), RAH_NAMESPACE::end(range), RAH_STD::forward<V>(value));
    }

    /// @brief Checks if an element equivalent to value appears within the range
    /// @remark pipeable syntax
    ///
    /// @snippet test.cpp rah::binary_search_pipeable
    template <typename V>
    auto binary_search(V&& value)
    {
        return make_pipeable(
            [=](auto&& range)
            { return binary_search(RAH_STD::forward<decltype(range)>(range), value); });
    }

    // ****************************************** transform *******************************************

    /// @brief Applies the given function unary_op to the range rangeIn and stores the result in the range rangeOut
    ///
    /// @snippet test.cpp rah::transform3
    template <typename RI, typename OutIt, typename F>
    auto transform(RI&& rangeIn, OutIt rangeOut, F&& unary_op)
    {
        return RAH_STD::transform(
            RAH_NAMESPACE::begin(rangeIn),
            RAH_NAMESPACE::end(rangeIn),
            rangeOut,
            RAH_STD::forward<F>(unary_op));
    }

    /// @brief The binary operation binary_op is applied to pairs of elements from two ranges
    ///
    /// @snippet test.cpp rah::transform4
    template <typename RI1, typename RI2, typename OutIt, typename F>
    auto transform(RI1&& rangeIn1, RI2&& rangeIn2, OutIt rangeOut, F&& binary_op)
    {
        return RAH_STD::transform(
            RAH_NAMESPACE::begin(rangeIn1),
            RAH_NAMESPACE::end(rangeIn1),
            RAH_NAMESPACE::begin(rangeIn2),
            rangeOut,
            RAH_STD::forward<F>(binary_op));
    }

    // ********************************************* reduce *******************************************

    /// @brief Executes a reducer function on each element of the range, resulting in a single output value
    ///
    /// @snippet test.cpp rah::reduce
    template <typename R, typename I, typename F>
    auto reduce(R&& range, I&& init, F&& reducer)
    {
        return RAH_STD::accumulate(
            RAH_NAMESPACE::begin(range),
            RAH_NAMESPACE::end(range),
            RAH_STD::forward<I>(init),
            RAH_STD::forward<F>(reducer));
    }

    /// @brief Executes a reducer function on each element of the range, resulting in a single output value
    /// @remark pipeable syntax
    ///
    /// @snippet test.cpp rah::reduce_pipeable
    template <typename I, typename F>
    auto reduce(I&& init, F&& reducer)
    {
        return make_pipeable(
            [=](auto&& range)
            { return reduce(RAH_STD::forward<decltype(range)>(range), init, reducer); });
    }

    // ************************* any_of *******************************************

    /// @brief Checks if unary predicate pred returns true for at least one element in the range
    ///
    /// @snippet test.cpp rah::any_of
    template <typename R, typename F>
    bool any_of(R&& range, F&& pred)
    {
        return RAH_STD::any_of(
            RAH_NAMESPACE::begin(range), RAH_NAMESPACE::end(range), RAH_STD::forward<F>(pred));
    }

    /// @brief Checks if unary predicate pred returns true for at least one element in the range
    /// @remark pipeable syntax
    ///
    /// @snippet test.cpp rah::any_of_pipeable
    template <typename P>
    auto any_of(P&& pred)
    {
        return make_pipeable([=](auto&& range)
                             { return any_of(RAH_STD::forward<decltype(range)>(range), pred); });
    }

    // ************************* all_of *******************************************

    /// @brief Checks if unary predicate pred returns true for all elements in the range
    ///
    /// @snippet test.cpp rah::all_of
    template <typename R, typename P>
    bool all_of(R&& range, P&& pred)
    {
        auto iter = RAH_NAMESPACE::begin(range);
        const auto endIter = RAH_NAMESPACE::end(range);
        for (; iter != endIter; ++iter)
        {
            if (not pred(*iter))
                return false;
        }
        return true;
    }

    /// @brief Checks if unary predicate pred returns true for all elements in the range
    /// @remark pipeable syntax
    ///
    /// @snippet test.cpp rah::all_of_pipeable
    template <typename P>
    auto all_of(P&& pred)
    {
        return make_pipeable([=](auto&& range)
                             { return all_of(RAH_STD::forward<decltype(range)>(range), pred); });
    }

    // ************************* none_of *******************************************

    /// @brief Checks if unary predicate pred returns true for no elements in the range
    ///
    /// @snippet test.cpp rah::none_of
    template <typename R, typename P>
    bool none_of(R&& range, P&& pred)
    {
        // return RAH_STD::none_of(RAH_NAMESPACE::begin(range), RAH_NAMESPACE::end(range), RAH_STD::forward<P>(pred));
        auto iter = RAH_NAMESPACE::begin(range);
        const auto endIter = RAH_NAMESPACE::end(range);
        for (; iter != endIter; ++iter)
        {
            if (pred(*iter))
                return false;
        }

        return true;
    }

    /// @brief Checks if unary predicate pred returns true for no elements in the range
    /// @remark pipeable syntax
    ///
    /// @snippet test.cpp rah::none_of_pipeable
    template <typename P>
    auto none_of(P&& pred)
    {
        return make_pipeable([=](auto&& range)
                             { return none_of(RAH_STD::forward<decltype(range)>(range), pred); });
    }

    // ************************* count ****************************************************************

    /// @brief Counts the elements that are equal to value
    ///
    /// @snippet test.cpp rah::count
    template <typename R, typename V>
    auto count(R&& range, V&& value)
    {
        return RAH_STD::count(
            RAH_NAMESPACE::begin(range), RAH_NAMESPACE::end(range), RAH_STD::forward<V>(value));
    }

    /// @brief Counts the elements that are equal to value
    /// @remark pipeable syntax
    ///
    /// @snippet test.cpp rah::count_pipeable
    template <typename V>
    auto count(V&& value)
    {
        return make_pipeable([=](auto&& range)
                             { return count(RAH_STD::forward<decltype(range)>(range), value); });
    }

    /// @brief Counts elements for which predicate pred returns true
    /// @remark pipeable syntax
    ///
    /// @snippet test.cpp rah::count_if
    template <typename R, typename P>
    auto count_if(R&& range, P&& pred)
    {
        return RAH_STD::count_if(
            RAH_NAMESPACE::begin(range), RAH_NAMESPACE::end(range), RAH_STD::forward<P>(pred));
    }

    /// @brief Counts elements for which predicate pred returns true
    ///
    /// @snippet test.cpp rah::count_if_pipeable
    template <typename P>
    auto count_if(P&& pred)
    {
        return make_pipeable([=](auto&& range)
                             { return count_if(RAH_STD::forward<decltype(range)>(range), pred); });
    }

    // ************************* foreach **************************************************************

    /// @brief Applies the given function func to each element of the range
    ///
    /// @snippet test.cpp rah::for_each
    template <typename R, typename F>
    auto for_each(R&& range, F&& func)
    {
        // return ::RAH_STD::for_each(RAH_NAMESPACE::begin(range), RAH_NAMESPACE::end(range), RAH_STD::forward<F>(func));
        for (auto&& v : range)
        {
            func(v);
        }
    }

    /// @brief Applies the given function func to each element of the range
    /// @remark pipeable syntax
    ///
    /// @snippet test.cpp rah::for_each_pipeable
    template <typename F>
    auto for_each(F&& func)
    {
        return make_pipeable([=](auto&& range)
                             { return for_each(RAH_STD::forward<decltype(range)>(range), func); });
    }

    // ************************* mismatch *************************************************************

    /// @brief Finds the first position where two ranges differ
    ///
    /// @snippet test.cpp rah::mismatch
    template <typename R1, typename R2>
    auto mismatch(R1&& range1, R2&& range2)
    {
        return RAH_STD::mismatch(
            RAH_NAMESPACE::begin(range1),
            RAH_NAMESPACE::end(range1),
            RAH_NAMESPACE::begin(range2),
            RAH_NAMESPACE::end(range2));
    }

    // ****************************************** find ************************************************

    /// @brief Finds the first element equal to value
    ///
    /// @snippet test.cpp rah::find
    template <typename R, typename V>
    auto find(R&& range, V&& value)
    {
        return RAH_STD::find(
            RAH_NAMESPACE::begin(range), RAH_NAMESPACE::end(range), RAH_STD::forward<V>(value));
    }

    /// @brief Finds the first element equal to value
    /// @remark pipeable syntax
    ///
    /// @snippet test.cpp rah::find_pipeable
    template <typename V>
    auto find(V&& value)
    {
        return make_pipeable([=](auto&& range)
                             { return find(RAH_STD::forward<decltype(range)>(range), value); });
    }

    /// @brief Finds the first element satisfying specific criteria
    ///
    /// @snippet test.cpp rah::find_if
    template <typename R, typename P>
    auto find_if(R&& range, P&& pred)
    {
        return RAH_STD::find_if(
            RAH_NAMESPACE::begin(range), RAH_NAMESPACE::end(range), RAH_STD::forward<P>(pred));
    }

    /// @brief Finds the first element satisfying specific criteria
    /// @remark pipeable syntax
    ///
    /// @snippet test.cpp rah::find_if_pipeable
    template <typename P>
    auto find_if(P&& pred)
    {
        return make_pipeable([=](auto&& range)
                             { return find_if(RAH_STD::forward<decltype(range)>(range), pred); });
    }

    /// @brief Finds the first element not satisfying specific criteria
    ///
    /// @snippet test.cpp rah::find_if_not
    template <typename R, typename P>
    auto find_if_not(R&& range, P&& pred)
    {
        return RAH_STD::find_if_not(
            RAH_NAMESPACE::begin(range), RAH_NAMESPACE::end(range), RAH_STD::forward<P>(pred));
    }

    /// @brief Finds the first element not satisfying specific criteria
    /// @remark pipeable syntax
    ///
    /// @snippet test.cpp rah::find_if_not_pipeable
    template <typename P>
    auto find_if_not(P&& pred)
    {
        return make_pipeable([=](auto&& range)
                             { return find_if_not(RAH_STD::forward<decltype(range)>(range), pred); });
    }

    // ************************************* max_element **********************************************

    /// @brief Finds the greatest element in the range
    ///
    /// @snippet test.cpp rah::max_element
    template <typename R, RAH_STD::enable_if_t<range<R>, int> = 0>
    auto max_element(R&& range)
    {
        return RAH_STD::max_element(RAH_NAMESPACE::begin(range), RAH_NAMESPACE::end(range));
    }

    /// @brief Finds the greatest element in the range
    ///
    /// @snippet test.cpp rah::max_element_pred
    template <typename R, typename P>
    auto max_element(R&& range, P&& pred)
    {
        return RAH_STD::max_element(
            RAH_NAMESPACE::begin(range), RAH_NAMESPACE::end(range), RAH_STD::forward<P>(pred));
    }

    // ************************************* min_element **********************************************

    /// @brief Finds the smallest element in the range
    ///
    /// @snippet test.cpp rah::min_element
    template <typename R, RAH_STD::enable_if_t<range<R>, int> = 0>
    auto min_element(R&& range)
    {
        return RAH_STD::min_element(RAH_NAMESPACE::begin(range), RAH_NAMESPACE::end(range));
    }

    /// @brief Finds the smallest element in the range
    ///
    /// @snippet test.cpp rah::min_element_pred
    template <typename R, typename P>
    auto min_element(R&& range, P&& pred)
    {
        return RAH_STD::min_element(
            RAH_NAMESPACE::begin(range), RAH_NAMESPACE::end(range), RAH_STD::forward<P>(pred));
    }

    // *************************************** copy ***************************************************

    template <
        typename R1,
        typename It,
        typename = std::enable_if_t<not range<It>>
        //, typename = std::enable_if_t<output_iterator<It>>
        >
    auto copy(R1&& in, It out, int = 0)
    {
        // return RAH_STD::copy(RAH_NAMESPACE::begin(in), RAH_NAMESPACE::end(in), RAH_NAMESPACE::begin(out));
        auto iter1 = RAH_NAMESPACE::begin(in);
        auto end1 = RAH_NAMESPACE::end(in);
        while (iter1 != end1)
        {
            *out = *iter1;
            ++iter1;
            ++out;
        }
        return out; // make_subrange(iter2, RAH_NAMESPACE::end(out));
    }

    // *************************************** fill ***************************************************

    /// @brief Assigns the given value to the elements in the range [first, last)
    ///
    /// @snippet test.cpp rah::copy
    template <typename R1, typename V>
    auto fill(R1&& in, V&& value)
    {
        return RAH_STD::fill(RAH_NAMESPACE::begin(in), RAH_NAMESPACE::end(in), value);
    }

    // *************************************** copy_if ***************************************************

    /// @brief Copies the elements for which the predicate pred returns true
    /// @return The part of out after the copied part
    ///
    /// @snippet test.cpp rah::copy_if
    template <typename R1, typename R2, typename P>
    auto copy_if(R1&& in, R2&& out, P&& pred)
    {
        return RAH_STD::copy_if(
            RAH_NAMESPACE::begin(in),
            RAH_NAMESPACE::end(in),
            RAH_NAMESPACE::begin(out),
            RAH_STD::forward<P>(pred));
    }

    // *************************************** size ***************************************************

    /// @brief Get the size of range
    ///
    /// @snippet test.cpp rah::size
    template <typename R>
    auto size(R&& range)
    {
        return RAH_STD::distance(RAH_NAMESPACE::begin(range), RAH_NAMESPACE::end(range));
    }

    // *************************************** equal **************************************************

    /// @brief Determines if two sets of elements are the same
    ///
    /// @snippet test.cpp rah::equal
    template <typename R1, typename R2>
    auto equal(R1&& range1, R2&& range2)
    {
        auto it1 = RAH_NAMESPACE::begin(range1);
        auto it2 = RAH_NAMESPACE::begin(range2);
        auto end1 = RAH_NAMESPACE::end(range1);
        auto end2 = RAH_NAMESPACE::end(range2);
        for (; it1 != end1 && it2 != end2; ++it1, ++it2)
        {
            if (!(*it1 == *it2))
            {
                return false;
            }
        }
        if (it1 != end1 || it2 != end2)
            return false;
        return true;
    }

    // *********************************** remove_if **************************************************

    /// @brief Keep at the begining of the range only elements for which pred(elt) is false\n
    /// @return Return the (end) part of the range to erase.
    ///
    /// @snippet test.cpp rah::remove_if
    template <typename R, typename P>
    auto remove_if(R&& range, P&& pred)
    {
        return RAH_STD::remove_if(
            RAH_NAMESPACE::begin(range), RAH_NAMESPACE::end(range), RAH_STD::forward<P>(pred));
    }

    // *********************************** remove *****************************************************

    /// @brief Keep at the begining of the range only elements not equal to value\n
    /// @return Return iterator to the part of the range to erase.
    ///
    /// @snippet test.cpp rah::remove
    template <typename R, typename V>
    auto remove(R&& range, V&& value)
    {
        return RAH_STD::remove(
            RAH_NAMESPACE::begin(range), RAH_NAMESPACE::end(range), RAH_STD::forward<V>(value));
    }

    // *********************************** partition **************************************************

    /// @brief Reorders the elements in the @b range in such a way that all elements for which the
    /// predicate @b pred returns `true` precede the elements for which predicate @b pred returns `false`.
    /// Relative order of the elements is not preserved.
    /// @return Iterator to the first element of the second group.
    ///
    /// @snippet test.cpp rah::partition
    template <typename R, typename P>
    auto partition(R&& range, P&& pred)
    {
        return RAH_STD::partition(
            RAH_NAMESPACE::begin(range), RAH_NAMESPACE::end(range), RAH_STD::forward<P>(pred));
    }

    // *********************************** stable_partition *******************************************

    /// @brief Reorders the elements in the @b range in such a way that all elements for which
    /// the predicate @b pred returns `true` precede the elements for which predicate @b pred returns false.
    /// Relative order of the elements is preserved.
    /// @return Iterator to the first element of the second group.
    ///
    /// @snippet test.cpp rah::stable_partition
    template <typename R, typename P>
    auto stable_partition(R&& range, P&& pred)
    {
        return RAH_STD::stable_partition(
            RAH_NAMESPACE::begin(range), RAH_NAMESPACE::end(range), RAH_STD::forward<P>(pred));
    }

    // *********************************** sort *******************************************************

    /// @brief Sort a range in place, using the given predicate.
    ///
    /// @snippet test.cpp rah::sort
    /// @snippet test.cpp rah::sort_pred
    template <typename R, typename P = is_lesser, typename = RAH_STD::enable_if_t<range<R>>>
    void sort(R& range, P&& pred = {})
    {
        RAH_STD::sort(RAH_NAMESPACE::begin(range), RAH_NAMESPACE::end(range), pred);
    }

    // *********************************** stable_sort ************************************************

    /// @brief Sorts the elements in the range in ascending order. The order of equivalent elements is guaranteed to be preserved.
    ///
    /// @snippet test.cpp rah::stable_sort
    /// @snippet test.cpp rah::stable_sort_pred
    template <typename R, typename P = is_lesser, typename = RAH_STD::enable_if_t<range<R>>>
    void stable_sort(R& range, P&& pred = {})
    {
        RAH_STD::stable_sort(RAH_NAMESPACE::begin(range), RAH_NAMESPACE::end(range), pred);
    }

    // *********************************** shuffle *******************************************************

    /// @brief Reorders the elements in the given range such that each possible permutation of those elements has equal probability of appearance.
    ///
    /// @snippet test.cpp rah::shuffle
    template <typename R, typename URBG>
    void shuffle(R& range, URBG&& g)
    {
        RAH_STD::shuffle(
            RAH_NAMESPACE::begin(range), RAH_NAMESPACE::end(range), RAH_STD::forward<URBG>(g));
    }

    // *********************************** unique *****************************************************

    /// Apply the '==' operator on two values of any type
    struct is_equal
    {
        template <typename A, typename B>
        bool operator()(A&& a, B&& b)
        {
            return a == b;
        }
    };

    /// @brief Remove all but first successuve values which are equals. Without resizing the range.
    /// @return The end part of the range, which have to be remove.
    ///
    /// @snippet test.cpp rah::unique
    /// @snippet test.cpp rah::unique_pred
    template <typename R, typename P = is_equal, typename = RAH_STD::enable_if_t<range<R>>>
    auto unique(R&& range, P&& pred = {})
    {
        return RAH_STD::unique(RAH_NAMESPACE::begin(range), RAH_NAMESPACE::end(range), pred);
    }

    // *********************************** set_difference ************************************************

    /// @brief Copies the elements from the sorted range in1 which are not found in the sorted range in2 to the range out
    /// The resulting range is also sorted.
    ///
    /// @snippet test.cpp rah::set_difference
    template <typename IN1, typename IN2, typename OUT_>
    void set_difference(IN1&& in1, IN2&& in2, OUT_&& out)
    {
        RAH_STD::set_difference(
            RAH_NAMESPACE::begin(in1),
            RAH_NAMESPACE::end(in1),
            RAH_NAMESPACE::begin(in2),
            RAH_NAMESPACE::end(in2),
            RAH_NAMESPACE::begin(out));
    }

    // *********************************** set_intersection ************************************************

    /// @brief Copies the elements from the sorted range in1 which are also found in the sorted range in2 to the range out
    /// The resulting range is also sorted.
    ///
    /// @snippet test.cpp rah::set_intersection
    template <typename IN1, typename IN2, typename OUT_>
    void set_intersection(IN1&& in1, IN2&& in2, OUT_&& out)
    {
        RAH_STD::set_intersection(
            RAH_NAMESPACE::begin(in1),
            RAH_NAMESPACE::end(in1),
            RAH_NAMESPACE::begin(in2),
            RAH_NAMESPACE::end(in2),
            RAH_NAMESPACE::begin(out));
    }

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
    template <typename ForwardIterator1, typename ForwardSentinel1, typename ForwardIterator2, typename ForwardSentinel2>
    RAH_NAMESPACE::subrange<ForwardIterator1, ForwardSentinel1> search(
        ForwardIterator1 first1, ForwardSentinel1 last1, ForwardIterator2 first2, ForwardSentinel2 last2)
    {
        if (first2 != last2) // If there is anything to search for...
        {
            // We need to make a special case for a pattern of one element,
            // as the logic below prevents one element patterns from working.
            ForwardIterator2 temp2(first2);
            ++temp2;

            if (temp2 != last2) // If what we are searching for has a length > 1...
            {
                ForwardIterator1 cur1(first1);
                ForwardIterator2 p2;

                while (first1 != last1)
                {
                    // The following loop is the equivalent of eastl::find(first1, last1, *first2)
                    while ((first1 != last1) && !(*first1 == *first2))
                        ++first1;

                    if (first1 != last1)
                    {
                        p2 = temp2;
                        cur1 = first1;

                        if (++cur1 != last1)
                        {
                            while (*cur1 == *p2)
                            {
                                if (++p2 == last2)
                                    return RAH_NAMESPACE::make_subrange(first1, ++cur1);

                                if (++cur1 == last1)
                                    return RAH_NAMESPACE::make_subrange(last1, last1);
                            }

                            ++first1;
                            continue;
                        }
                    }
                    return RAH_NAMESPACE::make_subrange(last1, last1);
                }

                // Fall through to the end.
            }
            else
            {
                auto found1 = RAH_STD::find(first1, last1, *first2);
                auto found1end = found1;
                return RAH_NAMESPACE::make_subrange(found1, ++found1end);
            }
        }

        return RAH_NAMESPACE::make_subrange(first1, first1);
    }

    template <typename Range1, typename Range2>
    RAH_NAMESPACE::subrange<RAH_NAMESPACE::iterator_t<Range1>, RAH_NAMESPACE::iterator_t<Range2>>
    search(Range1&& range1, Range2&& range2)
    {
        return RAH_NAMESPACE::search(
            RAH_NAMESPACE::begin(range1),
            RAH_NAMESPACE::end(range1),
            RAH_NAMESPACE::begin(range2),
            RAH_NAMESPACE::end(range2));
    }

} // namespace rah
