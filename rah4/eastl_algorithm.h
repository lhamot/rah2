/////////////////////////////////////////////////////////////////////////////
// Copyright (c) Electronic Arts Inc. All rights reserved.
/////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
// This file implements some of the primary algorithms from the C++ STL
// algorithm library. These versions are just like that STL versions and so
// are redundant. They are provided solely for the purpose of projects that
// either cannot use standard C++ STL or want algorithms that have guaranteed
// identical behaviour across platforms.
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
// Definitions
//
// You will notice that we are very particular about the templated typenames
// we use here. You will notice that we follow the C++ standard closely in
// these respects. Each of these typenames have a specific meaning;
// this is why we don't just label templated arguments with just letters
// such as T, U, V, A, B. Here we provide a quick reference for the typenames
// we use. See the C++ standard, section 25-8 for more details.
//    --------------------------------------------------------------
//    typename                     Meaning
//    --------------------------------------------------------------
//    T                            The value type.
//    Compare                      A function which takes two arguments and returns the lesser of the two.
//    Predicate                    A function which takes one argument returns true if the argument meets some criteria.
//    BinaryPredicate              A function which takes two arguments and returns true if some criteria is met (e.g. they are equal).
//    StrickWeakOrdering           A BinaryPredicate that compares two objects, returning true if the first precedes the second. Like Compare but has additional requirements. Used for sorting routines.
//    Function                     A function which takes one argument and applies some operation to the target.
//    Size                         A count or size.
//    Generator                    A function which takes no arguments and returns a value (which will usually be assigned to an object).
//    UnaryOperation               A function which takes one argument and returns a value (which will usually be assigned to second object).
//    BinaryOperation              A function which takes two arguments and returns a value (which will usually be assigned to a third object).
//    InputIterator                An input iterator (iterator you read from) which allows reading each element only once and only in a forward direction.
//    ForwardIterator              An input iterator which is like InputIterator except it can be reset back to the beginning.
//    BidirectionalIterator        An input iterator which is like ForwardIterator except it can be read in a backward direction as well.
//    RandomAccessIterator         An input iterator which can be addressed like an array. It is a superset of all other input iterators.
//    OutputIterator               An output iterator (iterator you write to) which allows writing each element only once in only in a forward direction.
//
// Note that with iterators that a function which takes an InputIterator will
// also work with a ForwardIterator, BidirectionalIterator, or RandomAccessIterator.
// The given iterator type is merely the -minimum- supported functionality the
// iterator must support.
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
// Optimizations
//
// There are a number of opportunities for opptimizations that we take here
// in this library. The most obvious kinds are those that subsitute memcpy
// in the place of a conventional loop for data types with which this is
// possible. The algorithms here are optimized to a higher level than currently
// available C++ STL algorithms from vendors such as Microsoft. This is especially
// so for game programming on console devices, as we do things such as reduce
// branching relative to other STL algorithm implementations. However, the
// proper implementation of these algorithm optimizations is a fairly tricky
// thing.
//
// The various things we look to take advantage of in order to implement
// optimizations include:
//    - Taking advantage of random access iterators.
//    - Taking advantage of POD (plain old data) data types.
//    - Taking advantage of type_traits in general.
//    - Reducing branching and taking advantage of likely branch predictions.
//    - Taking advantage of issues related to pointer and reference aliasing.
//    - Improving cache coherency during memory accesses.
//    - Making code more likely to be inlinable by the compiler.
//
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
// Supported Algorithms
//
// Algorithms that we implement are listed here. Note that these items are not
// all within this header file, as we split up the header files in order to
// improve compilation performance. Items marked with '+' are items that are
// extensions which don't exist in the C++ standard.
//
//    -------------------------------------------------------------------------------
//      Algorithm                                   Notes
//    -------------------------------------------------------------------------------
//      adjacent_find
//      adjacent_find<Compare>
//      all_of                                      C++11
//      any_of                                      C++11
//      none_of                                     C++11
//      binary_search
//      binary_search<Compare>
//     +binary_search_i
//     +binary_search_i<Compare>
//     +change_heap                                 Found in heap.h
//     +change_heap<Compare>                        Found in heap.h
//      clamp
//      copy
//      copy_if                                     C++11
//      copy_n                                      C++11
//      copy_backward
//      count
//      count_if
//      equal
//      equal<Compare>
//      equal_range
//      equal_range<Compare>
//      fill
//      fill_n
//      find
//      find_end
//      find_end<Compare>
//      find_first_of
//      find_first_of<Compare>
//     +find_first_not_of
//     +find_first_not_of<Compare>
//     +find_last_of
//     +find_last_of<Compare>
//     +find_last_not_of
//     +find_last_not_of<Compare>
//      find_if
//      find_if_not
//      for_each
//      generate
//      generate_n
//     +identical
//     +identical<Compare>
//      iter_swap
//      lexicographical_compare
//      lexicographical_compare<Compare>
//      lexicographical_compare_three_way
//      lower_bound
//      lower_bound<Compare>
//      make_heap                                   Found in heap.h
//      make_heap<Compare>                          Found in heap.h
//      min
//      min<Compare>
//      max
//      max<Compare>
//     +min_alt                                     Exists to work around the problem of conflicts with min/max #defines on some systems.
//     +min_alt<Compare>
//     +max_alt
//     +max_alt<Compare>
//     +median
//     +median<Compare>
//      merge                                       Found in sort.h
//      merge<Compare>                              Found in sort.h
//      min_element
//      min_element<Compare>
//      max_element
//      max_element<Compare>
//      mismatch
//      mismatch<Compare>
//      move
//      move_backward
//      nth_element                                 Found in sort.h
//      nth_element<Compare>                        Found in sort.h
//      partial_sort                                Found in sort.h
//      partial_sort<Compare>                       Found in sort.h
//      push_heap                                   Found in heap.h
//      push_heap<Compare>                          Found in heap.h
//      pop_heap                                    Found in heap.h
//      pop_heap<Compare>                           Found in heap.h
//      random_shuffle<Random>
//      remove
//      remove_if
//     +apply_and_remove
//     +apply_and_remove_if
//      remove_copy
//      remove_copy_if
//     +remove_heap                                 Found in heap.h
//     +remove_heap<Compare>                        Found in heap.h
//      replace
//      replace_if
//      replace_copy
//      replace_copy_if
//      reverse_copy
//      reverse
//      random_shuffle
//      rotate
//      rotate_copy
//      search
//      search<Compare>
//      search_n
//      set_difference
//      set_difference<Compare>
//      set_difference_2
//      set_difference_2<Compare>
//      set_decomposition
//      set_decomposition<Compare>
//      set_intersection
//      set_intersection<Compare>
//      set_symmetric_difference
//      set_symmetric_difference<Compare>
//      set_union
//      set_union<Compare>
//      sort                                        Found in sort.h
//      sort<Compare>                               Found in sort.h
//      sort_heap                                   Found in heap.h
//      sort_heap<Compare>                          Found in heap.h
//      stable_sort                                 Found in sort.h
//      stable_sort<Compare>                        Found in sort.h
//      swap
//      swap_ranges
//      transform
//      transform<Operation>
//      unique
//      unique<Compare>
//      upper_bound
//      upper_bound<Compare>
//      is_permutation
//      is_permutation<Predicate>
//      next_permutation
//      next_permutation<Compare>
//
// Algorithms from the C++ standard that we don't implement are listed here.
// Most of these items are absent because they aren't used very often.
// They also happen to be the more complicated than other algorithms.
// However, we can implement any of these functions for users that might
// need them.
//      includes
//      includes<Compare>
//      inplace_merge
//      inplace_merge<Compare>
//      partial_sort_copy
//      partial_sort_copy<Compare>
//      paritition
//      prev_permutation
//      prev_permutation<Compare>
//      search_n<Compare>
//      stable_partition
//      unique_copy
//      unique_copy<Compare>
//
///////////////////////////////////////////////////////////////////////////////

#pragma once

#include "range_bases.hpp"
#include <random>

namespace RAH_NAMESPACE
{
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

    template <class I, class O>
    using move_result = RAH_NAMESPACE::in_out_result<I, O>;

    template <class I, class O1, class O2>
    struct in_out_out_result
    {
        I in;
        O1 out1;
        O2 out2;
    };

    struct move_fn
    {
        template <
            typename I,
            typename S,
            typename O,
            std::enable_if_t<
                RAH_NAMESPACE::input_iterator<I> && RAH_NAMESPACE::sentinel_for<S, I>
                && RAH_NAMESPACE::weakly_incrementable<O>>* = nullptr>
        constexpr RAH_NAMESPACE::move_result<I, O> operator()(I first, S last, O result) const
        {
            for (; first != last; ++first, ++result)
                *result = RAH_NAMESPACE::iter_move(first);
            return {std::move(first), std::move(result)};
        }
        template <
            typename R,
            typename O,
            std::enable_if_t<RAH_NAMESPACE::input_range<R> && RAH_NAMESPACE::weakly_incrementable<O>>* = nullptr>
        constexpr RAH_NAMESPACE::move_result<RAH_NAMESPACE::borrowed_iterator_t<R>, O>
        operator()(R&& r, O result) const
        {
            return (*this)(RAH_NAMESPACE::begin(r), RAH_NAMESPACE::end(r), std::move(result));
        }
    };

    constexpr move_fn move{};

    template <class I, class O>
    using copy_result = RAH_NAMESPACE::in_out_result<I, O>;

    struct copy_fn
    {
        template <typename I, typename S, typename O>
        constexpr RAH_NAMESPACE::copy_result<I, O> operator()(I first, S last, O result) const
        {
            for (; first != last; ++first, (void)++result)
                *result = *first;
            return {std::move(first), std::move(result)};
        }

        template <typename R, typename O>
        RAH_NAMESPACE::copy_result<RAH_NAMESPACE::iterator_t<R>, O> operator()(R&& r, O result) const
        {
            return (*this)(RAH_NAMESPACE::begin(r), RAH_NAMESPACE::end(r), RAH_STD::move(result));
        }
    };

    constexpr copy_fn copy;

    template <class I, class O>
    using copy_if_result = RAH_NAMESPACE::in_out_result<I, O>;

    struct copy_if_fn
    {
        template <
            typename I,
            typename S,
            typename O,
            class Proj = RAH_NAMESPACE::identity,
            typename Pred,
            std::enable_if_t<input_iterator<I> && sentinel_for<S, I>>* = nullptr>
        constexpr RAH_NAMESPACE::copy_if_result<I, O>
        operator()(I first, S last, O result, Pred pred, Proj proj = {}) const
        {
            for (; first != last; ++first)
                if (pred(proj(*first)))
                {
                    *result = *first;
                    ++result;
                }
            return {std::move(first), std::move(result)};
        }

        template <
            typename R,
            typename O,
            class Proj = RAH_NAMESPACE::identity,
            typename Pred,
            std::enable_if_t<input_range<R>>* = nullptr>
        constexpr RAH_NAMESPACE::copy_if_result<RAH_NAMESPACE::iterator_t<R>, O>
        operator()(R&& r, O result, Pred pred, Proj proj = {}) const
        {
            return (*this)(
                RAH_NAMESPACE::begin(r),
                RAH_NAMESPACE::end(r),
                RAH_STD::move(result),
                RAH_STD::ref(pred),
                RAH_STD::ref(proj));
        }
    };

    constexpr copy_if_fn copy_if;

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
    template <
        typename ForwardIterator,
        typename Sentinel,
        std::enable_if_t<forward_iterator<ForwardIterator> && sentinel_for<Sentinel, ForwardIterator>>* = nullptr>
    ForwardIterator min_element(ForwardIterator first, Sentinel last)
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
    auto min_element(ForwardRange&& range)
    {
        return min_element(RAH_NAMESPACE::begin(range), RAH_NAMESPACE::end(range));
    }

    /// min_element
    ///
    /// min_element finds the smallest element in the range [first, last).
    /// It returns the first iterator i in [first, last) such that no other
    /// iterator in [first, last) points to a value smaller than *i.
    /// The return value is last if and only if [first, last) is an empty range.
    ///
    /// Returns: The first iterator i in the range [first, last) such that
    /// for any iterator j in the range [first, last) the following corresponding
    /// conditions hold: compare(*j, *i) == false.
    ///
    /// Complexity: Exactly 'max((last - first) - 1, 0)' applications of the
    /// corresponding comparisons.
    ///
    template <typename ForwardIterator, typename ForwardSentinel, typename Compare>
    ForwardIterator min_element(ForwardIterator first, ForwardSentinel last, Compare compare)
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

    template <typename ForwardRange, typename Compare>
    auto min_element(ForwardRange&& range, Compare compare)
    {
        return min_element(
            RAH_NAMESPACE::begin(range), RAH_NAMESPACE::end(range), RAH_STD::move(compare));
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
    /// condition holds: !(*i < *j).
    ///
    /// Complexity: Exactly 'max((last - first) - 1, 0)' applications of the
    /// corresponding comparisons.
    ///
    template <
        typename ForwardIterator,
        typename ForwardSentinel,
        std::enable_if_t<
            forward_iterator<ForwardIterator> && sentinel_for<ForwardSentinel, ForwardIterator>>* = nullptr>
    ForwardIterator max_element(ForwardIterator first, ForwardSentinel last)
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
    iterator_t<ForwardRange> max_element(ForwardRange&& range)
    {
        return max_element(RAH_NAMESPACE::begin(range), RAH_NAMESPACE::end(range));
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
    ForwardIterator max_element(ForwardIterator first, ForwardSentinel last, Compare compare)
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

    template <typename ForwardRange, typename Compare, std::enable_if_t<forward_range<ForwardRange>>* = nullptr>
    iterator_t<ForwardRange> max_element(ForwardRange&& range, Compare compare)
    {
        return max_element(
            RAH_NAMESPACE::begin(range), RAH_NAMESPACE::end(range), RAH_STD::move(compare));
    }

#if EASTL_MINMAX_ENABLED

    /// min
    ///
    /// Min returns the lesser of its two arguments; it returns the first
    /// argument if neither is less than the other. The two arguments are
    /// compared with operator <.
    ///
    /// This min and our other min implementations are defined as returning:
    ///     b < a ? b : a
    /// which for example may in practice result in something different than:
    ///     b <= a ? b : a
    /// in the case where b is different from a (though they compare as equal).
    /// We choose the specific ordering here because that's the ordering
    /// done by other STL implementations.
    ///
    /// Some compilers (e.g. VS20003 - VS2013) generate poor code for the case of
    /// scalars returned by reference, so we provide a specialization for those cases.
    /// The specialization returns T by value instead of reference, which is
    /// not that the Standard specifies. The Standard allows you to use
    /// an expression like &max(x, y), which would be impossible in this case.
    /// However, we have found no actual code that uses min or max like this and
    /// this specialization causes no problems in practice. Microsoft has acknowledged
    /// the problem and may fix it for a future VS version.
    ///
    template <typename T>
    inline RAH_CONSTEXPR typename RAH_STD::enable_if<RAH_NAMESPACE::is_scalar<T>::value, T>::type
    min(T a, T b)
    {
        return b < a ? b : a;
    }

    template <typename T>
    inline RAH_CONSTEXPR
        typename RAH_STD::enable_if<!RAH_NAMESPACE::is_scalar<T>::value, const T&>::type
        min(const T& a, const T& b)
    {
        return b < a ? b : a;
    }

    inline RAH_CONSTEXPR float min(float a, float b)
    {
        return b < a ? b : a;
    }
    inline RAH_CONSTEXPR double min(double a, double b)
    {
        return b < a ? b : a;
    }
    inline RAH_CONSTEXPR long double min(long double a, long double b)
    {
        return b < a ? b : a;
    }

#endif // EASTL_MINMAX_ENABLED

    /// min_alt
    ///
    /// This is an alternative version of min that avoids any possible
    /// collisions with Microsoft #defines of min and max.
    ///
    /// See min(a, b) for detailed specifications.
    ///
    template <typename T>
    inline RAH_CONSTEXPR typename RAH_STD::enable_if<RAH_NAMESPACE::is_scalar<T>::value, T>::type
    min_alt(T a, T b)
    {
        return b < a ? b : a;
    }

    template <typename T>
    inline typename RAH_STD::enable_if<!RAH_NAMESPACE::is_scalar<T>::value, const T&>::type
    min_alt(const T& a, const T& b)
    {
        return b < a ? b : a;
    }

    inline RAH_CONSTEXPR float min_alt(float a, float b)
    {
        return b < a ? b : a;
    }
    inline RAH_CONSTEXPR double min_alt(double a, double b)
    {
        return b < a ? b : a;
    }
    inline RAH_CONSTEXPR long double min_alt(long double a, long double b)
    {
        return b < a ? b : a;
    }

#if EASTL_MINMAX_ENABLED

    /// min
    ///
    /// Min returns the lesser of its two arguments; it returns the first
    /// argument if neither is less than the other. The two arguments are
    /// compared with the Compare function (or function object), which
    /// takes two arguments and returns true if the first is less than
    /// the second.
    ///
    /// See min(a, b) for detailed specifications.
    ///
    /// Example usage:
    ///    struct A{ int a; };
    ///    struct Struct{ bool operator()(const A& a1, const A& a2){ return a1.a < a2.a; } };
    ///
    ///    A a1, a2, a3;
    ///    a3 = min(a1, a2, Struct());
    ///
    /// Example usage:
    ///    struct B{ int b; };
    ///    inline bool Function(const B& b1, const B& b2){ return b1.b < b2.b; }
    ///
    ///    B b1, b2, b3;
    ///    b3 = min(b1, b2, Function);
    ///
    template <typename T, typename Compare>
    inline const T& min(const T& a, const T& b, Compare compare)
    {
        return compare(b, a) ? b : a;
    }

#endif // EASTL_MINMAX_ENABLED

    /// min_alt
    ///
    /// This is an alternative version of min that avoids any possible
    /// collisions with Microsoft #defines of min and max.
    ///
    /// See min(a, b) for detailed specifications.
    ///
    template <typename T, typename Compare>
    inline const T& min_alt(const T& a, const T& b, Compare compare)
    {
        return compare(b, a) ? b : a;
    }

#if EASTL_MINMAX_ENABLED

    /// max
    ///
    /// Max returns the greater of its two arguments; it returns the first
    /// argument if neither is greater than the other. The two arguments are
    /// compared with operator < (and not operator >).
    ///
    /// This min and our other min implementations are defined as returning:
    ///     a < b ? b : a
    /// which for example may in practice result in something different than:
    ///     a <= b ? b : a
    /// in the case where b is different from a (though they compare as equal).
    /// We choose the specific ordering here because that's the ordering
    /// done by other STL implementations.
    ///
    template <typename T>
    inline RAH_CONSTEXPR typename RAH_STD::enable_if<RAH_NAMESPACE::is_scalar<T>::value, T>::type
    max(T a, T b)
    {
        return a < b ? b : a;
    }

    template <typename T>
    inline RAH_CONSTEXPR
        typename RAH_STD::enable_if<!RAH_NAMESPACE::is_scalar<T>::value, const T&>::type
        max(const T& a, const T& b)
    {
        return a < b ? b : a;
    }

    inline RAH_CONSTEXPR float max(float a, float b)
    {
        return a < b ? b : a;
    }
    inline RAH_CONSTEXPR double max(double a, double b)
    {
        return a < b ? b : a;
    }
    inline RAH_CONSTEXPR long double max(long double a, long double b)
    {
        return a < b ? b : a;
    }

#endif // EASTL_MINMAX_ENABLED

    /// max_alt
    ///
    /// This is an alternative version of max that avoids any possible
    /// collisions with Microsoft #defines of min and max.
    ///
    template <typename T>
    inline RAH_CONSTEXPR typename RAH_STD::enable_if<RAH_NAMESPACE::is_scalar<T>::value, T>::type
    max_alt(T a, T b)
    {
        return a < b ? b : a;
    }

    template <typename T>
    inline RAH_CONSTEXPR
        typename RAH_STD::enable_if<!RAH_NAMESPACE::is_scalar<T>::value, const T&>::type
        max_alt(const T& a, const T& b)
    {
        return a < b ? b : a;
    }

    inline RAH_CONSTEXPR float max_alt(float a, float b)
    {
        return a < b ? b : a;
    }
    inline RAH_CONSTEXPR double max_alt(double a, double b)
    {
        return a < b ? b : a;
    }
    inline RAH_CONSTEXPR long double max_alt(long double a, long double b)
    {
        return a < b ? b : a;
    }

#if EASTL_MINMAX_ENABLED
    /// max
    ///
    /// Min returns the lesser of its two arguments; it returns the first
    /// argument if neither is less than the other. The two arguments are
    /// compared with the Compare function (or function object), which
    /// takes two arguments and returns true if the first is less than
    /// the second.
    ///
    template <typename T, typename Compare>
    inline const T& max(const T& a, const T& b, Compare compare)
    {
        return compare(a, b) ? b : a;
    }
#endif

    /// max_alt
    ///
    /// This is an alternative version of max that avoids any possible
    /// collisions with Microsoft #defines of min and max.
    ///
    template <typename T, typename Compare>
    inline const T& max_alt(const T& a, const T& b, Compare compare)
    {
        return compare(a, b) ? b : a;
    }

    /// min(std::initializer_list)
    ///
    template <typename T>
    T min(std::initializer_list<T> ilist)
    {
        return *RAH_NAMESPACE::min_element(ilist.begin(), ilist.end());
    }

    /// min(std::initializer_list, Compare)
    ///
    template <typename T, typename Compare>
    T min(std::initializer_list<T> ilist, Compare compare)
    {
        return *RAH_NAMESPACE::min_element(ilist.begin(), ilist.end(), compare);
    }

    /// max(std::initializer_list)
    ///
    template <typename T>
    T max(std::initializer_list<T> ilist)
    {
        return *RAH_NAMESPACE::max_element(ilist.begin(), ilist.end());
    }

    /// max(std::initializer_list, Compare)
    ///
    template <typename T, typename Compare>
    T max(std::initializer_list<T> ilist, Compare compare)
    {
        return *RAH_NAMESPACE::max_element(ilist.begin(), ilist.end(), compare);
    }

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
    template <typename ForwardIterator, typename ForwardSentinel, typename Compare>
    RAH_STD::pair<ForwardIterator, ForwardIterator>
    minmax_element(ForwardIterator first, ForwardSentinel last, Compare compare)
    {
        RAH_STD::pair<ForwardIterator, ForwardIterator> result(first, first);

        if (!(first == last) && !(++first == last))
        {
            if (compare(*first, *result.first))
            {
                result.second = result.first;
                result.first = first;
            }
            else
                result.second = first;

            while (++first != last)
            {
                ForwardIterator i = first;

                if (++first == last)
                {
                    if (compare(*i, *result.first))
                        result.first = i;
                    else if (!compare(*i, *result.second))
                        result.second = i;
                    break;
                }
                else
                {
                    if (compare(*first, *i))
                    {
                        if (compare(*first, *result.first))
                            result.first = first;

                        if (!compare(*i, *result.second))
                            result.second = i;
                    }
                    else
                    {
                        if (compare(*i, *result.first))
                            result.first = i;

                        if (!compare(*first, *result.second))
                            result.second = first;
                    }
                }
            }
        }

        return result;
    }

    template <typename ForwardIterator, typename ForwardSentinel>
    RAH_STD::pair<ForwardIterator, ForwardIterator>
    minmax_element(ForwardIterator first, ForwardSentinel last)
    {
        typedef typename RAH_STD::iterator_traits<ForwardIterator>::value_type value_type;

        return RAH_NAMESPACE::minmax_element(first, last, RAH_STD::less<value_type>());
    }

    /// minmax
    ///
    /// Requires: Type T shall be LessThanComparable.
    /// Returns: pair<const T&, const T&>(b, a) if b is smaller than a, and pair<const T&, const T&>(a, b) otherwise.
    /// Remarks: Returns pair<const T&, const T&>(a, b) when the arguments are equivalent.
    /// Complexity: Exactly one comparison.
    ///

    // The following optimization is a problem because it changes the return value in a way that would break
    // users unless they used auto (e.g. auto result = minmax(17, 33); )
    //
    // template <typename T>
    // inline RAH_CONSTEXPR typename RAH_STD::enable_if<RAH_NAMESPACE::is_scalar<T>::value, RAH_STD::pair<T, T> >::type
    // minmax(T a, T b)
    // {
    //     return (b < a) ? RAH_STD::make_pair(b, a) : RAH_STD::make_pair(a, b);
    // }
    //
    // template <typename T>
    // inline typename RAH_STD::enable_if<!RAH_NAMESPACE::is_scalar<T>::value, RAH_STD::pair<const T&, const T&> >::type
    // minmax(const T& a, const T& b)
    // {
    //     return (b < a) ? RAH_STD::make_pair(b, a) : RAH_STD::make_pair(a, b);
    // }

    // It turns out that the following conforming definition of minmax generates a warning when used with VC++ up
    // to at least VS2012. The VS2012 version of minmax is a broken and non-conforming definition, and we don't
    // want to do that. We could do it for scalars alone, though we'd have to decide if we are going to do that
    // for all compilers, because it changes the return value from a pair of references to a pair of values.
    template <typename T>
    inline RAH_STD::pair<const T&, const T&> minmax(const T& a, const T& b)
    {
        return (b < a) ? RAH_STD::make_pair(b, a) : RAH_STD::make_pair(a, b);
    }

    template <typename T, typename Compare>
    RAH_STD::pair<const T&, const T&> minmax(const T& a, const T& b, Compare compare)
    {
        return compare(b, a) ? RAH_STD::make_pair(b, a) : RAH_STD::make_pair(a, b);
    }

    template <typename T>
    RAH_STD::pair<T, T> minmax(std::initializer_list<T> ilist)
    {
        typedef typename std::initializer_list<T>::iterator iterator_type;
        RAH_STD::pair<iterator_type, iterator_type> iteratorPair =
            RAH_NAMESPACE::minmax_element(ilist.begin(), ilist.end());
        return RAH_STD::make_pair(*iteratorPair.first, *iteratorPair.second);
    }

    template <typename T, class Compare>
    RAH_STD::pair<T, T> minmax(std::initializer_list<T> ilist, Compare compare)
    {
        typedef typename std::initializer_list<T>::iterator iterator_type;
        RAH_STD::pair<iterator_type, iterator_type> iteratorPair =
            RAH_NAMESPACE::minmax_element(ilist.begin(), ilist.end(), compare);
        return RAH_STD::make_pair(*iteratorPair.first, *iteratorPair.second);
    }

    template <typename T>
    inline T&& median_impl(T&& a, T&& b, T&& c)
    {
        if (RAH_STD::less<T>()(a, b))
        {
            if (RAH_STD::less<T>()(b, c))
                return RAH_STD::forward<T>(b);
            else if (RAH_STD::less<T>()(a, c))
                return RAH_STD::forward<T>(c);
            else
                return RAH_STD::forward<T>(a);
        }
        else if (RAH_STD::less<T>()(a, c))
            return RAH_STD::forward<T>(a);
        else if (RAH_STD::less<T>()(b, c))
            return RAH_STD::forward<T>(c);
        return RAH_STD::forward<T>(b);
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
    inline const T& median(const T& a, const T& b, const T& c)
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
    inline T&& median(T&& a, T&& b, T&& c)
    {
        return RAH_STD::forward<T>(
            median_impl(RAH_STD::forward<T>(a), RAH_STD::forward<T>(b), RAH_STD::forward<T>(c)));
    }

    template <typename T, typename Compare>
    inline T&& median_impl(T&& a, T&& b, T&& c, Compare compare)
    {
        if (compare(a, b))
        {
            if (compare(b, c))
                return RAH_STD::forward<T>(b);
            else if (compare(a, c))
                return RAH_STD::forward<T>(c);
            else
                return RAH_STD::forward<T>(a);
        }
        else if (compare(a, c))
            return RAH_STD::forward<T>(a);
        else if (compare(b, c))
            return RAH_STD::forward<T>(c);
        return RAH_STD::forward<T>(b);
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
    inline const T& median(const T& a, const T& b, const T& c, Compare compare)
    {
        return median_impl<const T&, Compare>(a, b, c, compare);
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
    inline T&& median(T&& a, T&& b, T&& c, Compare compare)
    {
        return RAH_STD::forward<T>(median_impl<T&&, Compare>(
            RAH_STD::forward<T>(a), RAH_STD::forward<T>(b), RAH_STD::forward<T>(c), compare));
    }

    /// all_of
    ///
    /// Returns: true if the unary predicate p returns true for all elements in the range [first, last)
    ///
    template <typename InputIterator, typename InputSentinel, typename Predicate>
    inline bool all_of(InputIterator first, InputSentinel last, Predicate p)
    {
        for (; first != last; ++first)
        {
            if (!p(*first))
                return false;
        }
        return true;
    }

    template <typename InputRange, typename Predicate>
    inline bool all_of(InputRange&& range, Predicate p)
    {
        return all_of(RAH_NAMESPACE::begin(range), RAH_NAMESPACE::end(range), RAH_STD::move(p));
    }

    /// any_of
    ///
    /// Returns: true if the unary predicate p returns true for any of the elements in the range [first, last)
    ///
    template <typename InputIterator, typename InputSentinel, typename Predicate>
    inline bool any_of(InputIterator first, InputSentinel last, Predicate p)
    {
        for (; first != last; ++first)
        {
            if (p(*first))
                return true;
        }
        return false;
    }

    template <typename InputRange, typename Predicate>
    inline bool any_of(InputRange&& range, Predicate p)
    {
        return any_of(RAH_NAMESPACE::begin(range), RAH_NAMESPACE::end(range), RAH_STD::move(p));
    }

    /// none_of
    ///
    /// Returns: true if the unary predicate p returns true for none of the elements in the range [first, last)
    ///
    template <typename InputIterator, typename InputSentinel, typename Predicate>
    inline bool none_of(InputIterator first, InputSentinel last, Predicate p)
    {
        for (; first != last; ++first)
        {
            if (p(*first))
                return false;
        }
        return true;
    }

    template <typename InputRange, typename Predicate>
    inline bool none_of(InputRange&& range, Predicate&& p)
    {
        return none_of(
            RAH_NAMESPACE::begin(range), RAH_NAMESPACE::end(range), RAH_STD::forward<Predicate>(p));
    }

    /// adjacent_find
    ///
    /// Returns: The first iterator i such that both i and i + 1 are in the range
    /// [first, last) for which the following corresponding conditions hold: *i == *(i + 1).
    /// Returns last if no such iterator is found.
    ///
    /// Complexity: Exactly 'find(first, last, value) - first' applications of the corresponding predicate.
    ///
    struct adjacent_find_fn
    {
        template <
            typename I,
            typename S,
            class Proj = RAH_NAMESPACE::identity,
            typename Pred = RAH_NAMESPACE::equal_to,
            std::enable_if_t<forward_iterator<I> && sentinel_for<S, I>>* = nullptr>
        constexpr I operator()(I first, S last, Pred pred = {}, Proj proj = {}) const
        {
            if (first == last)
                return first;
            auto next = RAH_NAMESPACE::next(first);
            for (; next != last; ++next, ++first)
            {
                if (pred(proj(*first), proj(*next)))
                    return first;
            }
            return next;
        }

        template <
            typename R,
            class Proj = RAH_NAMESPACE::identity,
            typename Pred = RAH_NAMESPACE::equal_to,
            std::enable_if_t<forward_range<R>>* = nullptr>
        constexpr RAH_NAMESPACE::borrowed_iterator_t<R>
        operator()(R&& r, Pred pred = {}, Proj proj = {}) const
        {
            return (*this)(
                RAH_NAMESPACE::begin(r), RAH_NAMESPACE::end(r), std::ref(pred), std::ref(proj));
        }
    };

    constexpr adjacent_find_fn adjacent_find;

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
    ///     struct Rand{ RAH_STD::size_t operator()(RAH_STD::size_t n) { return (RAH_STD::size_t)(rand() % n); } }; // Note: The C rand function is poor and slow.
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
                typename RAH_STD::iterator_traits<RandomAccessIterator>::difference_type difference_type;
            typedef typename RAH_STD::make_unsigned<difference_type>::type unsigned_difference_type;
            typedef typename RAH_STD::uniform_int_distribution<unsigned_difference_type>
                uniform_int_distribution;
            typedef typename uniform_int_distribution::param_type uniform_int_distribution_param_type;

            uniform_int_distribution uid;

            for (RandomAccessIterator i = first + 1; i != last; ++i)
                iter_swap(i, first + uid(urng, uniform_int_distribution_param_type(0, i - first)));
        }
    }

    template <typename RandomRange, typename UniformRandomNumberGenerator>
    void shuffle(RandomRange&& range, UniformRandomNumberGenerator&& urng)
    {
        shuffle(
            RAH_NAMESPACE::begin(range),
            RAH_NAMESPACE::end(range),
            RAH_STD::forward<UniformRandomNumberGenerator>(urng));
    }

    /// random_shuffle
    ///
    /// Randomizes a sequence of values.
    ///
    /// Effects: Shuffles the elements in the range [first, last) with uniform distribution.
    ///
    /// Complexity: Exactly '(last - first) - 1' swaps.
    ///
    /// Example usage:
    ///     RAH_STD::size_t Rand(RAH_STD::size_t n) { return (RAH_STD::size_t)(rand() % n); } // Note: The C rand function is poor and slow.
    ///     pointer_to_unary_function<RAH_STD::size_t, RAH_STD::size_t> randInstance(Rand);
    ///     random_shuffle(pArrayBegin, pArrayEnd, randInstance);
    ///
    /// Example usage:
    ///     struct Rand{ RAH_STD::size_t operator()(RAH_STD::size_t n) { return (RAH_STD::size_t)(rand() % n); } }; // Note: The C rand function is poor and slow.
    ///     Rand randInstance;
    ///     random_shuffle(pArrayBegin, pArrayEnd, randInstance);
    ///
    template <typename RandomAccessIterator, typename Sentinel, typename RandomNumberGenerator>
    inline void random_shuffle(RandomAccessIterator first, Sentinel last, RandomNumberGenerator&& rng)
    {
        typedef
            typename RAH_STD::iterator_traits<RandomAccessIterator>::difference_type difference_type;

        // We must do 'rand((i - first) + 1)' here and cannot do 'rand(last - first)',
        // as it turns out that the latter results in unequal distribution probabilities.
        // http://www.cigital.com/papers/download/developer_gambling.php

        for (RandomAccessIterator i = first + 1; i < last; ++i)
            iter_swap(i, first + (difference_type)rng((RAH_STD::size_t)((i - first) + 1)));
    }

    /// random_shuffle
    ///
    /// Randomizes a sequence of values.
    ///
    /// Effects: Shuffles the elements in the range [first, last) with uniform distribution.
    ///
    /// Complexity: Exactly '(last - first) - 1' swaps.
    ///
    /// Example usage:
    ///     random_shuffle(pArrayBegin, pArrayEnd);
    ///
    /// *** Disabled until we decide if we want to get into the business of writing random number generators. ***
    ///
    /// template <typename RandomAccessIterator, typename Sentinel>
    /// inline void random_shuffle(RandomAccessIterator first, Sentinel last)
    /// {
    ///     for(RandomAccessIterator i = first + 1; i < last; ++i)
    ///         iter_swap(i, first + SomeRangedRandomNumberGenerator((i - first) + 1));
    /// }

    /// move_n
    ///
    /// Same as move(InputIterator, InputIterator, OutputIterator) except based on count instead of iterator range.
    ///
    template <typename InputIterator, typename Size, typename OutputIterator>
    inline OutputIterator
    move_n_impl(InputIterator first, Size n, OutputIterator result, RAH_ITC_NS::input_iterator_tag)
    {
        for (; n > 0; --n)
            *result++ = RAH_STD::move(*first++);
        return result;
    }

    template <class I, class O>
    using copy_n_result = in_out_result<I, O>;

    /// copy_n
    ///
    /// Same as copy(InputIterator, InputIterator, OutputIterator) except based on count instead of iterator range.
    /// Effects: Copies exactly count values from the range beginning at first to the range beginning at result, if count > 0. Does nothing otherwise.
    /// Returns: Iterator in the destination range, pointing past the last element copied if count>0 or first otherwise.
    /// Complexity: Exactly count assignments, if count > 0.
    ///
    struct copy_n_fn
    {
        template <
            typename I,
            typename O,
            std::enable_if_t<
                RAH_NAMESPACE::input_iterator<I> && !RAH_NAMESPACE::random_access_iterator<I>
                && RAH_NAMESPACE::weakly_incrementable<O>>* = nullptr>
        constexpr RAH_NAMESPACE::copy_n_result<I, O>
        operator()(I first, iter_difference_t<I> n, O result) const
        {
            for (RAH_NAMESPACE::iter_difference_t<I> i{}; i != n; ++i, ++first, ++result)
                *result = *first;
            return {std::move(first), std::move(result)};
        }

        template <
            typename I,
            typename O,
            std::enable_if_t<
                RAH_NAMESPACE::input_iterator<I> && RAH_NAMESPACE::random_access_iterator<I>
                && RAH_NAMESPACE::weakly_incrementable<O>>* = nullptr>
        constexpr RAH_NAMESPACE::copy_n_result<I, O>
        operator()(I first, iter_difference_t<I> n, O result) const
        {
            return RAH_NAMESPACE::copy(first, first + n, result);
        }
    };

    constexpr copy_n_fn copy_n{};

    template <class I, class O>
    using move_backward_result = RAH_NAMESPACE::in_out_result<I, O>;

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
    ///     RAH_NAMESPACE::move_backward(myArray.begin(), myArray.end(), myDestArray.end());
    ///
    /// Reference implementation:
    ///     template <typename BidirectionalIterator1, typename Sentinel1, typename BidirectionalIterator2 >
    ///     BidirectionalIterator2 move_backward(BidirectionalIterator1 first, Sentinel1 last, BidirectionalIterator2 resultEnd)
    ///     {
    ///         while(last != first)
    ///             *--resultEnd = RAH_STD::move(*--last);
    ///         return resultEnd;
    ///     }
    struct move_backward_fn
    {
        template <
            typename I1,
            typename S1,
            typename I2,
            std::enable_if_t<
                RAH_NAMESPACE::bidirectional_iterator<I1> && RAH_NAMESPACE::sentinel_for<S1, I1>
                && RAH_NAMESPACE::bidirectional_iterator<I2>>* = nullptr>
        constexpr RAH_NAMESPACE::move_backward_result<I1, I2> operator()(I1 first, S1 last, I2 result) const
        {
            auto last2 = RAH_NAMESPACE::next(first, last);
            auto i = last2;
            for (; i != first; *--result = RAH_NAMESPACE::iter_move(--i))
            {
            }
            return {std::move(last2), std::move(result)};
        }

        template <
            typename R,
            typename I,
            std::enable_if_t<
                RAH_NAMESPACE::bidirectional_range<R> && RAH_NAMESPACE::bidirectional_iterator<I>>* = nullptr>
        constexpr RAH_NAMESPACE::move_backward_result<RAH_NAMESPACE::borrowed_iterator_t<R>, I>
        operator()(R&& r, I result) const
        {
            return (*this)(RAH_NAMESPACE::begin(r), RAH_NAMESPACE::end(r), std::move(result));
        }
    };

    constexpr move_backward_fn move_backward{};

    template <class I1, class I2>
    using copy_backward_result = RAH_NAMESPACE::in_out_result<I1, I2>;

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
    struct copy_backward_fn
    {
        template <
            typename I1,
            typename S1,
            typename I2,
            std::enable_if_t<
                RAH_NAMESPACE::bidirectional_iterator<I1> && RAH_NAMESPACE::sentinel_for<S1, I1>
                && RAH_NAMESPACE::bidirectional_iterator<I2>>* = nullptr>
        constexpr RAH_NAMESPACE::copy_backward_result<I1, I2> operator()(I1 first, S1 last, I2 result) const
        {
            I1 last1{RAH_NAMESPACE::next(first, std::move(last))};
            for (I1 i{last1}; i != first;)
                *--result = *--i;
            return {std::move(last1), std::move(result)};
        }

        template <
            typename R,
            typename I,
            std::enable_if_t<
                RAH_NAMESPACE::bidirectional_range<R> && RAH_NAMESPACE::bidirectional_iterator<I>>* = nullptr>
        constexpr RAH_NAMESPACE::copy_backward_result<RAH_NAMESPACE::borrowed_iterator_t<R>, I>
        operator()(R&& r, I result) const
        {
            return (*this)(RAH_NAMESPACE::begin(r), RAH_NAMESPACE::end(r), std::move(result));
        }
    };

    constexpr copy_backward_fn copy_backward{};

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
    struct count_fn
    {
        template <
            typename I,
            typename S,
            class T,
            class Proj = RAH_NAMESPACE::identity,
            std::enable_if_t<input_iterator<I> && sentinel_for<S, I>>* = nullptr>
        constexpr RAH_NAMESPACE::iter_difference_t<I>
        operator()(I first, S last, const T& value, Proj proj) const
        {
            RAH_NAMESPACE::iter_difference_t<I> counter = 0;
            for (; first != last; ++first)
            {
                if (proj(*first) == value)
                    ++counter;
            }
            return counter;
        }

        template <typename R, class T, class Proj = RAH_NAMESPACE::identity, std::enable_if_t<input_range<R>>* = nullptr>
        constexpr RAH_NAMESPACE::range_difference_t<R> operator()(R&& r, const T& value, Proj proj) const
        {
            return (*this)(RAH_NAMESPACE::begin(r), RAH_NAMESPACE::end(r), value, std::ref(proj));
        }

        template <typename I, typename S, class T, std::enable_if_t<input_iterator<I> && sentinel_for<S, I>>* = nullptr>
        constexpr RAH_NAMESPACE::iter_difference_t<I> operator()(I first, S last, const T& value) const
        {
            RAH_NAMESPACE::iter_difference_t<I> counter = 0;
            for (; first != last; ++first)
                if (*first == value)
                    ++counter;
            return counter;
        }

        template <typename R, class T, std::enable_if_t<input_range<R>>* = nullptr>
        constexpr RAH_NAMESPACE::range_difference_t<R> operator()(R&& r, const T& value) const
        {
            return (*this)(RAH_NAMESPACE::begin(r), RAH_NAMESPACE::end(r), value);
        }
    };

    constexpr count_fn count;

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
    struct count_if_fn
    {
        template <
            typename I,
            typename S,
            class Proj = RAH_NAMESPACE::identity,
            typename Pred,
            std::enable_if_t<input_iterator<I> && sentinel_for<S, I>>* = nullptr>
        RAH_NODISCARD RAH_CONSTEXPR20 RAH_NAMESPACE::iter_difference_t<I>
        operator()(I first, S last, Pred pred, Proj proj) const
        {
            RAH_NAMESPACE::iter_difference_t<I> counter = 0;
            for (; first != last; ++first)
                if (pred(proj(*first)))
                    ++counter;
            return counter;
        }

        template <
            typename R,
            class Proj = RAH_NAMESPACE::identity,
            typename Pred,
            std::enable_if_t<input_range<R>>* = nullptr>
        RAH_NODISCARD RAH_CONSTEXPR20 RAH_NAMESPACE::range_difference_t<R>
        operator()(R&& r, Pred pred, Proj proj) const
        {
            return (*this)(
                RAH_NAMESPACE::begin(r), RAH_NAMESPACE::end(r), std::ref(pred), std::ref(proj));
        }

        template <
            typename I,
            typename S,
            typename Pred,
            std::enable_if_t<input_iterator<I> && sentinel_for<S, I>>* = nullptr>
        RAH_NODISCARD RAH_CONSTEXPR20 RAH_NAMESPACE::iter_difference_t<I>
        operator()(I first, S last, Pred pred) const
        {
            RAH_NAMESPACE::iter_difference_t<I> counter = 0;
            for (; first != last; ++first)
                if (pred(*first))
                    ++counter;
            return counter;
        }

        template <typename R, typename Pred, std::enable_if_t<input_range<R>>* = nullptr>
        RAH_NODISCARD RAH_CONSTEXPR20 RAH_NAMESPACE::range_difference_t<R>
        operator()(R&& r, Pred pred) const
        {
            return (*this)(RAH_NAMESPACE::begin(r), RAH_NAMESPACE::end(r), std::ref(pred));
        }
    };

    constexpr count_if_fn count_if;

    struct find_fn
    {
        template <
            typename I,
            typename S,
            class T,
            class Proj = RAH_NAMESPACE::identity,
            std::enable_if_t<input_iterator<I> && sentinel_for<S, I>>* = nullptr>
        constexpr I operator()(I first, S last, const T& value, Proj proj = {}) const
        {
            for (; first != last; ++first)
                if (proj(*first) == value)
                    return first;
            return first;
        }

        template <typename R, class T, class Proj = RAH_NAMESPACE::identity, std::enable_if_t<input_range<R>>* = nullptr>
        constexpr RAH_NAMESPACE::borrowed_iterator_t<R>
        operator()(R&& r, const T& value, Proj proj = {}) const
        {
            return (*this)(RAH_NAMESPACE::begin(r), RAH_NAMESPACE::end(r), value, std::ref(proj));
        }
    };

    constexpr find_fn find;

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
    struct find_if_fn
    {
        template <
            typename I,
            typename S,
            class Proj = RAH_NAMESPACE::identity,
            typename Pred,
            std::enable_if_t<input_iterator<I> && sentinel_for<S, I>>* = nullptr>
        constexpr I operator()(I first, S last, Pred pred, Proj proj = {}) const
        {
            for (; first != last; ++first)
                if (pred(proj(*first)))
                    return first;
            return first;
        }

        template <
            typename R,
            class Proj = RAH_NAMESPACE::identity,
            typename Pred,
            std::enable_if_t<input_range<R>>* = nullptr>
        constexpr RAH_NAMESPACE::borrowed_iterator_t<R> operator()(R&& r, Pred pred, Proj proj = {}) const
        {
            return (*this)(
                RAH_NAMESPACE::begin(r), RAH_NAMESPACE::end(r), std::ref(pred), std::ref(proj));
        }
    };

    constexpr find_if_fn find_if;

    /// find_if_not
    ///
    /// find_if_not works the same as find_if except it tests for if the predicate
    /// returns false for the elements instead of true.
    ///
    struct find_if_not_fn
    {
        template <
            typename I,
            typename S,
            class Proj = RAH_NAMESPACE::identity,
            typename Pred,
            std::enable_if_t<input_iterator<I> && sentinel_for<S, I>>* = nullptr>
        constexpr I operator()(I first, S last, Pred pred, Proj proj = {}) const
        {
            for (; first != last; ++first)
                if (!pred(proj(*first)))
                    return first;
            return first;
        }

        template <
            typename R,
            class Proj = RAH_NAMESPACE::identity,
            typename Pred,
            std::enable_if_t<input_range<R>>* = nullptr>
        constexpr RAH_NAMESPACE::borrowed_iterator_t<R> operator()(R&& r, Pred pred, Proj proj = {}) const
        {
            return (*this)(
                RAH_NAMESPACE::begin(r), RAH_NAMESPACE::end(r), std::ref(pred), std::ref(proj));
        }
    };

    constexpr find_if_not_fn find_if_not;

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
    struct find_first_of_fn
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
                input_iterator<I1> && sentinel_for<S1, I1> && forward_iterator<I2>
                && sentinel_for<S2, I2>>* = nullptr>
        constexpr I1 operator()(
            I1 first1, S1 last1, I2 first2, S2 last2, Pred pred = {}, Proj1 proj1 = {}, Proj2 proj2 = {}) const
        {
            for (; first1 != last1; ++first1)
                for (auto i = first2; i != last2; ++i)
                    if (RAH_INVOKE_2(pred, (RAH_INVOKE_1(proj1, *first1)), (RAH_INVOKE_1(proj2, *i))))
                        return first1;
            return first1;
        }

        template <
            typename R1,
            typename R2,
            class Pred = RAH_NAMESPACE::equal_to,
            class Proj1 = RAH_NAMESPACE::identity,
            class Proj2 = RAH_NAMESPACE::identity,
            std::enable_if_t<input_range<R1> && forward_range<R2>>* = nullptr>
        constexpr RAH_NAMESPACE::borrowed_iterator_t<R1>
        operator()(R1&& r1, R2&& r2, Pred pred = {}, Proj1 proj1 = {}, Proj2 proj2 = {}) const
        {
            return (*this)(
                RAH_NAMESPACE::begin(r1),
                RAH_NAMESPACE::end(r1),
                RAH_NAMESPACE::begin(r2),
                RAH_NAMESPACE::end(r2),
                std::move(pred),
                std::move(proj1),
                std::move(proj2));
        }
    };

    constexpr find_first_of_fn find_first_of{};

    /// find_first_not_of
    ///
    /// Searches through first range for the first element that does not belong the second input range.
    /// This is very much like the C++ string find_first_not_of function.
    ///
    /// Returns: The first iterator i in the range [first1, last1) such that for some
    /// integer j in the range [first2, last2) the following conditions hold: !(*i == *j).
    /// Returns last1 if no such iterator is found.
    ///
    /// Complexity: At most '(last1 - first1) * (last2 - first2)' applications of the
    /// corresponding predicate.
    ///
    template <class ForwardIterator1, typename ForwardSentinel1, class ForwardIterator2, typename ForwardSentinel2>
    ForwardIterator1 find_first_not_of(
        ForwardIterator1 first1, ForwardSentinel1 last1, ForwardIterator2 first2, ForwardSentinel2 last2)
    {
        for (; first1 != last1; ++first1)
        {
            if (RAH_NAMESPACE::find(first2, last2, *first1) == last2)
                break;
        }

        return first1;
    }

    /// find_first_not_of
    ///
    /// Searches through first range for the first element that does not belong the second input range.
    /// This is very much like the C++ string find_first_not_of function.
    ///
    /// Returns: The first iterator i in the range [first1, last1) such that for some
    /// integer j in the range [first2, last2) the following conditions hold: pred(*i, *j) == false.
    /// Returns last1 if no such iterator is found.
    ///
    /// Complexity: At most '(last1 - first1) * (last2 - first2)' applications of the
    /// corresponding predicate.
    ///
    template <class ForwardIterator1, typename ForwardSentinel1, class ForwardIterator2, typename ForwardSentinel2, class BinaryPredicate>
    inline ForwardIterator1 find_first_not_of(
        ForwardIterator1 first1,
        ForwardSentinel1 last1,
        ForwardIterator2 first2,
        ForwardSentinel2 last2,
        BinaryPredicate predicate)
    {
        for (; first1 != last1; ++first1)
        {
            if (RAH_NAMESPACE::find_if(
                    first2,
                    last2,
                    [f1 = *first1, &predicate](auto&& v)
                    { return predicate(f1, RAH_STD::forward<decltype(v)>(v)); })
                == last2)
                break;
        }

        return first1;
    }

    template <class BidirectionalIterator1, class BidirectionalSentinel1, class ForwardIterator2, typename ForwardSentinel2>
    inline BidirectionalIterator1 find_last_of(
        BidirectionalIterator1 first1,
        BidirectionalSentinel1 last1,
        ForwardIterator2 first2,
        ForwardSentinel2 last2)
    {
        if ((first1 != last1) && (first2 != last2))
        {
            BidirectionalIterator1 it1(last1);

            while ((--it1 != first1) && (RAH_NAMESPACE::find(first2, last2, *it1) == last2))
                ; // Do nothing

            if ((it1 != first1) || (RAH_NAMESPACE::find(first2, last2, *it1) != last2))
                return it1;
        }

        return last1;
    }

    template <class BidirectionalIterator1, typename Sentinel1, class ForwardIterator2, typename ForwardSentinel2, class BinaryPredicate>
    BidirectionalIterator1 find_last_of(
        BidirectionalIterator1 first1,
        Sentinel1 last1,
        ForwardIterator2 first2,
        ForwardSentinel2 last2,
        BinaryPredicate predicate)
    {
        if ((first1 != last1) && (first2 != last2))
        {
            BidirectionalIterator1 it1(last1);

            while ((--it1 != first1)
                   && (RAH_NAMESPACE::find_if(
                           first2,
                           last2,
                           [v1 = *it1, predicate](auto&& v)
                           { return predicate(v1, RAH_STD::forward<decltype(v)>(v)); })
                       == last2))
                ; // Do nothing

            if ((it1 != first1)
                || (RAH_NAMESPACE::find_if(
                        first2,
                        last2,
                        [v1 = *it1, predicate](auto&& v)
                        { return predicate(v1, RAH_STD::forward<decltype(v)>(v)); })
                    != last2))
                return it1;
        }

        return last1;
    }

    template <class BidirectionalIterator1, typename Sentinel1, class ForwardIterator2, typename ForwardSentinel2>
    inline BidirectionalIterator1 find_last_not_of(
        BidirectionalIterator1 first1, Sentinel1 last1, ForwardIterator2 first2, ForwardSentinel2 last2)
    {
        if ((first1 != last1) && (first2 != last2))
        {
            BidirectionalIterator1 it1(last1);

            while ((--it1 != first1) && (RAH_NAMESPACE::find(first2, last2, *it1) != last2))
                ; // Do nothing

            if ((it1 != first1) || (RAH_NAMESPACE::find(first2, last2, *it1) == last2))
                return it1;
        }

        return last1;
    }

    template <class BidirectionalIterator1, typename Sentinel1, class ForwardIterator2, typename ForwardSentinel2, class BinaryPredicate>
    inline BidirectionalIterator1 find_last_not_of(
        BidirectionalIterator1 first1,
        Sentinel1 last1,
        ForwardIterator2 first2,
        ForwardSentinel2 last2,
        BinaryPredicate predicate)
    {
        if ((first1 != last1) && (first2 != last2))
        {
            BidirectionalIterator1 it1(last1);

            while ((--it1 != first1)
                   && (RAH_NAMESPACE::find_if(
                           first2,
                           last2,
                           [v1 = *it1, predicate](auto&& v)
                           { return predicate(v1, RAH_STD::forward<decltype(v)>(v)); })
                       != last2))
                ; // Do nothing

            if ((it1 != first1)
                || (RAH_NAMESPACE::find_if(
                       first2,
                       last2,
                       [v1 = *it1, predicate](auto&& v)
                       { return predicate(v1, RAH_STD::forward<decltype(v)>(v)); }))
                       != last2)
                return it1;
        }

        return last1;
    }

    /// for_each
    ///
    /// Calls the Function function for each value in the range [first, last).
    /// Function takes a single parameter: the current value.
    ///
    /// Effects: Applies function to the result of dereferencing every iterator in
    /// the range [first, last), starting from first and proceeding to last 1.
    ///
    /// Returns: function.
    ///
    /// Complexity: Applies function exactly 'last - first' times.
    ///
    /// Note: If function returns a result, the result is ignored.
    ///
    template <typename InputIterator, typename InputSentinel, typename Function>
    inline Function for_each(InputIterator first, InputSentinel last, Function function)
    {
        for (; first != last; ++first)
            function(*first);
        return function;
    }

    template <typename InputRange, typename Function>
    inline auto for_each(InputRange&& range, Function function)
    {
        return for_each(RAH_NAMESPACE::begin(range), RAH_NAMESPACE::end(range), std::move(function));
    }

    template <class I, class F>
    struct in_fun_result
    {
        I in;
        F fun;
    };

    template <class I, class F>
    using for_each_n_result = RAH_NAMESPACE::in_fun_result<I, F>;

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
    struct for_each_n_fn
    {
        template <typename I, class Proj = RAH_NAMESPACE::identity, typename Fun>
        constexpr for_each_n_result<I, Fun>
        operator()(I first, RAH_NAMESPACE::iter_difference_t<I> n, Fun fun, Proj proj = Proj{}) const
        {
            for (; n-- > 0; ++first)
                fun(proj(*first));
            return {RAH_STD::move(first), RAH_STD::move(fun)};
        }
    };

    constexpr for_each_n_fn for_each_n{};

    /// generate
    ///
    /// Iterates the range of [first, last) and assigns to each element the
    /// result of the function generator. Generator is a function which takes
    /// no arguments.
    ///
    /// Complexity: Exactly 'last - first' invocations of generator and assignments.
    ///
    struct generate_fn
    {
        template <
            typename O,
            typename S,
            typename F,
            std::enable_if_t<
                RAH_NAMESPACE::input_or_output_iterator<O> && RAH_NAMESPACE::sentinel_for<S, O>
                && RAH_NAMESPACE::copy_constructible<F>>* = nullptr>
        constexpr O operator()(O first, S last, F gen) const
        {
            for (; first != last; *first = RAH_INVOKE_0(gen), ++first)
            {
            }
            return first;
        }

        template <
            typename R,
            typename F,
            std::enable_if_t<true
                             // RAH_NAMESPACE::copy_constructible<F>
                             //&& RAH_NAMESPACE::output_range<R, decltype(std::declval<F&>())>
                             >* = nullptr>
        constexpr RAH_NAMESPACE::borrowed_iterator_t<R> operator()(R&& r, F gen) const
        {
            return (*this)(RAH_NAMESPACE::begin(r), RAH_NAMESPACE::end(r), std::move(gen));
        }
    };

    constexpr generate_fn generate{};

    /// generate_n
    ///
    /// Iterates an interator n times and assigns the result of generator
    /// to each succeeding element. Generator is a function which takes
    /// no arguments.
    ///
    /// Complexity: Exactly n invocations of generator and assignments.
    ///
    struct generate_n_fn
    {
        template <
            typename O,
            typename F,
            std::enable_if_t<
                RAH_NAMESPACE::input_or_output_iterator<O> && RAH_NAMESPACE::copy_constructible<F>>* = nullptr>
        constexpr O operator()(O first, RAH_NAMESPACE::iter_difference_t<O> n, F gen) const
        {
            for (; n-- > 0; *first = gen(), ++first)
            {
            }
            return first;
        }
    };

    constexpr generate_n_fn generate_n{};

    template <class I, class O>
    using unary_transform_result = RAH_NAMESPACE::in_out_result<I, O>;

    template <class I1, class I2, class O>
    using binary_transform_result = RAH_NAMESPACE::in_in_out_result<I1, I2, O>;

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
        std::enable_if_t<sentinel_for<InputSentinel, InputIterator>>* = nullptr>
    inline unary_transform_result<InputIterator, OutputIterator> transform(
        InputIterator first, InputSentinel last, OutputIterator result, UnaryOperation unaryOperation)
    {
        for (; first != last; ++first, ++result)
            *result = unaryOperation(*first);
        return {first, result};
    }
    template <typename Range, typename OutputIterator, typename UnaryOperation>
    inline unary_transform_result<iterator_t<Range>, OutputIterator>
    transform(Range&& range, OutputIterator result, UnaryOperation unaryOperation)
    {
        return RAH_NAMESPACE::transform(
            RAH_NAMESPACE::begin(range),
            RAH_NAMESPACE::end(range),
            RAH_STD::move(result),
            RAH_STD::move(unaryOperation));
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
        std::enable_if_t<
            sentinel_for<InputSentinel1, InputIterator1>
            && sentinel_for<InputSentinel2, InputIterator2>>* = nullptr>
    inline OutputIterator transform(
        InputIterator1 first1,
        InputSentinel1 last1,
        InputIterator2 first2,
        InputSentinel2 last2,
        OutputIterator result,
        BinaryOperation binaryOperation)
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
        std::enable_if_t<input_range<InputRange1> && input_range<InputRange2>>* = nullptr>
    inline OutputIterator transform(
        InputRange1&& range1, InputRange2 range2, OutputIterator result, BinaryOperation binaryOperation)
    {
        return transform(
            RAH_NAMESPACE::begin(range1),
            RAH_NAMESPACE::end(range1),
            RAH_NAMESPACE::begin(range2),
            RAH_NAMESPACE::end(range2),
            result,
            RAH_STD::move(binaryOperation));
    }

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
        std::enable_if_t<
            input_iterator<InputIterator1> && input_iterator<InputIterator2>
            && sentinel_for<InputSentinel1, InputIterator1>
            && sentinel_for<InputSentinel2, InputIterator2>>* = nullptr>
    RAH_CPP14_CONSTEXPR inline bool
    equal(InputIterator1 first1, InputSentinel1 last1, InputIterator2 first2, InputSentinel2 last2)
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
        std::enable_if_t<input_range<InputRange1> && input_range<InputRange2>>* = nullptr>
    RAH_CPP14_CONSTEXPR inline bool equal(InputRange1&& range1, InputRange2&& range2)
    {
        return equal(
            RAH_NAMESPACE::begin(range1),
            RAH_NAMESPACE::end(range1),
            RAH_NAMESPACE::begin(range2),
            RAH_NAMESPACE::end(range2));
    }

    /* Enable the following if there was shown to be some benefit. A glance and Microsoft VC++ memcmp
		shows that it is not optimized in any way, much less one that would benefit us here.

	inline bool equal(const bool* first1, const bool* last1, const bool* first2)
		{ return (memcmp(first1, first2, (size_t)((uintptr_t)last1 - (uintptr_t)first1)) == 0); }

	inline bool equal(const char* first1, const char* last1, const char* first2)
		{ return (memcmp(first1, first2, (size_t)((uintptr_t)last1 - (uintptr_t)first1)) == 0); }

	inline bool equal(const unsigned char* first1, const unsigned char* last1, const unsigned char* first2)
		{ return (memcmp(first1, first2, (size_t)((uintptr_t)last1 - (uintptr_t)first1)) == 0); }

	inline bool equal(const signed char* first1, const signed char* last1, const signed char* first2)
		{ return (memcmp(first1, first2, (size_t)((uintptr_t)last1 - (uintptr_t)first1)) == 0); }

	#ifndef EA_WCHAR_T_NON_NATIVE
		inline bool equal(const wchar_t* first1, const wchar_t* last1, const wchar_t* first2)
			{ return (memcmp(first1, first2, (size_t)((uintptr_t)last1 - (uintptr_t)first1)) == 0); }
	#endif

	inline bool equal(const int16_t* first1, const int16_t* last1, const int16_t* first2)
		{ return (memcmp(first1, first2, (size_t)((uintptr_t)last1 - (uintptr_t)first1)) == 0); }

	inline bool equal(const uint16_t* first1, const uint16_t* last1, const uint16_t* first2)
		{ return (memcmp(first1, first2, (size_t)((uintptr_t)last1 - (uintptr_t)first1)) == 0); }

	inline bool equal(const int32_t* first1, const int32_t* last1, const int32_t* first2)
		{ return (memcmp(first1, first2, (size_t)((uintptr_t)last1 - (uintptr_t)first1)) == 0); }

	inline bool equal(const uint32_t* first1, const uint32_t* last1, const uint32_t* first2)
		{ return (memcmp(first1, first2, (size_t)((uintptr_t)last1 - (uintptr_t)first1)) == 0); }

	inline bool equal(const int64_t* first1, const int64_t* last1, const int64_t* first2)
		{ return (memcmp(first1, first2, (size_t)((uintptr_t)last1 - (uintptr_t)first1)) == 0); }

	inline bool equal(const uint64_t* first1, const uint64_t* last1, const uint64_t* first2)
		{ return (memcmp(first1, first2, (size_t)((uintptr_t)last1 - (uintptr_t)first1)) == 0); }
	*/

    /// equal
    ///
    /// Returns: true if for every iterator i in the range [first1, last1) the
    /// following corresponding conditions hold: pred(*i, *(first2 + (i first1))) != false.
    /// Otherwise, returns false.
    ///
    /// Complexity: At most last1 first1 applications of the corresponding predicate.
    ///
    template <typename InputIterator1, typename InputSentinel1, typename InputIterator2, typename InputSentinel2, typename BinaryPredicate>
    inline bool equal(
        InputIterator1 first1,
        InputSentinel1 last1,
        InputIterator2 first2,
        InputSentinel2 last2,
        BinaryPredicate&& predicate)
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
        std::enable_if_t<input_range<InputRange1> && input_range<InputRange2>>* = nullptr>
    RAH_CPP14_CONSTEXPR inline bool
    equal(InputRange1&& range1, InputRange2 range2, BinaryPredicate&& predicate)
    {
        return equal(
            RAH_NAMESPACE::begin(range1),
            RAH_NAMESPACE::end(range1),
            RAH_NAMESPACE::begin(range2),
            RAH_NAMESPACE::end(range2),
            RAH_STD::forward<BinaryPredicate>(predicate));
    }

    /// identical
    ///
    /// Returns true if the two input ranges are equivalent.
    /// There is a subtle difference between this algorithm and
    /// the 'equal' algorithm. The equal algorithm assumes the
    /// two ranges are of equal length. This algorithm efficiently
    /// compares two ranges for both length equality and for
    /// element equality. There is no other standard algorithm
    /// that can do this.
    ///
    /// Returns: true if the sequence of elements defined by the range
    /// [first1, last1) is of the same length as the sequence of
    /// elements defined by the range of [first2, last2) and if
    /// the elements in these ranges are equal as per the
    /// equal algorithm.
    ///
    /// Complexity: At most 'min((last1 - first1), (last2 - first2))' applications
    /// of the corresponding comparison.
    ///
    template <typename InputIterator1, typename Sentinel1, typename InputIterator2, typename Sentinel2>
    bool identical(InputIterator1 first1, Sentinel1 last1, InputIterator2 first2, Sentinel2 last2)
    {
        while ((first1 != last1) && (first2 != last2) && (*first1 == *first2))
        {
            ++first1;
            ++first2;
        }
        return (first1 == last1) && (first2 == last2);
    }

    /// identical
    ///
    template <typename InputIterator1, typename Sentinel1, typename InputIterator2, typename Sentinel2, typename BinaryPredicate>
    bool identical(
        InputIterator1 first1,
        Sentinel1 last1,
        InputIterator2 first2,
        Sentinel2 last2,
        BinaryPredicate predicate)
    {
        while ((first1 != last1) && (first2 != last2) && predicate(*first1, *first2))
        {
            ++first1;
            ++first2;
        }
        return (first1 == last1) && (first2 == last2);
    }

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

    struct lexicographical_compare_fn
    {
        template <
            typename I1,
            typename S1,
            typename I2,
            typename S2,
            class Proj1 = RAH_NAMESPACE::identity,
            class Proj2 = RAH_NAMESPACE::identity,
            typename Comp = RAH_NAMESPACE::less>
        constexpr bool operator()(
            I1 first1, S1 last1, I2 first2, S2 last2, Comp comp = {}, Proj1 proj1 = {}, Proj2 proj2 = {}) const
        {
            for (; (first1 != last1) && (first2 != last2); ++first1, (void)++first2)
            {
                if (comp(proj1(*first1), proj2(*first2)))
                    return true;

                if (comp(proj2(*first2), proj1(*first1)))
                    return false;
            }
            return (first1 == last1) && (first2 != last2);
        }

        inline bool // Specialization for const char*.
        operator()(const char* first1, const char* last1, const char* first2, const char* last2)
        {
            const ptrdiff_t n1(last1 - first1), n2(last2 - first2);
            const int result = memcmp(first1, first2, (size_t)RAH_STD::min(n1, n2));
            return result ? (result < 0) : (n1 < n2);
        }

        inline bool // Specialization for char*.
        operator()(char* first1, char* last1, char* first2, char* last2)
        {
            const ptrdiff_t n1(last1 - first1), n2(last2 - first2);
            const int result = memcmp(first1, first2, (size_t)RAH_STD::min(n1, n2));
            return result ? (result < 0) : (n1 < n2);
        }

        inline bool // Specialization for const unsigned char*.
        operator()(
            const unsigned char* first1,
            const unsigned char* last1,
            const unsigned char* first2,
            const unsigned char* last2)
        {
            const ptrdiff_t n1(last1 - first1), n2(last2 - first2);
            const int result = memcmp(first1, first2, (size_t)RAH_STD::min(n1, n2));
            return result ? (result < 0) : (n1 < n2);
        }

        inline bool // Specialization for unsigned char*.
        operator()(unsigned char* first1, unsigned char* last1, unsigned char* first2, unsigned char* last2)
        {
            const ptrdiff_t n1(last1 - first1), n2(last2 - first2);
            const int result = memcmp(first1, first2, (size_t)RAH_STD::min(n1, n2));
            return result ? (result < 0) : (n1 < n2);
        }

        inline bool // Specialization for const signed char*.
        operator()(
            const signed char* first1,
            const signed char* last1,
            const signed char* first2,
            const signed char* last2)
        {
            const ptrdiff_t n1(last1 - first1), n2(last2 - first2);
            const int result = memcmp(first1, first2, (size_t)RAH_STD::min(n1, n2));
            return result ? (result < 0) : (n1 < n2);
        }

        inline bool // Specialization for signed char*.
        operator()(signed char* first1, signed char* last1, signed char* first2, signed char* last2)
        {
            const ptrdiff_t n1(last1 - first1), n2(last2 - first2);
            const int result = memcmp(first1, first2, (size_t)RAH_STD::min(n1, n2));
            return result ? (result < 0) : (n1 < n2);
        }

        template <
            typename R1,
            typename R2,
            class Proj1 = RAH_NAMESPACE::identity,
            class Proj2 = RAH_NAMESPACE::identity,
            typename Comp = RAH_NAMESPACE::less>
        constexpr bool
        operator()(R1&& r1, R2&& r2, Comp comp = {}, Proj1 proj1 = {}, Proj2 proj2 = {}) const
        {
            return (*this)(
                RAH_NAMESPACE::begin(r1),
                RAH_NAMESPACE::end(r1),
                RAH_NAMESPACE::begin(r2),
                RAH_NAMESPACE::end(r2),
                std::ref(comp),
                std::ref(proj1),
                std::ref(proj2));
        }
    };

    constexpr lexicographical_compare_fn lexicographical_compare;

    template <class I1, class I2>
    struct in_in_result
    {
        I1 in1;
        I2 in2;
    };

    template <class I1, class I2>
    using mismatch_result = RAH_NAMESPACE::in_in_result<I1, I2>;

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
    struct mismatch_fn
    {
        template <
            typename I1,
            typename S1,
            typename I2,
            typename S2,
            class Pred = RAH_NAMESPACE::equal_to,
            class Proj1 = RAH_NAMESPACE::identity,
            class Proj2 = RAH_NAMESPACE::identity>
        constexpr RAH_NAMESPACE::mismatch_result<I1, I2> operator()(
            I1 first1, S1 last1, I2 first2, S2 last2, Pred pred = {}, Proj1 proj1 = {}, Proj2 proj2 = {}) const
        {
            for (; first1 != last1 && first2 != last2; ++first1, (void)++first2)
                if (not pred(proj1(*first1), proj2(*first2)))
                    break;

            return {first1, first2};
        }

        template <
            typename R1,
            typename R2,
            class Pred = RAH_NAMESPACE::equal_to,
            class Proj1 = RAH_NAMESPACE::identity,
            class Proj2 = RAH_NAMESPACE::identity>
        constexpr RAH_NAMESPACE::mismatch_result<borrowed_iterator_t<R1>, borrowed_iterator_t<R2>>
        operator()(R1&& r1, R2&& r2, Pred pred = {}, Proj1 proj1 = {}, Proj2 proj2 = {}) const
        {
            return (*this)(
                RAH_NAMESPACE::begin(r1),
                RAH_NAMESPACE::end(r1),
                RAH_NAMESPACE::begin(r2),
                RAH_NAMESPACE::end(r2),
                std::ref(pred),
                std::ref(proj1),
                std::ref(proj2));
        }
    };

    constexpr mismatch_fn mismatch;

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
    template <typename ForwardIterator, typename ForwardSentinel, typename T>
    ForwardIterator lower_bound(ForwardIterator first, ForwardSentinel last, const T& value)
    {
        typedef typename RAH_STD::iterator_traits<ForwardIterator>::difference_type DifferenceType;

        DifferenceType d = RAH_NAMESPACE::distance(
            first, last); // This will be efficient for a random access iterator such as an array.

        while (d > 0)
        {
            ForwardIterator i = first;
            DifferenceType d2 =
                d >> 1; // We use '>>1' here instead of '/2' because MSVC++ for some reason generates significantly worse code for '/2'. Go figure.

            RAH_NAMESPACE::advance(
                i, d2); // This will be efficient for a random access iterator such as an array.

            if (*i < value)
            {
                // Disabled because std::lower_bound doesn't specify (23.3.3.3, p3) this can be done: EASTL_VALIDATE_COMPARE(!(value < *i)); // Validate that the compare function is sane.
                first = ++i;
                d -= d2 + 1;
            }
            else
                d = d2;
        }
        return first;
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
    ForwardIterator
    lower_bound(ForwardIterator first, ForwardSentinel last, const T& value, Compare compare)
    {
        typedef typename RAH_STD::iterator_traits<ForwardIterator>::difference_type DifferenceType;

        DifferenceType d = RAH_NAMESPACE::distance(
            first, last); // This will be efficient for a random access iterator such as an array.

        while (d > 0)
        {
            ForwardIterator i = first;
            DifferenceType d2 =
                d >> 1; // We use '>>1' here instead of '/2' because MSVC++ for some reason generates significantly worse code for '/2'. Go figure.

            RAH_NAMESPACE::advance(
                i, d2); // This will be efficient for a random access iterator such as an array.

            if (compare(*i, value))
            {
                // Disabled because std::lower_bound doesn't specify (23.3.3.1, p3) this can be done: EASTL_VALIDATE_COMPARE(!compare(value, *i)); // Validate that the compare function is sane.
                first = ++i;
                d -= d2 + 1;
            }
            else
                d = d2;
        }
        return first;
    }

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
    template <typename ForwardIterator, typename ForwardSentinel, typename T>
    ForwardIterator upper_bound(ForwardIterator first, ForwardSentinel last, const T& value)
    {
        typedef typename RAH_STD::iterator_traits<ForwardIterator>::difference_type DifferenceType;

        DifferenceType len = RAH_NAMESPACE::distance(first, last);

        while (len > 0)
        {
            ForwardIterator i = first;
            DifferenceType len2 =
                len >> 1; // We use '>>1' here instead of '/2' because MSVC++ for some reason generates significantly worse code for '/2'. Go figure.

            RAH_NAMESPACE::advance(i, len2);

            if (!(value < *i)) // Note that we always express value comparisons in terms of < or ==.
            {
                first = ++i;
                len -= len2 + 1;
            }
            else
            {
                // Disabled because std::upper_bound doesn't specify (23.3.3.2, p3) this can be done: EASTL_VALIDATE_COMPARE(!(*i < value)); // Validate that the compare function is sane.
                len = len2;
            }
        }
        return first;
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
    ForwardIterator
    upper_bound(ForwardIterator first, ForwardSentinel last, const T& value, Compare compare)
    {
        typedef typename RAH_STD::iterator_traits<ForwardIterator>::difference_type DifferenceType;

        DifferenceType len = RAH_NAMESPACE::distance(first, last);

        while (len > 0)
        {
            ForwardIterator i = first;
            DifferenceType len2 =
                len >> 1; // We use '>>1' here instead of '/2' because MSVC++ for some reason generates significantly worse code for '/2'. Go figure.

            RAH_NAMESPACE::advance(i, len2);

            if (!compare(value, *i))
            {
                first = ++i;
                len -= len2 + 1;
            }
            else
            {
                // Disabled because std::upper_bound doesn't specify (23.3.3.2, p3) this can be done: EASTL_VALIDATE_COMPARE(!compare(*i, value)); // Validate that the compare function is sane.
                len = len2;
            }
        }
        return first;
    }

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
        std::enable_if_t<sentinel_for<Sentinel, ForwardIterator>>* = nullptr>
    RAH_NAMESPACE::subrange<ForwardIterator>
    equal_range(ForwardIterator first, Sentinel last, const T& value)
    {
        typedef typename RAH_STD::iterator_traits<ForwardIterator>::difference_type DifferenceType;

        DifferenceType d = RAH_NAMESPACE::distance(first, last);

        while (d > 0)
        {
            ForwardIterator i(first);
            DifferenceType d2 =
                d >> 1; // We use '>>1' here instead of '/2' because MSVC++ for some reason generates significantly worse code for '/2'. Go figure.

            RAH_NAMESPACE::advance(i, d2);

            if (*i < value)
            {
                RAH_ASSERT(!(value < *i)); // Validate that the compare function is sane.
                first = ++i;
                d -= d2 + 1;
            }
            else if (value < *i)
            {
                RAH_ASSERT(!(*i < value)); // Validate that the compare function is sane.
                d = d2;
                last = i;
            }
            else
            {
                ForwardIterator j(i);

                return {
                    RAH_NAMESPACE::lower_bound(first, i, value),
                    RAH_NAMESPACE::upper_bound(++j, last, value)};
            }
        }
        return {first, first};
    }

    template <typename ForwardRange, typename T>
    RAH_NAMESPACE::borrowed_subrange_t<ForwardRange> equal_range(ForwardRange&& range, const T& value)
    {
        return RAH_NAMESPACE::equal_range(
            RAH_NAMESPACE::begin(range), RAH_NAMESPACE::end(range), value);
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
        std::enable_if_t<sentinel_for<Sentinel, ForwardIterator>>* = nullptr>
    RAH_NAMESPACE::subrange<ForwardIterator, ForwardIterator>
    equal_range(ForwardIterator first, Sentinel last, const T& value, Compare compare)
    {
        typedef typename RAH_STD::iterator_traits<ForwardIterator>::difference_type DifferenceType;

        DifferenceType d = RAH_NAMESPACE::distance(first, last);

        while (d > 0)
        {
            ForwardIterator i(first);
            DifferenceType d2 =
                d >> 1; // We use '>>1' here instead of '/2' because MSVC++ for some reason generates significantly worse code for '/2'. Go figure.

            RAH_NAMESPACE::advance(i, d2);

            if (compare(*i, value))
            {
                RAH_ASSERT(!compare(value, *i)); // Validate that the compare function is sane.
                first = ++i;
                d -= d2 + 1;
            }
            else if (compare(value, *i))
            {
                RAH_ASSERT(!compare(*i, value)); // Validate that the compare function is sane.
                d = d2;
                last = i;
            }
            else
            {
                ForwardIterator j(i);

                return {
                    RAH_NAMESPACE::lower_bound(first, i, value, compare),
                    RAH_NAMESPACE::upper_bound(++j, last, value, compare)};
            }
        }
        return {first, first};
    }

    template <typename ForwardRange, typename T, typename Compare>
    RAH_NAMESPACE::subrange<iterator_t<ForwardRange>, sentinel_t<ForwardRange>>
    equal_range(ForwardRange&& range, const T& value, Compare compare)
    {
        return RAH_NAMESPACE::equal_range(
            RAH_NAMESPACE::begin(range), RAH_NAMESPACE::end(range), value, RAH_STD::move(compare));
    }

    struct replace_fn
    {
        template <
            typename I,
            typename S,
            class T1,
            class T2,
            std::enable_if_t<input_iterator<I> && sentinel_for<S, I> && indirectly_writable<I, const T2&>>* = nullptr>
        constexpr I operator()(I first, S last, const T1& old_value, const T2& new_value) const
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
            std::enable_if_t<input_range<R> && indirectly_writable<iterator_t<R>, const T2&>>* = nullptr>
        constexpr borrowed_iterator_t<R> operator()(R&& r, const T1& old_value, const T2& new_value) const
        {
            return (*this)(RAH_NAMESPACE::begin(r), RAH_NAMESPACE::end(r), old_value, new_value);
        }
    };

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
    constexpr replace_fn replace{};

    struct replace_if_fn
    {
        template <
            typename I, // input_iterator
            typename S, // std::sentinel_for<I>
            class T,
            typename Pred, // indirect_unary_predicate<I>
            std::enable_if_t<input_iterator<I> && sentinel_for<S, I> && indirectly_writable<I, const T&>>* = nullptr>
        constexpr I operator()(I first, S last, Pred pred, const T& new_value) const
        {
            for (; first != last; ++first)
                if (pred(*first))
                    *first = new_value;
            return std::move(first);
        }

        template <
            typename R, // input_range
            class T,
            typename Pred, // indirect_unary_predicate<iterator_t<R>>>
            std::enable_if_t<input_range<R> && output_range<R, T>>* = nullptr>
        constexpr borrowed_iterator_t<R> operator()(R&& r, Pred pred, const T& new_value) const
        {
            return (*this)(
                RAH_NAMESPACE::begin(r), RAH_NAMESPACE::end(r), std::move(pred), new_value);
        }
    };
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
    constexpr replace_if_fn replace_if{};

    template <class I, class O>
    using remove_copy_result = RAH_NAMESPACE::in_out_result<I, O>;

    struct remove_copy_fn
    {
        template <
            typename I,
            typename S,
            typename O,
            class T,
            std::enable_if_t<
                RAH_NAMESPACE::input_iterator<I> && RAH_NAMESPACE::sentinel_for<S, I>
                && RAH_NAMESPACE::weakly_incrementable<O> && RAH_NAMESPACE::indirectly_copyable<I, O>>* = nullptr>
        constexpr RAH_NAMESPACE::remove_copy_result<I, O>
        operator()(I first, S last, O result, const T& value) const
        {
            for (; !(first == last); ++first)
            {
                if (value != *first)
                {
                    *result = *first;
                    ++result;
                }
            }
            return {std::move(first), std::move(result)};
        }

        template <
            typename R,
            typename O,
            class T,
            std::enable_if_t<
                RAH_NAMESPACE::input_range<R> && RAH_NAMESPACE::weakly_incrementable<O>
                && RAH_NAMESPACE::indirectly_copyable<RAH_NAMESPACE::iterator_t<R>, O>>* = nullptr>
        constexpr RAH_NAMESPACE::remove_copy_result<RAH_NAMESPACE::borrowed_iterator_t<R>, O>
        operator()(R&& r, O result, const T& value) const
        {
            return (*this)(RAH_NAMESPACE::begin(r), RAH_NAMESPACE::end(r), std::move(result), value);
        }
    };
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
    constexpr remove_copy_fn remove_copy{};

    template <class I, class O>
    using remove_copy_if_result = RAH_NAMESPACE::in_out_result<I, O>;
    struct remove_copy_if_fn
    {
        template <
            typename I, // std::input_iterator
            typename S, // std::sentinel_for<I>
            typename O, // std::weakly_incrementable
            typename Pred, // std::indirect_unary_predicate<std::projected<I, Proj>>
            std::enable_if_t<
                input_iterator<I> && sentinel_for<S, I> && weakly_incrementable<O>
                && indirectly_copyable<I, O>>* = nullptr>
        constexpr RAH_NAMESPACE::remove_copy_if_result<I, O>
        operator()(I first, S last, O result, Pred pred) const
        {
            for (; first != last; ++first)
            {
                if (false == pred(*first))
                {
                    *result = *first;
                    ++result;
                }
            }
            return {std::move(first), std::move(result)};
        }

        template <
            typename R, // RAH_NAMESPACE::input_range
            typename O, // RAH_NAMESPACE::weakly_incrementable
            typename Pred,
            std::enable_if_t<
                input_range<R> && weakly_incrementable<O> && indirectly_copyable<iterator_t<R>, O>>* = nullptr>
        constexpr RAH_NAMESPACE::remove_copy_if_result<RAH_NAMESPACE::borrowed_iterator_t<R>, O>
        operator()(R&& r, O result, Pred pred) const
        {
            return (*this)(
                RAH_NAMESPACE::begin(r), RAH_NAMESPACE::end(r), std::move(result), std::move(pred));
        }
    };

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
    constexpr remove_copy_if_fn remove_copy_if{};

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
    subrange<ForwardIterator> remove(ForwardIterator first, ForwardSentinel last, const T& value)
    {
        first = RAH_NAMESPACE::find(first, last, value);
        if (first != last)
        {
            ForwardIterator i(first);
            return {RAH_NAMESPACE::remove_copy(++i, last, first, value).out, last};
        }
        return {first, last};
    }

    template <typename ForwardRange, typename T>
    borrowed_subrange_t<ForwardRange> remove(ForwardRange&& range, const T& value)
    {
        return RAH_NAMESPACE::remove(RAH_NAMESPACE::begin(range), RAH_NAMESPACE::end(range), value);
    }

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
    subrange<ForwardIterator> remove_if(ForwardIterator first, ForwardSentinel last, Predicate predicate)
    {
        first = RAH_NAMESPACE::find_if(first, last, predicate);
        if (first != last)
        {
            ForwardIterator i(first);
            return {RAH_NAMESPACE::remove_copy_if(++i, last, first, predicate).out, last};
        }
        return {first, last};
    }

    template <typename ForwardRange, typename Predicate>
    borrowed_subrange_t<ForwardRange> remove_if(ForwardRange&& range, Predicate predicate)
    {
        return RAH_NAMESPACE::remove_if(
            RAH_NAMESPACE::begin(range), RAH_NAMESPACE::end(range), RAH_STD::move(predicate));
    }

    /// apply_and_remove_if
    ///
    /// Calls the Function function for all elements referred to  my iterator i in the range
    /// [first, last) for which the following corresponding condition holds:
    /// predicate(*i) == true
    /// and then left shift moves potential non-matching elements over it.
    ///
    /// Returns: a past-the-end iterator for the new end of the range.
    ///
    /// Complexity: Exactly 'last - first' applications of the corresponding predicate + applies
    /// function once for every time the condition holds.
    ///
    /// Note: Since removing is done by shifting (by means of copy move assignment) the elements
    /// in the range in such a way that the elements that are not to be removed appear in the
    /// beginning of the range doesn't actually remove it from the given container, the user must call
    /// the container erase function if the user wants to erase the element
    /// from the container. I.e. in the same they as for remove_if the excess elements
    /// are left in a valid but possibly moved from state.
    ///
    template <typename ForwardIterator, typename ForwardSentinel, typename Function, typename Predicate>
    inline ForwardIterator apply_and_remove_if(
        ForwardIterator first, ForwardSentinel last, Function function, Predicate predicate)
    {
        first = RAH_NAMESPACE::find_if(first, last, predicate);
        if (first != last)
        {
            function(*first);
            for (auto i = next(first); i != last; ++i)
            {
                if (predicate(*i))
                {
                    function(*i);
                    continue;
                }
                *first = RAH_STD::move(*i);
                ++first;
            }
        }
        return first;
    }

    /// apply_and_remove
    ///
    /// Calls the Function function for all elements referred to my iterator i in the range
    /// [first, last) for which the following corresponding condition holds:
    /// value == *i
    /// and then left shift moves potential non-matching elements over it.
    ///
    /// Returns: a past-the-end iterator for the new end of the range.
    ///
    /// Complexity: Exactly 'last - first' applications of the corresponding equality test
    /// + applies function once for every time the condition holds.
    ///
    /// Note: Since removing is done by shifting (by means of copy move assignment) the elements
    /// in the range in such a way that the elements that are not to be removed appear in the
    /// beginning of the range doesn't actually remove it from the given container, the user must call
    /// the container erase function if the user wants to erase the element
    /// from the container. I.e. in the same they as for remove_if the excess elements
    /// are left in a valid but possibly moved from state.
    ///
    template <typename ForwardIterator, typename ForwardSentinel, typename Function, typename T>
    inline ForwardIterator
    apply_and_remove(ForwardIterator first, ForwardSentinel last, Function function, const T& value)
    {
        first = RAH_NAMESPACE::find(first, last, value);
        if (first != last)
        {
            function(*first);
            for (auto i = next(first); i != last; ++i)
            {
                if (value == *i)
                {
                    function(*i);
                    continue;
                }
                *first = RAH_STD::move(*i);
                ++first;
            }
        }
        return first;
    }

    template <class I, class O>
    using replace_copy_result = RAH_NAMESPACE::in_out_result<I, O>;

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
    inline replace_copy_result<InputIterator, OutputIterator> replace_copy(
        InputIterator first,
        InputSentinel last,
        OutputIterator result,
        const T& old_value,
        const T& new_value)
    {
        for (; first != last; ++first, ++result)
            *result = (*first == old_value) ? new_value : *first;
        return {first, result};
    }

    template <typename InputRange, typename OutputIterator, typename T>
    inline replace_copy_result<RAH_NAMESPACE::borrowed_iterator_t<InputRange>, OutputIterator>
    replace_copy(InputRange&& range, OutputIterator result, const T& old_value, const T& new_value)
    {
        return RAH_NAMESPACE::replace_copy(
            RAH_NAMESPACE::begin(range),
            RAH_NAMESPACE::end(range),
            std::move(result),
            old_value,
            new_value);
    }

    template <class I, class O>
    using replace_copy_if_result = RAH_NAMESPACE::in_out_result<I, O>;

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
    inline replace_copy_if_result<InputIterator, OutputIterator> replace_copy_if(
        InputIterator first,
        InputSentinel last,
        OutputIterator result,
        Predicate predicate,
        const T& new_value)
    {
        for (; first != last; ++first, ++result)
            *result = predicate(*first) ? new_value : *first;
        return {first, result};
    }

    template <typename InputRange, typename OutputIterator, typename Predicate, typename T>
    inline replace_copy_if_result<RAH_NAMESPACE::borrowed_iterator_t<InputRange>, OutputIterator>
    replace_copy_if(InputRange&& range, OutputIterator result, Predicate predicate, const T& new_value)
    {
        return RAH_NAMESPACE::replace_copy_if(
            RAH_NAMESPACE::begin(range),
            RAH_NAMESPACE::end(range),
            RAH_STD::move(result),
            RAH_STD::move(predicate),
            new_value);
    }

    // reverse
    //
    // We provide helper functions which allow reverse to be implemented more
    // efficiently for some types of iterators and types.
    //
    template <typename BidirectionalIterator, typename Sentinel>
    inline BidirectionalIterator
    reverse_impl(BidirectionalIterator first, Sentinel last, RAH_ITC_NS::bidirectional_iterator_tag)
    {
        for (; (first != last) && (first != --last);
             ++first) // We are not allowed to use operator <, <=, >, >= with a
            RAH_STD::iter_swap(first, last); // generic (bidirectional or otherwise) iterator.
        return first;
    }

    template <typename RandomAccessIterator, typename Sentinel>
    inline RandomAccessIterator
    reverse_impl(RandomAccessIterator first, Sentinel last, RAH_ITC_NS::random_access_iterator_tag)
    {
        if (first != last)
        {
            for (; first < --last;
                 ++first) // With a random access iterator, we can use operator < to more efficiently implement
                RAH_STD::iter_swap(
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
    inline BidirectionalIterator reverse(BidirectionalIterator first, Sentinel last)
    {
        typedef typename RAH_STD::iterator_traits<BidirectionalIterator>::iterator_category IC;
        return reverse_impl(first, last, IC());
    }

    template <typename BidirectionalRange>
    inline borrowed_iterator_t<BidirectionalRange> reverse(BidirectionalRange&& range)
    {
        using IC = range_iter_categ_t<BidirectionalRange>;
        return reverse_impl(RAH_NAMESPACE::begin(range), RAH_NAMESPACE::end(range), IC());
    }

    template <class I, class O>
    using reverse_copy_result = RAH_NAMESPACE::in_out_result<I, O>;

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
    inline reverse_copy_result<BidirectionalIterator, OutputIterator>
    reverse_copy(BidirectionalIterator first, Sentinel last, OutputIterator result)
    {
        auto ret = RAH_NAMESPACE::next(first, last);
        for (; last != first; ++result)
            *result = *--last;
        return {std::move(ret), std::move(result)};
    }

    template <typename BidirectionalRange, typename OutputIterator>
    inline reverse_copy_result<RAH_NAMESPACE::borrowed_iterator_t<BidirectionalRange>, OutputIterator>
    reverse_copy(BidirectionalRange&& range, OutputIterator result)
    {
        return RAH_NAMESPACE::reverse_copy(
            RAH_NAMESPACE::begin(range), RAH_NAMESPACE::end(range), RAH_STD::move(result));
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
    struct search_fn
    {
        template <
            typename I1,
            typename S1,
            typename I2,
            typename S2,
            class Pred = RAH_NAMESPACE::equal_to,
            class Proj1 = RAH_NAMESPACE::identity,
            class Proj2 = RAH_NAMESPACE::identity,
            std::enable_if_t<forward_iterator<I1> && forward_iterator<I2>>* = nullptr>
        constexpr subrange<I1> operator()(
            I1 first1, S1 last1, I2 first2, S2 last2, Pred pred = {}, Proj1 proj1 = {}, Proj2 proj2 = {}) const
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
            class Pred = RAH_NAMESPACE::equal_to,
            class Proj1 = RAH_NAMESPACE::identity,
            class Proj2 = RAH_NAMESPACE::identity,
            std::enable_if_t<forward_range<R1> && forward_range<R2>>* = nullptr>
        constexpr std::enable_if_t<forward_range<R1>, subrange<iterator_t<R1>>>
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

    constexpr search_fn search{};

    // search_n helper functions
    //
    template <typename ForwardIterator, typename ForwardSentinel, typename Size, typename T>
    ForwardIterator // Generic implementation.
    search_n_impl(
        ForwardIterator first,
        ForwardSentinel last,
        Size count,
        const T& value,
        RAH_ITC_NS::forward_iterator_tag)
    {
        if (count <= 0)
            return first;

        Size d1 = (Size)RAH_NAMESPACE::distance(
            first,
            last); // Should d1 be of type Size, ptrdiff_t, or iterator_traits<ForwardIterator>::difference_type?
        // The problem with using iterator_traits<ForwardIterator>::difference_type is that
        if (count > d1) // ForwardIterator may not be a true iterator but instead something like a pointer.
            return last;

        for (; d1 >= count; ++first, --d1)
        {
            ForwardIterator i(first);

            for (Size n = 0; n < count; ++n, ++i, --d1)
            {
                if (!(*i == value)) // Note that we always express value comparisons in terms of < or ==.
                    goto not_found;
            }
            return first;

        not_found:
            first = i;
        }
        return last;
    }

    template <typename RandomAccessIterator, typename Sentinel, typename Size, typename T>
    inline RandomAccessIterator // Random access iterator implementation. Much faster than generic implementation.
    search_n_impl(
        RandomAccessIterator first,
        Sentinel last,
        Size count,
        const T& value,
        RAH_ITC_NS::random_access_iterator_tag)
    {
        if (count <= 0)
            return first;
        else if (count == 1)
            return RAH_NAMESPACE::find(first, last, value);
        else if (last > first)
        {
            RandomAccessIterator lookAhead;
            RandomAccessIterator backTrack;

            Size skipOffset = (count - 1);
            Size tailSize = (Size)(last - first);
            Size remainder;
            Size prevRemainder;

            for (lookAhead = first + skipOffset; tailSize >= count; lookAhead += count)
            {
                tailSize -= count;

                if (*lookAhead == value)
                {
                    remainder = skipOffset;

                    for (backTrack = lookAhead - 1; *backTrack == value; --backTrack)
                    {
                        if (--remainder == 0)
                            return (lookAhead - skipOffset); // success
                    }

                    if (remainder <= tailSize)
                    {
                        prevRemainder = remainder;

                        while (*(++lookAhead) == value)
                        {
                            if (--remainder == 0)
                                return (backTrack + 1); // success
                        }
                        tailSize -= (prevRemainder - remainder);
                    }
                    else
                        return last; // failure
                }

                // lookAhead here is always pointing to the element of the last mismatch.
            }
        }

        return last; // failure
    }

    /// search_n
    ///
    /// Returns: The first iterator i in the range [first, last count) such that
    /// for any nonnegative integer n less than count the following corresponding
    /// conditions hold: *(i + n) == value, pred(*(i + n),value) != false.
    /// Returns last if no such iterator is found.
    ///
    /// Complexity: At most '(last1 - first1) * count' applications of the corresponding predicate.
    ///
    struct search_n_fn
    {
        // TODO : Make a faster random_access version like in EASTL
        template <
            typename I,
            typename S,
            class T,
            class Pred = RAH_NAMESPACE::equal_to,
            class Proj = RAH_NAMESPACE::identity,
            std::enable_if_t<forward_iterator<I> && sentinel_for<S, I>>* = nullptr>
        constexpr RAH_NAMESPACE::subrange<I> operator()(
            I first,
            S last,
            RAH_NAMESPACE::iter_difference_t<I> count,
            const T& value,
            Pred pred = {},
            Proj proj = {}) const
        {
            if (count <= 0)
                return {first, first};
            for (; first != last; ++first)
            {
                if (RAH_INVOKE_2(pred, RAH_INVOKE_1(proj, *first), value))
                {
                    I start = first;
                    RAH_NAMESPACE::iter_difference_t<I> n{1};
                    for (;;)
                    {
                        if (n++ == count)
                            return {start, std::next(first)}; // found
                        if (++first == last)
                            return {first, first}; // not found
                        if (!RAH_INVOKE_2(pred, RAH_INVOKE_1(proj, *first), value))
                            break; // not equ to value
                    }
                }
            }
            return {first, first};
        }

        template <
            typename R,
            class T,
            class Pred = RAH_NAMESPACE::equal_to,
            class Proj = RAH_NAMESPACE::identity,
            std::enable_if_t<forward_range<R>>* = nullptr>
        constexpr RAH_NAMESPACE::borrowed_subrange_t<R> operator()(
            R&& r,
            RAH_NAMESPACE::range_difference_t<R> count,
            const T& value,
            Pred pred = {},
            Proj proj = {}) const
        {
            return (*this)(
                RAH_NAMESPACE::begin(r),
                RAH_NAMESPACE::end(r),
                RAH_STD::move(count),
                value,
                RAH_STD::move(pred),
                RAH_STD::move(proj));
        }
    };

    constexpr search_n_fn search_n{};

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
        std::enable_if_t<sentinel_for<Sentinel, ForwardIterator>>* = nullptr>
    inline bool binary_search(ForwardIterator first, Sentinel last, const T& value)
    {
        // To do: This can be made slightly faster by not using lower_bound.
        ForwardIterator i(RAH_NAMESPACE::lower_bound<ForwardIterator, T>(first, last, value));
        return (
            (i != last)
            && !(value < *i)); // Note that we always express value comparisons in terms of < or ==.
    }

    template <typename Range, typename T>
    inline bool binary_search(Range&& range, const T& value)
    {
        return binary_search(RAH_NAMESPACE::begin(range), RAH_NAMESPACE::end(range), value);
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
        std::enable_if_t<sentinel_for<Sentinel, ForwardIterator>>* = nullptr>
    inline bool binary_search(ForwardIterator first, Sentinel last, const T& value, Compare compare)
    {
        // To do: This can be made slightly faster by not using lower_bound.
        ForwardIterator i(
            RAH_NAMESPACE::lower_bound<ForwardIterator, T, Compare>(first, last, value, compare));
        return ((i != last) && !compare(value, *i));
    }

    template <typename Range, typename T, typename Compare>
    inline bool binary_search(Range&& range, const T& value, Compare compare)
    {
        return binary_search(
            RAH_NAMESPACE::begin(range), RAH_NAMESPACE::end(range), value, RAH_STD::move(compare));
    }

    /// binary_search_i
    ///
    /// Returns: iterator if there is an iterator i in the range [first last) that
    /// satisfies the corresponding conditions: !(*i < value) && !(value < *i).
    /// Returns last if the value is not found.
    ///
    /// Complexity: At most 'log(last - first) + 2' comparisons.
    ///
    template <typename ForwardIterator, typename ForwardSentinel, typename T>
    inline ForwardIterator binary_search_i(ForwardIterator first, ForwardSentinel last, const T& value)
    {
        // To do: This can be made slightly faster by not using lower_bound.
        ForwardIterator i(RAH_NAMESPACE::lower_bound<ForwardIterator, T>(first, last, value));
        if ((i != last)
            && !(value < *i)) // Note that we always express value comparisons in terms of < or ==.
            return i;
        return last;
    }

    /// binary_search_i
    ///
    /// Returns: iterator if there is an iterator i in the range [first last) that
    /// satisfies the corresponding conditions: !(*i < value) && !(value < *i).
    /// Returns last if the value is not found.
    ///
    /// Complexity: At most 'log(last - first) + 2' comparisons.
    ///
    template <typename ForwardIterator, typename ForwardSentinel, typename T, typename Compare>
    inline ForwardIterator
    binary_search_i(ForwardIterator first, ForwardSentinel last, const T& value, Compare compare)
    {
        // To do: This can be made slightly faster by not using lower_bound.
        ForwardIterator i(
            RAH_NAMESPACE::lower_bound<ForwardIterator, T, Compare>(first, last, value, compare));
        if ((i != last) && !compare(value, *i))
            return i;
        return last;
    }

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
    struct unique_fn
    {
        template <
            typename I,
            typename S,
            class Proj = RAH_NAMESPACE::identity,
            typename C = RAH_NAMESPACE::equal_to,
            std::enable_if_t<forward_iterator<I> && sentinel_for<S, I>>* = nullptr>
        constexpr subrange<I> operator()(I first, S last, C comp = {}, Proj proj = {}) const
        {
            first = RAH_NAMESPACE::adjacent_find(first, last, comp, proj);
            if (first == last)
                return {first, first};
            auto i{first};
            ++first;
            while (++first != last)
            {
                if (!comp(proj(*i), proj(*first)))
                    *++i = RAH_NAMESPACE::iter_move(first);
            }
            return {++i, first};
        }

        template <
            typename R,
            class Proj = RAH_NAMESPACE::identity,
            typename C = RAH_NAMESPACE::equal_to,
            std::enable_if_t<forward_range<R>>* = nullptr>
        constexpr borrowed_subrange_t<R> operator()(R&& r, C comp = {}, Proj proj = {}) const
        {
            return (*this)(
                RAH_NAMESPACE::begin(r), RAH_NAMESPACE::end(r), std::move(comp), std::move(proj));
        }
    };

    constexpr unique_fn unique{};

    // find_end
    //
    // We provide two versions here, one for a bidirectional iterators and one for
    // regular forward iterators. Given that we are searching backward, it's a bit
    // more efficient if we can use backwards iteration to implement our search,
    // though this requires an iterator that can be reversed.
    //
    struct find_end_fn
    {
        //TODO search from end when reversible
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
        constexpr RAH_NAMESPACE::subrange<I1> operator()(
            I1 first1, S1 last1, I2 first2, S2 last2, Pred pred = {}, Proj1 proj1 = {}, Proj2 proj2 = {}) const
        {
            if (first2 == last2)
            {
                auto last_it = RAH_NAMESPACE::next(first1, last1);
                return {last_it, last_it};
            }
            auto result =
                RAH_NAMESPACE::search(std::move(first1), last1, first2, last2, pred, proj1, proj2);

            if (result.empty())
                return result;

            for (;;)
            {
                auto new_result = RAH_NAMESPACE::search(
                    std::next(result.begin()), last1, first2, last2, pred, proj1, proj2);
                if (new_result.empty())
                    return result;
                else
                    result = std::move(new_result);
            }
        }

        template <
            typename R1,
            typename R2,
            class Pred = RAH_NAMESPACE::equal_to,
            class Proj1 = RAH_NAMESPACE::identity,
            class Proj2 = RAH_NAMESPACE::identity,
            std::enable_if_t<forward_range<R1> && forward_range<R2>>* = nullptr>
        constexpr RAH_NAMESPACE::borrowed_subrange_t<R1>
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

    constexpr find_end_fn find_end{};

    template <class I, class O>
    using set_difference_result = in_out_result<I, O>;

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
        std::enable_if_t<
            input_iterator<InputIterator1> && sentinel_for<Sentinel1, InputIterator1>
            && input_iterator<InputIterator2> && sentinel_for<Sentinel2, InputIterator2>>* = nullptr>
    set_difference_result<InputIterator1, OutputIterator> set_difference(
        InputIterator1 first1,
        Sentinel1 last1,
        InputIterator2 first2,
        Sentinel2 last2,
        OutputIterator result)
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

        return RAH_NAMESPACE::copy(first1, last1, result);
    }

    template <typename InputRange1, typename InputRange2, typename OutputIterator>
    set_difference_result<iterator_t<InputRange1>, OutputIterator>
    set_difference(InputRange1&& range1, InputRange2&& range2, OutputIterator result)
    {
        return RAH_NAMESPACE::set_difference(
            RAH_NAMESPACE::begin(range1),
            RAH_NAMESPACE::end(range1),
            RAH_NAMESPACE::begin(range2),
            RAH_NAMESPACE::end(range2),
            RAH_STD::move(result));
    }

    template <
        typename InputIterator1,
        typename Sentinel1,
        typename InputIterator2,
        typename Sentinel2,
        typename OutputIterator,
        typename Compare>
    OutputIterator set_difference(
        InputIterator1 first1,
        Sentinel1 last1,
        InputIterator2 first2,
        Sentinel2 last2,
        OutputIterator result,
        Compare compare)
    {
        while ((first1 != last1) && (first2 != last2))
        {
            if (compare(*first1, *first2))
            {
                EASTL_VALIDATE_COMPARE(
                    !compare(*first2, *first1)); // Validate that the compare function is sane.
                *result = *first1;
                ++first1;
                ++result;
            }
            else if (compare(*first2, *first1))
            {
                EASTL_VALIDATE_COMPARE(
                    !compare(*first1, *first2)); // Validate that the compare function is sane.
                ++first2;
            }
            else
            {
                ++first1;
                ++first2;
            }
        }

        return RAH_NAMESPACE::copy(first1, last1, result);
    }

    /// set_difference_2
    ///
    /// set_difference_2 iterates over both input ranges and copies elements present
    /// in the first range but not the second to the first output range and copies
    /// elements present in the second range but not in the first to the second output
    /// range.
    ///
    /// Effects: Copies the elements of the range [first1, last1) which are not
    /// present in the range [first2, last2) to the first output range beginning at
    /// result1 AND copies the element of range [first2, last2) which are not present
    /// in the range [first1, last) to the second output range beginning at result2.
    /// The elements in the constructed range are sorted.
    ///
    /// Requires: The input ranges must be sorted.
    /// Requires: The output ranges shall not overlap with either of the original ranges.
    ///
    /// Returns:  Nothing.
    ///
    /// Complexity: At most (2 * ((last1 - first1) + (last2 - first2)) - 1) comparisons.
    ///
    template <
        typename InputIterator1,
        typename Sentinel1,
        typename InputIterator2,
        typename Sentinel2,
        typename OutputIterator,
        typename Compare>
    void set_difference_2(
        InputIterator1 first1,
        Sentinel1 last1,
        InputIterator2 first2,
        Sentinel2 last2,
        OutputIterator result1,
        OutputIterator result2,
        Compare compare)
    {
        while ((first1 != last1) && (first2 != last2))
        {
            if (compare(*first1, *first2))
            {
                EASTL_VALIDATE_COMPARE(
                    !compare(*first2, *first1)); // Validate that the compare function is sane.
                *result1++ = *first1++;
            }
            else if (compare(*first2, *first1))
            {
                EASTL_VALIDATE_COMPARE(
                    !compare(*first1, *first2)); // Validate that the compare function is sane.
                *result2++ = *first2++;
            }
            else
            {
                ++first1;
                ++first2;
            }
        }

        RAH_NAMESPACE::copy(first2, last2, result2);
        RAH_NAMESPACE::copy(first1, last1, result1);
    }

    /// set_difference_2
    ///
    ///  set_difference_2 with the default comparison object is RAH_STD::less<>.
    ///
    template <typename InputIterator1, typename Sentinel1, typename InputIterator2, typename Sentinel2, typename OutputIterator>
    void set_difference_2(
        InputIterator1 first1,
        Sentinel1 last1,
        InputIterator2 first2,
        Sentinel2 last2,
        OutputIterator result1,
        OutputIterator result2)
    {
        RAH_NAMESPACE::set_difference_2(
            first1, last1, first2, last2, result1, result2, RAH_STD::less<>{});
    }

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
    OutputIterator set_symmetric_difference(
        InputIterator1 first1,
        Sentinel1 last1,
        InputIterator2 first2,
        Sentinel2 last2,
        OutputIterator result)
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

        return RAH_NAMESPACE::copy(first2, last2, RAH_NAMESPACE::copy(first1, last1, result));
    }

    template <
        typename InputIterator1,
        typename Sentinel1,
        typename InputIterator2,
        typename Sentinel2,
        typename OutputIterator,
        typename Compare>
    OutputIterator set_symmetric_difference(
        InputIterator1 first1,
        Sentinel1 last1,
        InputIterator2 first2,
        Sentinel2 last2,
        OutputIterator result,
        Compare compare)
    {
        while ((first1 != last1) && (first2 != last2))
        {
            if (compare(*first1, *first2))
            {
                EASTL_VALIDATE_COMPARE(
                    !compare(*first2, *first1)); // Validate that the compare function is sane.
                *result = *first1;
                ++first1;
                ++result;
            }
            else if (compare(*first2, *first1))
            {
                EASTL_VALIDATE_COMPARE(
                    !compare(*first1, *first2)); // Validate that the compare function is sane.
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

        return RAH_NAMESPACE::copy(first2, last2, RAH_NAMESPACE::copy(first1, last1, result));
    }

    template <class I1, class I2, class O>
    using set_intersection_result = in_in_out_result<I1, I2, O>;

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
    set_intersection_result<InputIterator1, InputIterator2, OutputIterator> set_intersection(
        InputIterator1 first1,
        Sentinel1 last1,
        InputIterator2 first2,
        Sentinel2 last2,
        OutputIterator result)
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
    set_intersection(InputRange1&& range1, InputRange2&& range2, OutputIterator result)
    {
        return RAH_NAMESPACE::set_intersection(
            RAH_NAMESPACE::begin(range1),
            RAH_NAMESPACE::end(range1),
            RAH_NAMESPACE::begin(range2),
            RAH_NAMESPACE::end(range2),
            RAH_STD::move(result));
    }

    template <
        typename InputIterator1,
        typename Sentinel1,
        typename InputIterator2,
        typename Sentinel2,
        typename OutputIterator,
        typename Compare>
    OutputIterator set_intersection(
        InputIterator1 first1,
        Sentinel1 last1,
        InputIterator2 first2,
        Sentinel2 last2,
        OutputIterator result,
        Compare compare)
    {
        while ((first1 != last1) && (first2 != last2))
        {
            if (compare(*first1, *first2))
            {
                EASTL_VALIDATE_COMPARE(
                    !compare(*first2, *first1)); // Validate that the compare function is sane.
                ++first1;
            }
            else if (compare(*first2, *first1))
            {
                EASTL_VALIDATE_COMPARE(
                    !compare(*first1, *first2)); // Validate that the compare function is sane.
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
    OutputIterator set_union(
        InputIterator1 first1,
        Sentinel1 last1,
        InputIterator2 first2,
        Sentinel2 last2,
        OutputIterator result)
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

        return RAH_NAMESPACE::copy(first2, last2, RAH_NAMESPACE::copy(first1, last1, result));
    }

    template <
        typename InputIterator1,
        typename Sentinel1,
        typename InputIterator2,
        typename Sentinel2,
        typename OutputIterator,
        typename Compare>
    OutputIterator set_union(
        InputIterator1 first1,
        Sentinel1 last1,
        InputIterator2 first2,
        Sentinel2 last2,
        OutputIterator result,
        Compare compare)
    {
        while ((first1 != last1) && (first2 != last2))
        {
            if (compare(*first1, *first2))
            {
                EASTL_VALIDATE_COMPARE(
                    !compare(*first2, *first1)); // Validate that the compare function is sane.
                *result = *first1;
                ++first1;
            }
            else if (compare(*first2, *first1))
            {
                EASTL_VALIDATE_COMPARE(
                    !compare(*first1, *first2)); // Validate that the compare function is sane.
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

        return RAH_NAMESPACE::copy(first2, last2, RAH_NAMESPACE::copy(first1, last1, result));
    }

    /// set_decomposition
    ///
    /// set_decomposition iterates over both ranges and copies elements to one of the three
    /// categories of output ranges.
    ///
    /// Effects: Constructs three sorted containers of the elements from the two ranges.
    ///             * OutputIterator1 is elements only in Container1.
    ///             * OutputIterator2 is elements only in Container2.
    ///             * OutputIterator3 is elements that are in both Container1 and Container2.
    ///
    /// Requires: The input ranges must be sorted.
    /// Requires: The resulting ranges shall not overlap with either of the original ranges.
    ///
    /// Returns: The end of the constructed range of elements in both Container1 and Container2.
    ///
    /// Complexity: At most (2 * ((last1 - first1) + (last2 - first2)) - 1) comparisons.
    ///
    template <
        typename InputIterator1,
        typename Sentinel1,
        typename InputIterator2,
        typename Sentinel2,
        typename OutputIterator1,
        typename OutputIterator2,
        typename OutputIterator3,
        typename Compare>
    OutputIterator3 set_decomposition(
        InputIterator1 first1,
        Sentinel1 last1,
        InputIterator2 first2,
        Sentinel2 last2,
        OutputIterator1 result1,
        OutputIterator2 result2,
        OutputIterator3 result3,
        Compare compare)
    {
        while ((first1 != last1) && (first2 != last2))
        {
            if (compare(*first1, *first2))
            {
                EASTL_VALIDATE_COMPARE(
                    !compare(*first2, *first1)); // Validate that the compare function is sane.
                *result1++ = *first1++;
            }
            else if (compare(*first2, *first1))
            {
                EASTL_VALIDATE_COMPARE(
                    !compare(*first1, *first2)); // Validate that the compare function is sane.
                *result2++ = *first2++;
            }
            else
            {
                *result3++ = *first1++;
                ++first2;
            }
        }

        RAH_NAMESPACE::copy(first1, last1, result1);
        RAH_NAMESPACE::copy(first2, last2, result2);

        return result3;
    }

    /// set_decomposition
    ///
    ///  set_decomposition with the default comparison object is RAH_STD::less<>.
    ///
    template <
        typename InputIterator1,
        typename Sentinel1,
        typename InputIterator2,
        typename Sentinel2,
        typename OutputIterator1,
        typename OutputIterator2,
        typename OutputIterator3>
    OutputIterator3 set_decomposition(
        InputIterator1 first1,
        Sentinel1 last1,
        InputIterator2 first2,
        Sentinel2 last2,
        OutputIterator1 result1,
        OutputIterator2 result2,
        OutputIterator3 result3)
    {
        return RAH_NAMESPACE::set_decomposition(
            first1, last1, first2, last2, result1, result2, result3, RAH_STD::less<>{});
    }

    /// is_permutation
    ///
    struct is_permutation_fn
    {
        template <
            typename I1,
            typename S1,
            typename I2,
            typename S2,
            class Proj1 = RAH_NAMESPACE::identity,
            class Proj2 = RAH_NAMESPACE::identity,
            typename Pred = RAH_NAMESPACE::equal_to,
            std::enable_if_t<
                forward_iterator<I1> && sentinel_for<S1, I1> && forward_iterator<I2>
                && sentinel_for<S2, I2>>* = nullptr>
        constexpr bool operator()(
            I1 first1, S1 last1, I2 first2, S2 last2, Pred pred = {}, Proj1 proj1 = {}, Proj2 proj2 = {}) const
        {
            // skip common prefix
            auto ret = RAH_NAMESPACE::mismatch(
                first1, last1, first2, last2, std::ref(pred), std::ref(proj1), std::ref(proj2));
            first1 = ret.in1, first2 = ret.in2;

            // iterate over the rest, counting how many times each element
            // from [first1, last1) appears in [first2, last2)
            for (auto i{first1}; i != last1; ++i)
            {
                const auto i_proj{proj1(*i)};
                auto i_cmp = [&](auto&& t)
                {
                    return pred(i_proj, std::forward<decltype(t)>(t));
                };

                if (i != RAH_NAMESPACE::find_if(first1, i, i_cmp, proj1))
                    continue; // this *i has been checked

                const auto m{RAH_NAMESPACE::count_if(first2, last2, i_cmp, proj2)};
                if (m == 0 or m != RAH_NAMESPACE::count_if(i, last1, i_cmp, proj1))
                    return false;
            }
            return true;
        }

        template <
            typename R1,
            typename R2,
            class Proj1 = RAH_NAMESPACE::identity,
            class Proj2 = RAH_NAMESPACE::identity,
            typename Pred = RAH_NAMESPACE::equal_to,
            std::enable_if_t<forward_range<R1> && forward_range<R2>>* = nullptr>
        constexpr bool
        operator()(R1&& r1, R2&& r2, Pred pred = {}, Proj1 proj1 = {}, Proj2 proj2 = {}) const
        {
            return (*this)(
                RAH_NAMESPACE::begin(r1),
                RAH_NAMESPACE::end(r1),
                RAH_NAMESPACE::begin(r2),
                RAH_NAMESPACE::end(r2),
                std::move(pred),
                std::move(proj1),
                std::move(proj2));
        }
    };

    constexpr is_permutation_fn is_permutation{};

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

    template <typename BidirectionalIterator, typename Sentinel, typename Compare>
    bool next_permutation(BidirectionalIterator first, Sentinel last, Compare compare)
    {
        if (first != last) // If there is anything in the range...
        {
            BidirectionalIterator i = last;

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
                        RAH_STD::iter_swap(i, j); // Swap the first and the final.
                        RAH_NAMESPACE::reverse(ii, last); // Reverse the ranget from second to last.
                        return true;
                    }

                    if (i == first) // There are no two consecutive values where the first is less than the second, meaning the range is in reverse order. The reverse ordered range is always the last permutation.
                    {
                        RAH_NAMESPACE::reverse(first, last);
                        break; // We are done.
                    }
                }
            }
        }

        return false;
    }

    template <typename BidirectionalIterator, typename Sentinel>
    bool next_permutation(BidirectionalIterator first, Sentinel last)
    {
        typedef typename RAH_STD::iterator_traits<BidirectionalIterator>::value_type value_type;

        return RAH_NAMESPACE::next_permutation(first, last, RAH_STD::less<value_type>());
    }

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
    namespace Internal
    {
        template <typename ForwardIterator, typename ForwardSentinel>
        subrange<ForwardIterator>
        rotate_general_impl(ForwardIterator first, ForwardIterator middle, ForwardSentinel last)
        {
            using RAH_STD::swap;

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
            typedef typename RAH_STD::iterator_traits<ForwardIterator>::value_type value_type;

            value_type temp(RAH_STD::move(*first));
            ForwardIterator result = RAH_STD::move(
                RAH_STD::next(first),
                last,
                first); // Note that while our template type is BidirectionalIterator, if the actual
            *result = RAH_STD::move(
                temp); // iterator is a RandomAccessIterator then this move will be a memmove for trivial types.

            auto back = result;
            ++back;
            return {result, back}; // result points to the final element in the range.
        }

        template <typename BidirectionalIterator, typename Sentinel>
        subrange<BidirectionalIterator>
        move_rotate_right_by_one(BidirectionalIterator first, Sentinel last)
        {
            typedef typename RAH_STD::iterator_traits<BidirectionalIterator>::value_type value_type;

            auto last2 = RAH_NAMESPACE::next(first, last);
            BidirectionalIterator beforeLast = RAH_STD::prev(last2);
            value_type temp(RAH_STD::move(*beforeLast));
            // Note that while our template type is BidirectionalIterator, if the actual
            BidirectionalIterator result = RAH_NAMESPACE::move_backward(first, beforeLast, last).out;
            // iterator is a RandomAccessIterator then this move will be a memmove for trivial types.
            *first = RAH_STD::move(temp);

            // result points to the first element in the range.
            return {RAH_STD::move(result), RAH_STD::move(last2)};
        }

        template <typename /*IteratorCategory*/, bool /*is_trivially_move_assignable*/>
        struct rotate_helper
        {
            template <typename ForwardIterator, typename ForwardSentinel>
            static subrange<ForwardIterator>
            rotate_impl(ForwardIterator first, ForwardIterator middle, ForwardSentinel last)
            {
                return Internal::rotate_general_impl(first, middle, last);
            }
        };

        template <>
        struct rotate_helper<RAH_ITC_NS::forward_iterator_tag, true>
        {
            template <typename ForwardIterator, typename ForwardSentinel>
            static subrange<ForwardIterator>
            rotate_impl(ForwardIterator first, ForwardIterator middle, ForwardSentinel last)
            {
                if (RAH_STD::next(first)
                    == middle) // If moving trivial types by a single element, memcpy is fast for that case.
                    return Internal::move_rotate_left_by_one(first, last);
                return Internal::rotate_general_impl(first, middle, last);
            }
        };

        template <>
        struct rotate_helper<RAH_ITC_NS::bidirectional_iterator_tag, false>
        {
            template <typename BidirectionalIterator, typename Sentinel>
            static subrange<BidirectionalIterator>
            rotate_impl(BidirectionalIterator first, BidirectionalIterator middle, Sentinel last)
            {
                return Internal::rotate_general_impl(first, middle, last);
            } // rotate_general_impl outperforms the flipping hands algorithm.
        };

        template <>
        struct rotate_helper<RAH_ITC_NS::bidirectional_iterator_tag, true>
        {
            template <typename BidirectionalIterator, typename Sentinel>
            static subrange<BidirectionalIterator>
            rotate_impl(BidirectionalIterator first, BidirectionalIterator middle, Sentinel last)
            {
                if (RAH_STD::next(first)
                    == middle) // If moving trivial types by a single element, memcpy is fast for that case.
                    return Internal::move_rotate_left_by_one(first, last);
                if (RAH_STD::next(middle) == last)
                    return Internal::move_rotate_right_by_one(first, last);
                return Internal::rotate_general_impl(first, middle, last);
            }
        };

        template <typename Integer>
        inline Integer greatest_common_divisor(Integer x, Integer y)
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
        struct rotate_helper<RAH_ITC_NS::random_access_iterator_tag, false>
        {
            // This is the juggling algorithm, using move operations.
            // In practice this implementation is about 25% faster than rotate_general_impl. We may want to
            // consider sticking with just rotate_general_impl and avoid the code generation of this function.
            template <typename RandomAccessIterator, typename Sentinel>
            static subrange<RandomAccessIterator>
            rotate_impl(RandomAccessIterator first, RandomAccessIterator middle, Sentinel last)
            {
                typedef
                    typename RAH_STD::iterator_traits<RandomAccessIterator>::difference_type difference_type;
                typedef
                    typename RAH_STD::iterator_traits<RandomAccessIterator>::value_type value_type;

                const difference_type m1 = (middle - first);
                const difference_type m2 = (last - middle);
                const difference_type g = Internal::greatest_common_divisor(m1, m2);
                value_type temp;

                for (RandomAccessIterator p = first + g; p != first;)
                {
                    temp = RAH_STD::move(*--p);
                    RandomAccessIterator p1 = p;
                    RandomAccessIterator p2 = p + m1;
                    do
                    {
                        *p1 = RAH_STD::move(*p2);
                        p1 = p2;
                        const difference_type d = (last - p2);

                        if (m1 < d)
                            p2 += m1;
                        else
                            p2 = first + (m1 - d);
                    } while (p2 != p);

                    *p1 = RAH_STD::move(temp);
                }

                return {first + m2, last};
            }
        };

        template <>
        struct rotate_helper<RAH_ITC_NS::random_access_iterator_tag, true>
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
                if (RAH_STD::next(first)
                    == middle) // If moving trivial types by a single element, memcpy is fast for that case.
                    return Internal::move_rotate_left_by_one(first, last);
                if (RAH_STD::next(middle) == last)
                    return Internal::move_rotate_right_by_one(first, last);
                if ((last - first) < 32) // For small ranges rotate_general_impl is faster.
                    return Internal::rotate_general_impl(first, middle, last);
                return Internal::rotate_helper<RAH_ITC_NS::random_access_iterator_tag, false>::rotate_impl(
                    first, middle, last);
            }
        };

    } // namespace Internal

    template <typename ForwardIterator, typename ForwardSentinel>
    subrange<ForwardIterator> rotate(ForwardIterator first, ForwardIterator middle, ForwardSentinel last)
    {
        if (middle != first)
        {
            if (middle != last)
            {
                typedef typename RAH_STD::iterator_traits<ForwardIterator>::iterator_category IC;
                typedef typename RAH_STD::iterator_traits<ForwardIterator>::value_type value_type;

                return Internal::rotate_helper < IC,
                       RAH_STD::is_trivially_move_assignable<value_type>::value
#if not RAH_CPP20
                           || // This is the best way of telling if we can move types via memmove, but without a conforming C++11 compiler it usually returns false.
                           RAH_STD::is_pod<value_type>::value
#endif
                           || // This is a more conservative way of telling if we can move types via memmove, and most compilers support it, but it doesn't have as full of coverage as is_trivially_move_assignable.
                           RAH_NAMESPACE::is_scalar<value_type>::value
                               > // This is the most conservative means and works with all compilers, but works only for scalars.
                               ::rotate_impl(first, middle, last);
            }

            return {std::move(first), std::move(middle)};
        }
        auto last_it = RAH_NAMESPACE::next(first, last);
        return {last_it, last_it};
    }

    template <typename ForwardRange>
    iterator_t<ForwardRange> rotate(ForwardRange&& range, iterator_t<ForwardRange> middle)
    {
        return rotate(RAH_NAMESPACE::begin(range), middle, RAH_NAMESPACE::end(range));
    }

    template <class I, class O>
    using rotate_copy_result = in_out_result<I, O>;

    struct rotate_copy_fn
    {
        template <
            typename I,
            typename S,
            typename O,
            std::enable_if_t<
                RAH_NAMESPACE::forward_iterator<I> && RAH_NAMESPACE::sentinel_for<S, I>
                && RAH_NAMESPACE::weakly_incrementable<O> && RAH_NAMESPACE::indirectly_copyable<I, O>>* = nullptr>
        constexpr RAH_NAMESPACE::rotate_copy_result<I, O>
        operator()(I first, I middle, S last, O result) const
        {
            auto c1{RAH_NAMESPACE::copy(middle, std::move(last), std::move(result))};
            auto c2{RAH_NAMESPACE::copy(std::move(first), std::move(middle), std::move(c1.out))};
            return {std::move(c1.in), std::move(c2.out)};
        }

        template <
            typename R,
            typename O,
            std::enable_if_t<
                RAH_NAMESPACE::forward_range<R> && RAH_NAMESPACE::weakly_incrementable<O>
                && RAH_NAMESPACE::indirectly_copyable<RAH_NAMESPACE::iterator_t<R>, O>>* = nullptr>
        constexpr RAH_NAMESPACE::rotate_copy_result<RAH_NAMESPACE::borrowed_iterator_t<R>, O>
        operator()(R&& r, RAH_NAMESPACE::iterator_t<R> middle, O result) const
        {
            return (*this)(
                RAH_NAMESPACE::begin(r), std::move(middle), RAH_NAMESPACE::end(r), std::move(result));
        }
    };

    /// rotate_copy
    ///
    /// Similar to rotate except writes the output to the OutputIterator and
    /// returns an OutputIterator to the element past the last element copied
    /// (i.e. result + (last - first))
    ///
    constexpr rotate_copy_fn rotate_copy{};

    /// clamp
    ///
    /// Returns a reference to a clamped value within the range of [lo, hi].
    ///
    /// http://en.cppreference.com/w/cpp/algorithm/clamp
    ///
    struct clamp_fn
    {
        template <class T, class Proj = RAH_NAMESPACE::identity, typename Comp = RAH_STD::less<>>
        constexpr const T&
        operator()(const T& v, const T& lo, const T& hi, Comp comp = {}, Proj proj = {}) const
        {
            RAH_ASSERT(!comp(hi, lo));
            auto&& pv = proj(v);

            return comp(std::forward<decltype(pv)>(pv), proj(lo)) ? lo :
                   comp(proj(hi), std::forward<decltype(pv)>(pv)) ? hi :
                                                                    v;
        }
    };

    constexpr clamp_fn clamp;

    struct fill_fn
    {
        // TODO use the eastl version when possible

        template <class T, class O, class S, std::enable_if_t<output_iterator<O, T> && sentinel_for<S, O>>* = nullptr>
        constexpr O operator()(O first, S last, const T& value) const
        {
            while (first != last)
                *first++ = value;

            return first;
        }

        template <class T, class R, std::enable_if_t<output_range<R, T>>* = nullptr>
        constexpr RAH_NAMESPACE::iterator_t<R> operator()(R&& r, const T& value) const
        {
            return (*this)(RAH_NAMESPACE::begin(r), RAH_NAMESPACE::end(r), value);
        }
    };

    constexpr fill_fn fill;

    struct fill_n_fn
    {
        template <typename T, typename O, std::enable_if_t<output_iterator<O, const T&>>* = nullptr>
        constexpr O operator()(O first, RAH_NAMESPACE::iter_difference_t<O> n, const T& value) const
        {
            for (RAH_NAMESPACE::iter_difference_t<O> i{}; i != n; ++first, ++i)
                *first = value;
            return first;
        }
    };

    constexpr fill_n_fn fill_n{};

} // namespace RAH_NAMESPACE
