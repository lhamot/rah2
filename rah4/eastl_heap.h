///////////////////////////////////////////////////////////////////////////////
// Copyright (c) Electronic Arts Inc. All rights reserved.
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
// This file implements heap functionality much like the std C++ heap algorithms.
// Such heaps are not the same thing as memory heaps or pools, but rather are
// semi-sorted random access containers which have the primary purpose of
// supporting the implementation of priority_queue and similar data structures.
//
// The primary distinctions between this heap functionality and std::heap are:
//    - This heap exposes some extra functionality such as is_heap and change_heap.
//    - This heap is more efficient than versions found in typical STL
//      implementations such as STLPort, Microsoft, and Metrowerks. This comes
//      about due to better use of array dereferencing and branch prediction.
//      You should expect of 5-30%, depending on the usage and platform.
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
// The publicly usable functions we define are:
//    push_heap     -- Adds an entry to a heap.                             Same as C++ std::push_heap.
//    pop_heap      -- Removes the top entry from a heap.                   Same as C++ std::pop_heap.
//    make_heap     -- Converts an array to a heap.                         Same as C++ std::make_heap.
//    sort_heap     -- Sorts a heap in place.                               Same as C++ std::sort_heap.
//    remove_heap   -- Removes an arbitrary entry from a heap.
//    change_heap   -- Changes the priority of an entry in the heap.
//    is_heap       -- Returns true if an array appears is in heap format.   Same as C++11 std::is_heap.
//    is_heap_until -- Returns largest part of the range which is a heap.    Same as C++11 std::is_heap_until.
///////////////////////////////////////////////////////////////////////////////

#pragma once

//#include <EASTL/internal/config.h>
//#include <EASTL/iterator.h>
//#include <stddef.h>

#include "range_bases.hpp"

#if defined(EA_PRAGMA_ONCE_SUPPORTED)
#pragma once // Some compilers (e.g. VC++) benefit significantly from using this. We've measured 3-4% build speed improvements in apps as a result.
#endif

namespace RAH_NAMESPACE
{

    ///////////////////////////////////////////////////////////////////////
    // promote_heap (internal function)
    ///////////////////////////////////////////////////////////////////////

    template <typename RandomAccessIterator, typename Distance, typename T, typename ValueType>
    inline void
    promote_heap_impl(RandomAccessIterator first, Distance topPosition, Distance position, T value)
    {
        for (Distance parentPosition =
                 (position - 1)
                 >> 1; // This formula assumes that (position > 0). // We use '>> 1' instead of '/ 2' because we have seen VC++ generate better code with >>.
             (position > topPosition) && RAH_STD::less<ValueType>()(*(first + parentPosition), value);
             parentPosition = (position - 1) >> 1)
        {
            *(first + position) = RAH_STD::forward<ValueType>(
                *(first + parentPosition)); // Swap the node with its parent.
            position = parentPosition;
        }

        *(first + position) = RAH_STD::forward<ValueType>(value);
    }

    /// promote_heap
    ///
    /// Moves a value in the heap from a given position upward until
    /// it is sorted correctly. It's kind of like bubble-sort, except that
    /// instead of moving linearly from the back of a list to the front,
    /// it moves from the bottom of the tree up the branches towards the
    /// top. But otherwise is just like bubble-sort.
    ///
    /// This function requires that the value argument refer to a value
    /// that is currently not within the heap.
    ///
    template <typename RandomAccessIterator, typename Distance, typename T>
    inline void
    promote_heap(RandomAccessIterator first, Distance topPosition, Distance position, const T& value)
    {
        typedef typename RAH_STD::iterator_traits<RandomAccessIterator>::value_type value_type;
        promote_heap_impl<RandomAccessIterator, Distance, const T&, const value_type>(
            first, topPosition, position, value);
    }

    /// promote_heap
    ///
    /// Moves a value in the heap from a given position upward until
    /// it is sorted correctly. It's kind of like bubble-sort, except that
    /// instead of moving linearly from the back of a list to the front,
    /// it moves from the bottom of the tree up the branches towards the
    /// top. But otherwise is just like bubble-sort.
    ///
    /// This function requires that the value argument refer to a value
    /// that is currently not within the heap.
    ///
    template <typename RandomAccessIterator, typename Distance, typename T>
    inline void
    promote_heap(RandomAccessIterator first, Distance topPosition, Distance position, T&& value)
    {
        typedef typename RAH_STD::iterator_traits<RandomAccessIterator>::value_type value_type;
        promote_heap_impl<RandomAccessIterator, Distance, T&&, value_type>(
            first, topPosition, position, RAH_STD::forward<T>(value));
    }

    template <typename RandomAccessIterator, typename Distance, typename T, typename Compare, typename ValueType>
    inline void promote_heap_impl(
        RandomAccessIterator first, Distance topPosition, Distance position, T value, Compare compare)
    {
        for (Distance parentPosition =
                 (position - 1)
                 >> 1; // This formula assumes that (position > 0). // We use '>> 1' instead of '/ 2' because we have seen VC++ generate better code with >>.
             (position > topPosition) && compare(*(first + parentPosition), value);
             parentPosition = (position - 1) >> 1)
        {
            *(first + position) = RAH_STD::forward<ValueType>(
                *(first + parentPosition)); // Swap the node with its parent.
            position = parentPosition;
        }

        *(first + position) = RAH_STD::forward<ValueType>(value);
    }

    /// promote_heap
    ///
    /// Takes a Compare(a, b) function (or function object) which returns true if a < b.
    /// For example, you could use the standard 'less' comparison object.
    ///
    /// The Compare function must work equivalently to the compare function used
    /// to make and maintain the heap.
    ///
    /// This function requires that the value argument refer to a value
    /// that is currently not within the heap.
    ///
    template <typename RandomAccessIterator, typename Distance, typename T, typename Compare>
    inline void promote_heap(
        RandomAccessIterator first, Distance topPosition, Distance position, const T& value, Compare compare)
    {
        typedef typename RAH_STD::iterator_traits<RandomAccessIterator>::value_type value_type;
        promote_heap_impl<RandomAccessIterator, Distance, const T&, Compare, const value_type>(
            first, topPosition, position, value, compare);
    }

    /// promote_heap
    ///
    /// Takes a Compare(a, b) function (or function object) which returns true if a < b.
    /// For example, you could use the standard 'less' comparison object.
    ///
    /// The Compare function must work equivalently to the compare function used
    /// to make and maintain the heap.
    ///
    /// This function requires that the value argument refer to a value
    /// that is currently not within the heap.
    ///
    template <typename RandomAccessIterator, typename Distance, typename T, typename Compare>
    inline void promote_heap(
        RandomAccessIterator first, Distance topPosition, Distance position, T&& value, Compare compare)
    {
        typedef typename RAH_STD::iterator_traits<RandomAccessIterator>::value_type value_type;
        promote_heap_impl<RandomAccessIterator, Distance, T&&, Compare, value_type>(
            first, topPosition, position, RAH_STD::forward<T>(value), compare);
    }

    ///////////////////////////////////////////////////////////////////////
    // adjust_heap (internal function)
    ///////////////////////////////////////////////////////////////////////

    template <typename RandomAccessIterator, typename Distance, typename T, typename ValueType>
    void adjust_heap_impl(
        RandomAccessIterator first, Distance topPosition, Distance heapSize, Distance position, T value)
    {
        // We do the conventional approach of moving the position down to the
        // bottom then inserting the value at the back and moving it up.
        Distance childPosition = (2 * position) + 2;

        for (; childPosition < heapSize; childPosition = (2 * childPosition) + 2)
        {
            if (RAH_STD::less<ValueType>()(
                    *(first + childPosition),
                    *(first + (childPosition - 1)))) // Choose the larger of the two children.
                --childPosition;
            *(first + position) = RAH_STD::forward<ValueType>(
                *(first + childPosition)); // Swap positions with this child.
            position = childPosition;
        }

        if (childPosition == heapSize) // If we are at the very last index of the bottom...
        {
            *(first + position) = RAH_STD::forward<ValueType>(*(first + (childPosition - 1)));
            position = childPosition - 1;
        }

        RAH_NAMESPACE::promote_heap<RandomAccessIterator, Distance, T>(
            first, topPosition, position, RAH_STD::forward<ValueType>(value));
    }

    /// adjust_heap
    ///
    /// Given a position that has just been vacated, this function moves
    /// new values into that vacated position appropriately. The value
    /// argument is an entry which will be inserted into the heap after
    /// we move nodes into the positions that were vacated.
    ///
    /// This function requires that the value argument refer to a value
    /// that is currently not within the heap.
    ///
    template <typename RandomAccessIterator, typename Distance, typename T>
    void adjust_heap(
        RandomAccessIterator first,
        Distance topPosition,
        Distance heapSize,
        Distance position,
        const T& value)
    {
        typedef typename RAH_STD::iterator_traits<RandomAccessIterator>::value_type value_type;
        adjust_heap_impl<RandomAccessIterator, Distance, const T&, const value_type>(
            first, topPosition, heapSize, position, RAH_STD::forward<const T&>(value));
    }

    /// adjust_heap
    ///
    /// Given a position that has just been vacated, this function moves
    /// new values into that vacated position appropriately. The value
    /// argument is an entry which will be inserted into the heap after
    /// we move nodes into the positions that were vacated.
    ///
    /// This function requires that the value argument refer to a value
    /// that is currently not within the heap.
    ///
    template <typename RandomAccessIterator, typename Distance, typename T>
    void adjust_heap(
        RandomAccessIterator first, Distance topPosition, Distance heapSize, Distance position, T&& value)
    {
        typedef typename RAH_STD::iterator_traits<RandomAccessIterator>::value_type value_type;
        adjust_heap_impl<RandomAccessIterator, Distance, T&&, value_type>(
            first, topPosition, heapSize, position, RAH_STD::forward<T>(value));
    }

    template <typename RandomAccessIterator, typename Distance, typename T, typename Compare, typename ValueType>
    void adjust_heap_impl(
        RandomAccessIterator first,
        Distance topPosition,
        Distance heapSize,
        Distance position,
        T value,
        Compare compare)
    {
        // We do the conventional approach of moving the position down to the
        // bottom then inserting the value at the back and moving it up.
        Distance childPosition = (2 * position) + 2;

        for (; childPosition < heapSize; childPosition = (2 * childPosition) + 2)
        {
            if (compare(
                    *(first + childPosition),
                    *(first + (childPosition - 1)))) // Choose the larger of the two children.
                --childPosition;
            *(first + position) = RAH_STD::forward<ValueType>(
                *(first + childPosition)); // Swap positions with this child.
            position = childPosition;
        }

        if (childPosition == heapSize) // If we are at the bottom...
        {
            *(first + position) = RAH_STD::forward<ValueType>(*(first + (childPosition - 1)));
            position = childPosition - 1;
        }

        RAH_NAMESPACE::promote_heap<RandomAccessIterator, Distance, T, Compare>(
            first, topPosition, position, RAH_STD::forward<ValueType>(value), compare);
    }

    /// adjust_heap
    ///
    /// The Compare function must work equivalently to the compare function used
    /// to make and maintain the heap.
    ///
    /// This function requires that the value argument refer to a value
    /// that is currently not within the heap.
    ///
    template <typename RandomAccessIterator, typename Distance, typename T, typename Compare>
    void adjust_heap(
        RandomAccessIterator first,
        Distance topPosition,
        Distance heapSize,
        Distance position,
        const T& value,
        Compare compare)
    {
        typedef typename RAH_STD::iterator_traits<RandomAccessIterator>::value_type value_type;
        adjust_heap_impl<RandomAccessIterator, Distance, const T&, Compare, const value_type>(
            first, topPosition, heapSize, position, RAH_STD::forward<const T&>(value), compare);
    }

    /// adjust_heap
    ///
    /// The Compare function must work equivalently to the compare function used
    /// to make and maintain the heap.
    ///
    /// This function requires that the value argument refer to a value
    /// that is currently not within the heap.
    ///
    template <typename RandomAccessIterator, typename Distance, typename T, typename Compare>
    void adjust_heap(
        RandomAccessIterator first,
        Distance topPosition,
        Distance heapSize,
        Distance position,
        T&& value,
        Compare compare)
    {
        typedef typename RAH_STD::iterator_traits<RandomAccessIterator>::value_type value_type;
        adjust_heap_impl<RandomAccessIterator, Distance, T&&, Compare, value_type>(
            first, topPosition, heapSize, position, RAH_STD::forward<T>(value), compare);
    }

    ///////////////////////////////////////////////////////////////////////
    // push_heap
    ///////////////////////////////////////////////////////////////////////

    /// push_heap
    ///
    /// Adds an item to a heap (which is an array). The item necessarily
    /// comes from the back of the heap (array). Thus, the insertion of a
    /// new item in a heap is a two step process: push_back and push_heap.
    ///
    /// Example usage:
    ///    vector<int> heap;
    ///
    ///    heap.push_back(3);
    ///    push_heap(heap.begin(), heap.end()); // Places '3' appropriately.
    ///
    template <
        typename RandomAccessIterator,
        typename Sentinel,
        std::enable_if_t<
            random_access_iterator<RandomAccessIterator>
            && sentinel_for<Sentinel, RandomAccessIterator>>* = nullptr>
    inline RandomAccessIterator push_heap(RandomAccessIterator first, Sentinel last)
    {
        typedef
            typename RAH_STD::iterator_traits<RandomAccessIterator>::difference_type difference_type;
        typedef typename RAH_STD::iterator_traits<RandomAccessIterator>::value_type value_type;
        auto lasti = RAH_NAMESPACE::next(first, last);
        const value_type tempBottom(RAH_STD::forward<value_type>(*(lasti - 1)));

        RAH_NAMESPACE::promote_heap<RandomAccessIterator, difference_type, value_type>(
            first,
            (difference_type)0,
            (difference_type)(lasti - first - 1),
            RAH_STD::forward<const value_type>(tempBottom));
        return lasti;
    }

    template <typename RandomAccessRange>
    inline borrowed_iterator_t<RandomAccessRange> push_heap(RandomAccessRange&& range)
    {
        return RAH_NAMESPACE::push_heap(RAH_NAMESPACE::begin(range), RAH_NAMESPACE::end(range));
    }

    /// push_heap
    ///
    /// This version is useful for cases where your object comparison is unusual
    /// or where you want to have the heap store pointers to objects instead of
    /// storing the objects themselves (often in order to improve cache coherency
    /// while doing sorting).
    ///
    /// The Compare function must work equivalently to the compare function used
    /// to make and maintain the heap.
    ///
    template <typename RandomAccessIterator, typename Sentinel, typename Compare>
    inline RandomAccessIterator push_heap(RandomAccessIterator first, Sentinel last, Compare compare)
    {
        typedef
            typename RAH_STD::iterator_traits<RandomAccessIterator>::difference_type difference_type;
        typedef typename RAH_STD::iterator_traits<RandomAccessIterator>::value_type value_type;
        auto lasti = RAH_NAMESPACE::next(first, last);
        const value_type tempBottom(*(lasti - 1));

        RAH_NAMESPACE::promote_heap<RandomAccessIterator, difference_type, value_type, Compare>(
            first, (difference_type)0, (difference_type)(lasti - first - 1), tempBottom, compare);
        return lasti;
    }

    template <
        typename RandomAccessRange,
        typename Compare,
        std::enable_if_t<random_access_range<RandomAccessRange>>* = nullptr>
    inline borrowed_iterator_t<RandomAccessRange> push_heap(RandomAccessRange&& range, Compare compare)
    {
        return RAH_NAMESPACE::push_heap(
            RAH_NAMESPACE::begin(range), RAH_NAMESPACE::end(range), RAH_STD::move(compare));
    }

    ///////////////////////////////////////////////////////////////////////
    // pop_heap
    ///////////////////////////////////////////////////////////////////////

    /// pop_heap
    ///
    /// Removes the first item from the heap (which is an array), and adjusts
    /// the heap so that the highest priority item becomes the new first item.
    ///
    /// Example usage:
    ///    vector<int> heap;
    ///
    ///    heap.push_back(2);
    ///    heap.push_back(3);
    ///    heap.push_back(1);
    ///    <use heap[0], which is the highest priority item in the heap>
    ///    pop_heap(heap.begin(), heap.end());  // Moves heap[0] to the back of the heap and adjusts the heap.
    ///    heap.pop_back();                     // Remove value that was just at the top of the heap
    ///
    template <
        typename RandomAccessIterator,
        typename Sentinel,
        std::enable_if_t<
            random_access_iterator<RandomAccessIterator>
            && sentinel_for<Sentinel, RandomAccessIterator>>* = nullptr>
    inline RandomAccessIterator pop_heap(RandomAccessIterator first, Sentinel last)
    {
        typedef
            typename RAH_STD::iterator_traits<RandomAccessIterator>::difference_type difference_type;
        typedef typename RAH_STD::iterator_traits<RandomAccessIterator>::value_type value_type;
        auto lasti = RAH_NAMESPACE::next(first, last);
        value_type tempBottom(RAH_STD::forward<value_type>(*(lasti - 1)));
        *(lasti - 1) = RAH_STD::forward<value_type>(*first);
        RAH_NAMESPACE::adjust_heap<RandomAccessIterator, difference_type, value_type>(
            first,
            (difference_type)0,
            (difference_type)(lasti - first - 1),
            0,
            RAH_STD::forward<value_type>(tempBottom));
        return lasti;
    }

    template <typename RandomAccessRange>
    inline borrowed_iterator_t<RandomAccessRange> pop_heap(RandomAccessRange&& range)
    {
        return RAH_NAMESPACE::pop_heap(RAH_NAMESPACE::begin(range), RAH_NAMESPACE::end(range));
    }

    /// pop_heap
    ///
    /// This version is useful for cases where your object comparison is unusual
    /// or where you want to have the heap store pointers to objects instead of
    /// storing the objects themselves (often in order to improve cache coherency
    /// while doing sorting).
    ///
    /// The Compare function must work equivalently to the compare function used
    /// to make and maintain the heap.
    ///
    template <typename RandomAccessIterator, typename Sentinel, typename Compare>
    inline RandomAccessIterator pop_heap(RandomAccessIterator first, Sentinel last, Compare compare)
    {
        typedef
            typename RAH_STD::iterator_traits<RandomAccessIterator>::difference_type difference_type;
        typedef typename RAH_STD::iterator_traits<RandomAccessIterator>::value_type value_type;
        auto lasti = RAH_NAMESPACE::next(first, last);
        value_type tempBottom(RAH_STD::forward<value_type>(*(lasti - 1)));
        *(lasti - 1) = RAH_STD::forward<value_type>(*first);
        RAH_NAMESPACE::adjust_heap<RandomAccessIterator, difference_type, value_type, Compare>(
            first,
            (difference_type)0,
            (difference_type)(lasti - first - 1),
            0,
            RAH_STD::forward<value_type>(tempBottom),
            compare);
        return lasti;
    }

    template <
        typename RandomAccessRange,
        typename Compare,
        std::enable_if_t<random_access_range<RandomAccessRange>>* = nullptr>
    inline borrowed_iterator_t<RandomAccessRange> pop_heap(RandomAccessRange&& range, Compare compare)
    {
        return RAH_NAMESPACE::pop_heap(
            RAH_NAMESPACE::begin(range), RAH_NAMESPACE::end(range), RAH_STD::move(compare));
    }

    ///////////////////////////////////////////////////////////////////////
    // make_heap
    ///////////////////////////////////////////////////////////////////////

    /// make_heap
    ///
    /// Given an array, this function converts it into heap format.
    /// The complexity is O(n), where n is count of the range.
    /// The input range is not required to be in any order.
    ///
    template <
        typename RandomAccessIterator,
        typename Sentinel,
        std::enable_if_t<
            random_access_iterator<RandomAccessIterator>
            && sentinel_for<Sentinel, RandomAccessIterator>>* = nullptr>
    RandomAccessIterator make_heap(RandomAccessIterator first, Sentinel last)
    {
        // We do bottom-up heap construction as per Sedgewick. Such construction is O(n).
        typedef
            typename RAH_STD::iterator_traits<RandomAccessIterator>::difference_type difference_type;
        typedef typename RAH_STD::iterator_traits<RandomAccessIterator>::value_type value_type;

        auto lasti = next(first, last);
        const difference_type heapSize = lasti - first;

        if (heapSize
            >= 2) // If there is anything to do... (we need this check because otherwise the math fails below).
        {
            difference_type parentPosition =
                ((heapSize - 2) >> 1)
                + 1; // We use '>> 1' instead of '/ 2' because we have seen VC++ generate better code with >>.

            do
            {
                --parentPosition;
                value_type temp(RAH_STD::forward<value_type>(*(first + parentPosition)));
                RAH_NAMESPACE::adjust_heap<RandomAccessIterator, difference_type, value_type>(
                    first, parentPosition, heapSize, parentPosition, RAH_STD::forward<value_type>(temp));
            } while (parentPosition != 0);
        }
        return lasti;
    }

    template <typename RandomAccessRange>
    borrowed_iterator_t<RandomAccessRange> make_heap(RandomAccessRange&& range)
    {
        return RAH_NAMESPACE::make_heap(RAH_NAMESPACE::begin(range), RAH_NAMESPACE::end(range));
    }

    template <typename RandomAccessIterator, typename Sentinel, typename Compare>
    RandomAccessIterator make_heap(RandomAccessIterator first, Sentinel last, Compare compare)
    {
        typedef
            typename RAH_STD::iterator_traits<RandomAccessIterator>::difference_type difference_type;
        typedef typename RAH_STD::iterator_traits<RandomAccessIterator>::value_type value_type;

        auto lasti = next(first, last);
        const difference_type heapSize = lasti - first;

        if (heapSize
            >= 2) // If there is anything to do... (we need this check because otherwise the math fails below).
        {
            difference_type parentPosition =
                ((heapSize - 2) >> 1)
                + 1; // We use '>> 1' instead of '/ 2' because we have seen VC++ generate better code with >>.

            do
            {
                --parentPosition;
                value_type temp(RAH_STD::forward<value_type>(*(first + parentPosition)));
                RAH_NAMESPACE::adjust_heap<RandomAccessIterator, difference_type, value_type, Compare>(
                    first,
                    parentPosition,
                    heapSize,
                    parentPosition,
                    RAH_STD::forward<value_type>(temp),
                    compare);
            } while (parentPosition != 0);
        }
        return lasti;
    }

    template <
        typename RandomAccessRange,
        typename Compare,
        std::enable_if_t<random_access_range<RandomAccessRange>>* = nullptr>
    borrowed_iterator_t<RandomAccessRange> make_heap(RandomAccessRange&& range, Compare compare)
    {
        return RAH_NAMESPACE::make_heap(
            RAH_NAMESPACE::begin(range), RAH_NAMESPACE::end(range), RAH_STD::move(compare));
    }

    ///////////////////////////////////////////////////////////////////////
    // sort_heap
    ///////////////////////////////////////////////////////////////////////

    /// sort_heap
    ///
    /// After the application if this algorithm, the range it was applied to
    /// is no longer a heap, though it will be a reverse heap (smallest first).
    /// The item with the lowest priority will be first, and the highest last.
    /// This is not a stable sort because the relative order of equivalent
    /// elements is not necessarily preserved.
    /// The range referenced must be valid; all pointers must be dereferenceable
    /// and within the sequence the last position is reachable from the first
    /// by incrementation.
    /// The complexity is at most O(n * log(n)), where n is count of the range.
    ///
    template <typename RandomAccessIterator, typename Sentinel>
    inline RandomAccessIterator sort_heap(RandomAccessIterator first, Sentinel last)
    {
        auto lasti = RAH_NAMESPACE::next(first, last);
        auto res = lasti;
        for (; (lasti - first) > 1; --lasti) // We simply use the heap to sort itself.
            RAH_NAMESPACE::pop_heap<RandomAccessIterator>(first, lasti);
        return res;
    }

    template <typename RandomAccessRange>
    inline borrowed_iterator_t<RandomAccessRange> sort_heap(RandomAccessRange&& range)
    {
        return RAH_NAMESPACE::sort_heap(RAH_NAMESPACE::begin(range), RAH_NAMESPACE::end(range));
    }

    /// sort_heap
    ///
    /// The Compare function must work equivalently to the compare function used
    /// to make and maintain the heap.
    ///
    template <typename RandomAccessIterator, typename Sentinel, typename Compare>
    inline RandomAccessIterator sort_heap(RandomAccessIterator first, Sentinel last, Compare compare)
    {
        auto lasti = RAH_NAMESPACE::next(first, last);
        auto res = lasti;
        for (; (lasti - first) > 1; --lasti) // We simply use the heap to sort itself.
            RAH_NAMESPACE::pop_heap<RandomAccessIterator, Compare>(first, lasti, compare);
        return res;
    }

    template <typename RandomAccessRange, typename Compare>
    inline borrowed_iterator_t<RandomAccessRange> sort_heap(RandomAccessRange&& range, Compare compare)
    {
        return RAH_NAMESPACE::sort_heap(
            RAH_NAMESPACE::begin(range), RAH_NAMESPACE::end(range), RAH_STD::move(compare));
    }

    ///////////////////////////////////////////////////////////////////////
    // remove_heap
    ///////////////////////////////////////////////////////////////////////

    /// remove_heap
    ///
    /// Removes an arbitrary entry from the heap and adjusts the heap appropriately.
    /// This function is unlike pop_heap in that pop_heap moves the top item
    /// to the back of the heap, whereas remove_heap moves an arbitrary item to
    /// the back of the heap.
    ///
    /// Note: Since this function moves the element to the back of the heap and
    /// doesn't actually remove it from the given container, the user must call
    /// the container erase function if the user wants to erase the element
    /// from the container.
    ///
    template <typename RandomAccessIterator, typename Distance>
    inline void remove_heap(RandomAccessIterator first, Distance heapSize, Distance position)
    {
        typedef
            typename RAH_STD::iterator_traits<RandomAccessIterator>::difference_type difference_type;
        typedef typename RAH_STD::iterator_traits<RandomAccessIterator>::value_type value_type;

        const value_type tempBottom(*(first + heapSize - 1));
        *(first + heapSize - 1) = *(first + position);
        RAH_NAMESPACE::adjust_heap<RandomAccessIterator, difference_type, value_type>(
            first,
            (difference_type)0,
            (difference_type)(heapSize - 1),
            (difference_type)position,
            tempBottom);
    }

    /// remove_heap
    ///
    /// The Compare function must work equivalently to the compare function used
    /// to make and maintain the heap.
    ///
    /// Note: Since this function moves the element to the back of the heap and
    /// doesn't actually remove it from the given container, the user must call
    /// the container erase function if the user wants to erase the element
    /// from the container.
    ///
    template <typename RandomAccessIterator, typename Distance, typename Compare>
    inline void
    remove_heap(RandomAccessIterator first, Distance heapSize, Distance position, Compare compare)
    {
        typedef
            typename RAH_STD::iterator_traits<RandomAccessIterator>::difference_type difference_type;
        typedef typename RAH_STD::iterator_traits<RandomAccessIterator>::value_type value_type;

        const value_type tempBottom(*(first + heapSize - 1));
        *(first + heapSize - 1) = *(first + position);
        RAH_NAMESPACE::adjust_heap<RandomAccessIterator, difference_type, value_type, Compare>(
            first,
            (difference_type)0,
            (difference_type)(heapSize - 1),
            (difference_type)position,
            tempBottom,
            compare);
    }

    ///////////////////////////////////////////////////////////////////////
    // change_heap
    ///////////////////////////////////////////////////////////////////////

    /// change_heap
    ///
    /// Given a value in the heap that has changed in priority, this function
    /// adjusts the heap appropriately. The heap size remains unchanged after
    /// this operation.
    ///
    template <typename RandomAccessIterator, typename Distance>
    inline void change_heap(RandomAccessIterator first, Distance heapSize, Distance position)
    {
        typedef
            typename RAH_STD::iterator_traits<RandomAccessIterator>::difference_type difference_type;
        typedef typename RAH_STD::iterator_traits<RandomAccessIterator>::value_type value_type;

        RAH_NAMESPACE::remove_heap<RandomAccessIterator, Distance>(first, heapSize, position);

        value_type tempBottom(*(first + heapSize - 1));

        RAH_NAMESPACE::promote_heap<RandomAccessIterator, difference_type, value_type>(
            first, (difference_type)0, (difference_type)(heapSize - 1), tempBottom);
    }

    /// change_heap
    ///
    /// The Compare function must work equivalently to the compare function used
    /// to make and maintain the heap.
    ///
    template <typename RandomAccessIterator, typename Distance, typename Compare>
    inline void
    change_heap(RandomAccessIterator first, Distance heapSize, Distance position, Compare compare)
    {
        typedef
            typename RAH_STD::iterator_traits<RandomAccessIterator>::difference_type difference_type;
        typedef typename RAH_STD::iterator_traits<RandomAccessIterator>::value_type value_type;

        RAH_NAMESPACE::remove_heap<RandomAccessIterator, Distance, Compare>(
            first, heapSize, position, compare);

        value_type tempBottom(*(first + heapSize - 1));

        RAH_NAMESPACE::promote_heap<RandomAccessIterator, difference_type, value_type, Compare>(
            first, (difference_type)0, (difference_type)(heapSize - 1), tempBottom, compare);
    }

    ///////////////////////////////////////////////////////////////////////
    // is_heap_until
    ///////////////////////////////////////////////////////////////////////

    /// is_heap_until
    ///
    template <typename RandomAccessIterator, typename Sentinel>
    inline RandomAccessIterator is_heap_until(RandomAccessIterator first, Sentinel last)
    {
        int counter = 0;

        for (RandomAccessIterator child = first + 1; child < last; ++child, counter ^= 1)
        {
            if (*first < *child) // We must use operator <, and are not allowed to use > or >= here.
                return child;
            first += counter; // counter switches between 0 and 1 every time through.
        }

        return last;
    }

    template <typename RandomAccessRange>
    inline iterator_t<RandomAccessRange> is_heap_until(RandomAccessRange&& range)
    {
        return RAH_NAMESPACE::is_heap_until(RAH_NAMESPACE::begin(range), RAH_NAMESPACE::end(range));
    }

    /// is_heap_until
    ///
    /// The Compare function must work equivalently to the compare function used
    /// to make and maintain the heap.
    ///
    template <typename RandomAccessIterator, typename Sentinel, typename Compare>
    inline RandomAccessIterator is_heap_until(RandomAccessIterator first, Sentinel last, Compare compare)
    {
        int counter = 0;

        for (RandomAccessIterator child = first + 1; child < last; ++child, counter ^= 1)
        {
            if (compare(*first, *child))
                return child;
            first += counter; // counter switches between 0 and 1 every time through.
        }

        return last;
    }

    template <typename RandomAccessRange, typename Compare>
    inline iterator_t<RandomAccessRange> is_heap_until(RandomAccessRange&& range, Compare compare)
    {
        return RAH_NAMESPACE::is_heap_until(
            RAH_NAMESPACE::begin(range), RAH_NAMESPACE::end(range), RAH_STD::move(compare));
    }

    ///////////////////////////////////////////////////////////////////////
    // is_heap
    ///////////////////////////////////////////////////////////////////////

    /// is_heap
    ///
    /// This is a useful debugging algorithm for verifying that a random
    /// access container is in heap format.
    ///
    template <
        typename RandomAccessIterator,
        typename Sentinel,
        std::enable_if_t<
            random_access_iterator<RandomAccessIterator>
            && sentinel_for<Sentinel, RandomAccessIterator>>* = nullptr>
    inline bool is_heap(RandomAccessIterator first, RandomAccessIterator last)
    {
        return (RAH_NAMESPACE::is_heap_until(first, last) == last);
    }

    template <typename RandomAccessRange>
    inline bool is_heap(RandomAccessRange&& range)
    {
        return is_heap(RAH_NAMESPACE::begin(range), RAH_NAMESPACE::end(range));
    }

    /// is_heap
    ///
    /// The Compare function must work equivalently to the compare function used
    /// to make and maintain the heap.
    ///
    template <typename RandomAccessIterator, typename Sentinel, typename Compare>
    inline bool is_heap(RandomAccessIterator first, Sentinel last, Compare compare)
    {
        return (RAH_NAMESPACE::is_heap_until(first, last, compare) == last);
    }

    template <
        typename RandomAccessRange,
        typename Compare,
        std::enable_if_t<random_access_range<RandomAccessRange>>* = nullptr>
    inline bool is_heap(RandomAccessRange&& range, Compare compare)
    {
        return is_heap(
            RAH_NAMESPACE::begin(range), RAH_NAMESPACE::end(range), RAH_STD::move(compare));
    }

    // To consider: The following may be a faster implementation for most cases.
    //
    // template <typename RandomAccessIterator>
    // inline bool is_heap(RandomAccessIterator first, RandomAccessIterator last)
    // {
    //     if(((uintptr_t)(last - first) & 1) == 0) // If the range has an even number of elements...
    //         --last;
    //
    //     RandomAccessIterator parent = first, child = (first + 1);
    //
    //     for(; child < last; child += 2, ++parent)
    //     {
    //         if((*parent < *child) || (*parent < *(child + 1)))
    //             return false;
    //     }
    //
    //     if((((uintptr_t)(last - first) & 1) == 0) && (*parent < *child))
    //         return false;
    //
    //     return true;
    // }

} // namespace RAH_NAMESPACE
