#pragma once

#include "range_bases.hpp"

namespace RAH2_NS
{
    namespace ranges
    {
        namespace details
        {
            ///////////////////////////////////////////////////////////////////////
            // promote_heap (internal function)
            ///////////////////////////////////////////////////////////////////////

            template <typename RandomAccessIterator, typename Distance, typename T, typename ValueType>
            void promote_heap_impl(
                RandomAccessIterator first, Distance topPosition, Distance position, T value)
            {
                for (Distance parentPosition =
                         (position - 1)
                         >> 1; // This formula assumes that (position > 0). // We use '>> 1' instead of '/ 2' because we have seen VC++ generate better code with >>.
                     (position > topPosition)
                     && RAH2_STD::less<ValueType>()(*(first + parentPosition), value);
                     parentPosition = (position - 1) >> 1)
                {
                    *(first + position) = RAH2_STD::forward<ValueType>(
                        *(first + parentPosition)); // Swap the node with its parent.
                    position = parentPosition;
                }

                *(first + position) = RAH2_STD::forward<ValueType>(value);
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
            void promote_heap(
                RandomAccessIterator first, Distance topPosition, Distance position, T const& value)
            {
                using value_type =
                    typename RAH2_STD::iterator_traits<RandomAccessIterator>::value_type;
                promote_heap_impl<RandomAccessIterator, Distance, T const&, value_type const>(
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
            void promote_heap(
                RandomAccessIterator first, Distance topPosition, Distance position, T&& value)
            {
                using value_type =
                    typename RAH2_STD::iterator_traits<RandomAccessIterator>::value_type;
                promote_heap_impl<RandomAccessIterator, Distance, T&&, value_type>(
                    first, topPosition, position, RAH2_STD::forward<T>(value));
            }

            template <typename RandomAccessIterator, typename Distance, typename T, typename Compare, typename ValueType>
            void promote_heap_impl(
                RandomAccessIterator first,
                Distance topPosition,
                Distance position,
                T value,
                Compare compare)
            {
                for (Distance parentPosition =
                         (position - 1)
                         >> 1; // This formula assumes that (position > 0). // We use '>> 1' instead of '/ 2' because we have seen VC++ generate better code with >>.
                     (position > topPosition) && compare(*(first + parentPosition), value);
                     parentPosition = (position - 1) >> 1)
                {
                    *(first + position) = RAH2_STD::forward<ValueType>(
                        *(first + parentPosition)); // Swap the node with its parent.
                    position = parentPosition;
                }

                *(first + position) = RAH2_STD::forward<ValueType>(value);
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
            void promote_heap(
                RandomAccessIterator first,
                Distance topPosition,
                Distance position,
                T const& value,
                Compare compare)
            {
                using value_type =
                    typename RAH2_STD::iterator_traits<RandomAccessIterator>::value_type;
                promote_heap_impl<RandomAccessIterator, Distance, T const&, Compare, value_type const>(
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
            void promote_heap(
                RandomAccessIterator first,
                Distance topPosition,
                Distance position,
                T&& value,
                Compare compare)
            {
                using value_type =
                    typename RAH2_STD::iterator_traits<RandomAccessIterator>::value_type;
                promote_heap_impl<RandomAccessIterator, Distance, T&&, Compare, value_type>(
                    first, topPosition, position, RAH2_STD::forward<T>(value), compare);
            }

            ///////////////////////////////////////////////////////////////////////
            // adjust_heap (internal function)
            ///////////////////////////////////////////////////////////////////////

            template <typename RandomAccessIterator, typename Distance, typename T, typename ValueType>
            void adjust_heap_impl(
                RandomAccessIterator first,
                Distance topPosition,
                Distance heapSize,
                Distance position,
                T value)
            {
                // We do the conventional approach of moving the position down to the
                // bottom then inserting the value at the back and moving it up.
                Distance childPosition = (2 * position) + 2;

                for (; childPosition < heapSize; childPosition = (2 * childPosition) + 2)
                {
                    if (RAH2_STD::less<ValueType>()(
                            *(first + childPosition),
                            *(first + (childPosition - 1)))) // Choose the larger of the two children.
                        --childPosition;
                    *(first + position) = RAH2_STD::forward<ValueType>(
                        *(first + childPosition)); // Swap positions with this child.
                    position = childPosition;
                }

                if (childPosition == heapSize) // If we are at the very last index of the bottom...
                {
                    *(first + position) =
                        RAH2_STD::forward<ValueType>(*(first + (childPosition - 1)));
                    position = childPosition - 1;
                }

                promote_heap<RandomAccessIterator, Distance, T>(
                    first, topPosition, position, RAH2_STD::forward<ValueType>(value));
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
                T const& value)
            {
                using value_type =
                    typename RAH2_STD::iterator_traits<RandomAccessIterator>::value_type;
                adjust_heap_impl<RandomAccessIterator, Distance, T const&, value_type const>(
                    first, topPosition, heapSize, position, RAH2_STD::forward<T const&>(value));
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
                T&& value)
            {
                using value_type =
                    typename RAH2_STD::iterator_traits<RandomAccessIterator>::value_type;
                adjust_heap_impl<RandomAccessIterator, Distance, T&&, value_type>(
                    first, topPosition, heapSize, position, RAH2_STD::forward<T>(value));
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
                    *(first + position) = RAH2_STD::forward<ValueType>(
                        *(first + childPosition)); // Swap positions with this child.
                    position = childPosition;
                }

                if (childPosition == heapSize) // If we are at the bottom...
                {
                    *(first + position) =
                        RAH2_STD::forward<ValueType>(*(first + (childPosition - 1)));
                    position = childPosition - 1;
                }

                promote_heap<RandomAccessIterator, Distance, T, Compare>(
                    first, topPosition, position, RAH2_STD::forward<ValueType>(value), compare);
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
                T const& value,
                Compare compare)
            {
                using value_type =
                    typename RAH2_STD::iterator_traits<RandomAccessIterator>::value_type;
                adjust_heap_impl<RandomAccessIterator, Distance, T const&, Compare, value_type const>(
                    first, topPosition, heapSize, position, RAH2_STD::forward<T const&>(value), compare);
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
                using value_type =
                    typename RAH2_STD::iterator_traits<RandomAccessIterator>::value_type;
                adjust_heap_impl<RandomAccessIterator, Distance, T&&, Compare, value_type>(
                    first, topPosition, heapSize, position, RAH2_STD::forward<T>(value), compare);
            }
        } // namespace details

        namespace niebloids
        {
            struct push_heap
            {
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
                    RAH2_STD::enable_if_t<
                        random_access_iterator<
                            RandomAccessIterator> && sentinel_for<Sentinel, RandomAccessIterator>>* = nullptr>
                RandomAccessIterator operator()(RandomAccessIterator first, Sentinel last) const
                {
                    using difference_type =
                        typename RAH2_STD::iterator_traits<RandomAccessIterator>::difference_type;
                    using value_type =
                        typename RAH2_STD::iterator_traits<RandomAccessIterator>::value_type;
                    auto lasti = RAH2_NS::ranges::next(first, last);
                    value_type const tempBottom(RAH2_STD::forward<value_type>(*(lasti - 1)));

                    details::promote_heap<RandomAccessIterator, difference_type, value_type>(
                        first,
                        static_cast<difference_type>(0),
                        static_cast<difference_type>(lasti - first - 1),
                        RAH2_STD::forward<value_type const>(tempBottom));
                    return lasti;
                }

                template <typename RandomAccessRange>
                borrowed_iterator_t<RandomAccessRange> operator()(RandomAccessRange&& range) const
                {
                    return (*this)(RAH2_NS::ranges::begin(range), RAH2_NS::ranges::end(range));
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
                RandomAccessIterator
                operator()(RandomAccessIterator first, Sentinel last, Compare compare) const
                {
                    using difference_type =
                        typename RAH2_STD::iterator_traits<RandomAccessIterator>::difference_type;
                    using value_type =
                        typename RAH2_STD::iterator_traits<RandomAccessIterator>::value_type;
                    auto lasti = RAH2_NS::ranges::next(first, last);
                    value_type const tempBottom(*(lasti - 1));

                    details::promote_heap<RandomAccessIterator, difference_type, value_type, Compare>(
                        first,
                        static_cast<difference_type>(0),
                        static_cast<difference_type>(lasti - first - 1),
                        tempBottom,
                        compare);
                    return lasti;
                }

                template <
                    typename RandomAccessRange,
                    typename Compare,
                    RAH2_STD::enable_if_t<random_access_range<RandomAccessRange>>* = nullptr>
                borrowed_iterator_t<RandomAccessRange>
                operator()(RandomAccessRange&& range, Compare compare) const
                {
                    return (*this)(
                        RAH2_NS::ranges::begin(range),
                        RAH2_NS::ranges::end(range),
                        RAH2_STD::move(compare));
                }
            };
        } // namespace niebloids
        constexpr niebloids::push_heap push_heap;

        namespace niebloids
        {
            struct pop_heap
            {
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
                    RAH2_STD::enable_if_t<
                        random_access_iterator<
                            RandomAccessIterator> && sentinel_for<Sentinel, RandomAccessIterator>>* = nullptr>
                RandomAccessIterator operator()(RandomAccessIterator first, Sentinel last) const
                {
                    using difference_type =
                        typename RAH2_STD::iterator_traits<RandomAccessIterator>::difference_type;
                    using value_type =
                        typename RAH2_STD::iterator_traits<RandomAccessIterator>::value_type;
                    auto lasti = RAH2_NS::ranges::next(first, last);
                    value_type tempBottom(RAH2_STD::forward<value_type>(*(lasti - 1)));
                    *(lasti - 1) = RAH2_STD::forward<value_type>(*first);
                    details::adjust_heap<RandomAccessIterator, difference_type, value_type>(
                        first,
                        static_cast<difference_type>(0),
                        static_cast<difference_type>(lasti - first - 1),
                        0,
                        RAH2_STD::forward<value_type>(tempBottom));
                    return lasti;
                }

                template <typename RandomAccessRange>
                borrowed_iterator_t<RandomAccessRange> operator()(RandomAccessRange&& range) const
                {
                    return (*this)(RAH2_NS::ranges::begin(range), RAH2_NS::ranges::end(range));
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
                RandomAccessIterator
                operator()(RandomAccessIterator first, Sentinel last, Compare compare) const
                {
                    using difference_type =
                        typename RAH2_STD::iterator_traits<RandomAccessIterator>::difference_type;
                    using value_type =
                        typename RAH2_STD::iterator_traits<RandomAccessIterator>::value_type;
                    auto lasti = RAH2_NS::ranges::next(first, last);
                    value_type tempBottom(RAH2_STD::forward<value_type>(*(lasti - 1)));
                    *(lasti - 1) = RAH2_STD::forward<value_type>(*first);
                    details::adjust_heap<RandomAccessIterator, difference_type, value_type, Compare>(
                        first,
                        static_cast<difference_type>(0),
                        static_cast<difference_type>(lasti - first - 1),
                        0,
                        RAH2_STD::forward<value_type>(tempBottom),
                        compare);
                    return lasti;
                }

                template <
                    typename RandomAccessRange,
                    typename Compare,
                    RAH2_STD::enable_if_t<random_access_range<RandomAccessRange>>* = nullptr>
                borrowed_iterator_t<RandomAccessRange>
                operator()(RandomAccessRange&& range, Compare compare) const
                {
                    return (*this)(
                        RAH2_NS::ranges::begin(range),
                        RAH2_NS::ranges::end(range),
                        RAH2_STD::move(compare));
                }
            };
        } // namespace niebloids
        constexpr niebloids::pop_heap pop_heap;

        namespace niebloids
        {
            struct make_heap
            {
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
                    RAH2_STD::enable_if_t<
                        random_access_iterator<
                            RandomAccessIterator> && sentinel_for<Sentinel, RandomAccessIterator>>* = nullptr>
                RandomAccessIterator operator()(RandomAccessIterator first, Sentinel last) const
                {
                    // We do bottom-up heap construction as per Sedgewick. Such construction is O(n).
                    using difference_type =
                        typename RAH2_STD::iterator_traits<RandomAccessIterator>::difference_type;
                    using value_type =
                        typename RAH2_STD::iterator_traits<RandomAccessIterator>::value_type;

                    auto lasti = ranges::next(first, last);
                    difference_type const heapSize = lasti - first;

                    if (heapSize
                        >= 2) // If there is anything to do... (we need this check because otherwise the math fails below).
                    {
                        difference_type parentPosition =
                            ((heapSize - 2) >> 1)
                            + 1; // We use '>> 1' instead of '/ 2' because we have seen VC++ generate better code with >>.

                        do
                        {
                            --parentPosition;
                            value_type temp(RAH2_STD::forward<value_type>(*(first + parentPosition)));
                            details::adjust_heap<RandomAccessIterator, difference_type, value_type>(
                                first,
                                parentPosition,
                                heapSize,
                                parentPosition,
                                RAH2_STD::forward<value_type>(temp));
                        } while (parentPosition != 0);
                    }
                    return lasti;
                }

                template <typename RandomAccessRange>
                borrowed_iterator_t<RandomAccessRange> operator()(RandomAccessRange&& range) const
                {
                    return (*this)(RAH2_NS::ranges::begin(range), RAH2_NS::ranges::end(range));
                }

                template <typename RandomAccessIterator, typename Sentinel, typename Compare>
                RandomAccessIterator
                operator()(RandomAccessIterator first, Sentinel last, Compare compare) const
                {
                    using difference_type =
                        typename RAH2_STD::iterator_traits<RandomAccessIterator>::difference_type;
                    using value_type =
                        typename RAH2_STD::iterator_traits<RandomAccessIterator>::value_type;

                    auto lasti = ranges::next(first, last);
                    difference_type const heapSize = lasti - first;

                    if (heapSize
                        >= 2) // If there is anything to do... (we need this check because otherwise the math fails below).
                    {
                        difference_type parentPosition =
                            ((heapSize - 2) >> 1)
                            + 1; // We use '>> 1' instead of '/ 2' because we have seen VC++ generate better code with >>.

                        do
                        {
                            --parentPosition;
                            value_type temp(RAH2_STD::forward<value_type>(*(first + parentPosition)));
                            details::adjust_heap<RandomAccessIterator, difference_type, value_type, Compare>(
                                first,
                                parentPosition,
                                heapSize,
                                parentPosition,
                                RAH2_STD::forward<value_type>(temp),
                                compare);
                        } while (parentPosition != 0);
                    }
                    return lasti;
                }

                template <
                    typename RandomAccessRange,
                    typename Compare,
                    RAH2_STD::enable_if_t<random_access_range<RandomAccessRange>>* = nullptr>
                borrowed_iterator_t<RandomAccessRange>
                operator()(RandomAccessRange&& range, Compare compare) const
                {
                    return (*this)(
                        RAH2_NS::ranges::begin(range),
                        RAH2_NS::ranges::end(range),
                        RAH2_STD::move(compare));
                }
            };
        } // namespace niebloids
        constexpr niebloids::make_heap make_heap;

        namespace niebloids
        {
            struct sort_heap
            {
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
                RandomAccessIterator operator()(RandomAccessIterator first, Sentinel last) const
                {
                    auto lasti = RAH2_NS::ranges::next(first, last);
                    auto res = lasti;
                    for (; (lasti - first) > 1; --lasti) // We simply use the heap to sort itself.
                        RAH2_NS::ranges::pop_heap(first, lasti);
                    return res;
                }

                template <typename RandomAccessRange>
                borrowed_iterator_t<RandomAccessRange> operator()(RandomAccessRange&& range) const
                {
                    return (*this)(RAH2_NS::ranges::begin(range), RAH2_NS::ranges::end(range));
                }

                /// sort_heap
                ///
                /// The Compare function must work equivalently to the compare function used
                /// to make and maintain the heap.
                ///
                template <typename RandomAccessIterator, typename Sentinel, typename Compare>
                RandomAccessIterator
                operator()(RandomAccessIterator first, Sentinel last, Compare compare) const
                {
                    auto lasti = RAH2_NS::ranges::next(first, last);
                    auto res = lasti;
                    for (; (lasti - first) > 1; --lasti) // We simply use the heap to sort itself.
                        RAH2_NS::ranges::pop_heap(first, lasti, compare);
                    return res;
                }

                template <typename RandomAccessRange, typename Compare>
                borrowed_iterator_t<RandomAccessRange>
                operator()(RandomAccessRange&& range, Compare compare) const
                {
                    return (*this)(
                        RAH2_NS::ranges::begin(range),
                        RAH2_NS::ranges::end(range),
                        RAH2_STD::move(compare));
                }
            };
        } // namespace niebloids
        constexpr niebloids::sort_heap sort_heap;

        namespace niebloids
        {
            struct is_heap_until
            {
                ///////////////////////////////////////////////////////////////////////
                // is_heap_until
                ///////////////////////////////////////////////////////////////////////

                /// is_heap_until
                ///
                template <typename RandomAccessIterator, typename Sentinel>
                RandomAccessIterator operator()(RandomAccessIterator first, Sentinel last) const
                {
                    int counter = 0;

                    for (RandomAccessIterator child = first + 1; child < last; ++child, counter ^= 1)
                    {
                        if (*first
                            < *child) // We must use operator <, and are not allowed to use > or >= here.
                            return child;
                        first += counter; // counter switches between 0 and 1 every time through.
                    }

                    return last;
                }

                template <typename RandomAccessRange>
                iterator_t<RandomAccessRange> operator()(RandomAccessRange&& range) const
                {
                    return (*this)(RAH2_NS::ranges::begin(range), RAH2_NS::ranges::end(range));
                }

                /// is_heap_until
                ///
                /// The Compare function must work equivalently to the compare function used
                /// to make and maintain the heap.
                ///
                template <typename RandomAccessIterator, typename Sentinel, typename Compare>
                RandomAccessIterator
                operator()(RandomAccessIterator first, Sentinel last, Compare compare) const
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
                iterator_t<RandomAccessRange> operator()(RandomAccessRange&& range, Compare compare) const
                {
                    return (*this)(
                        RAH2_NS::ranges::begin(range),
                        RAH2_NS::ranges::end(range),
                        RAH2_STD::move(compare));
                }
            };
        } // namespace niebloids
        constexpr niebloids::is_heap_until is_heap_until;

        namespace niebloids
        {
            struct is_heap
            {
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
                    RAH2_STD::enable_if_t<
                        random_access_iterator<
                            RandomAccessIterator> && sentinel_for<Sentinel, RandomAccessIterator>>* = nullptr>
                bool operator()(RandomAccessIterator first, Sentinel last) const
                {
                    return (RAH2_NS::ranges::is_heap_until(first, last) == last);
                }

                template <typename RandomAccessRange>
                bool operator()(RandomAccessRange&& range) const
                {
                    return (*this)(RAH2_NS::ranges::begin(range), RAH2_NS::ranges::end(range));
                }

                /// is_heap
                ///
                /// The Compare function must work equivalently to the compare function used
                /// to make and maintain the heap.
                ///
                template <typename RandomAccessIterator, typename Sentinel, typename Compare>
                bool operator()(RandomAccessIterator first, Sentinel last, Compare compare) const
                {
                    return (RAH2_NS::ranges::is_heap_until(first, last, compare) == last);
                }

                template <
                    typename RandomAccessRange,
                    typename Compare,
                    RAH2_STD::enable_if_t<random_access_range<RandomAccessRange>>* = nullptr>
                bool operator()(RandomAccessRange&& range, Compare compare) const
                {
                    return (*this)(
                        RAH2_NS::ranges::begin(range),
                        RAH2_NS::ranges::end(range),
                        RAH2_STD::move(compare));
                }
            };
        } // namespace niebloids
        constexpr niebloids::is_heap is_heap;
    } // namespace ranges
} // namespace RAH2_NS
