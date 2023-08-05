#pragma once

#include "range_bases.hpp"
#include "base_algorithm.hpp"
#include "algo_heap.hpp"

namespace RAH2_NS
{
    namespace ranges
    {
        /// is_sorted_until
        ///
        /// Returns an iterator to the first element in the range [first,last) which does not follow an ascending order.
        /// The range between first and the iterator returned is sorted.
        /// If the entire range is sorted, the function returns last.
        /// The elements are compared using operator< for the first version, and comp for the second.
        template <
            typename ForwardIterator,
            typename Sentinel,
            RAH2_STD::enable_if_t<
                forward_iterator<ForwardIterator> && sentinel_for<Sentinel, ForwardIterator>>* = nullptr>
        ForwardIterator is_sorted_until(ForwardIterator first, Sentinel last)
        {
            if (first != last)
            {
                ForwardIterator next = first;

                while (++next != last)
                {
                    if (*next < *first)
                        return next;

                    first = next;
                }
            }

            return last;
        }

        template <typename ForwardIterator, typename Sentinel, typename Compare>
        ForwardIterator is_sorted_until(ForwardIterator first, Sentinel last, Compare compare)
        {
            if (first != last)
            {
                ForwardIterator next = first;

                while (++next != last)
                {
                    if (compare(*next, *first))
                        return next;

                    first = next;
                }
            }

            return last;
        }

        template <typename ForwardRange, typename Compare, RAH2_STD::enable_if_t<forward_range<ForwardRange>>* = nullptr>
        auto is_sorted_until(ForwardRange&& r, Compare compare)
        {
            return RAH2_NS::ranges::is_sorted_until(
                RAH2_NS::ranges::begin(r), RAH2_NS::ranges::end(r), RAH2_STD::move(compare));
        }

        template <typename ForwardRange>
        auto is_sorted_until(ForwardRange&& r)
        {
            return RAH2_NS::ranges::is_sorted_until(
                RAH2_NS::ranges::begin(r), RAH2_NS::ranges::end(r));
        }

        struct is_sorted_fn
        {
            template <
                typename I, // RAH2_STD::forward_iterator
                typename S, // RAH2_STD::sentinel_for<I>
                // class Proj = RAH2_STD::identity,
                typename Comp = RAH2_NS::less, // RAH2_STD::indirect_strict_weak_order<RAH2_STD::projected<I, Proj>>
                RAH2_STD::enable_if_t<forward_iterator<I> && sentinel_for<S, I>>* = nullptr>
            constexpr bool operator()(I first, S last, Comp comp = {}) const
            {
                return RAH2_NS::ranges::is_sorted_until(first, last, comp) == last;
            }

            template <
                typename R, // ranges::forward_range
                // class Proj = RAH2_STD::identity,
                typename Comp = RAH2_NS::less, // RAH2_STD::indirect_strict_weak_order<RAH2_STD::projected<ranges::iterator_t<R>, Proj>>
                RAH2_STD::enable_if_t<forward_range<R>>* = nullptr>
            constexpr bool operator()(R&& r, Comp comp = {}) const
            {
                return (*this)(
                    RAH2_NS::ranges::begin(r), RAH2_NS::ranges::end(r), RAH2_STD::ref(comp));
            }
        };

        /// is_sorted
        ///
        /// Returns true if the range [first, last) is sorted.
        /// An empty range is considered to be sorted.
        /// To test if a range is reverse-sorted, use 'greater' as the comparison
        /// instead of 'less'.
        ///
        /// Example usage:
        ///    vector<int> intArray;
        ///    bool bIsSorted        = is_sorted(intArray.begin(), intArray.end());
        ///    bool bIsReverseSorted = is_sorted(intArray.begin(), intArray.end(), greater<int>());
        ///
        constexpr is_sorted_fn is_sorted;

        template <class I1, class I2, class O>
        using merge_result = RAH2_NS::ranges::in_in_out_result<I1, I2, O>;

        /// merge
        ///
        /// This function merges two sorted input sorted ranges into a result sorted range.
        /// This merge is stable in that no element from the first range will be changed
        /// in order relative to other elements from the first range.
        ///
        template <
            typename InputIterator1,
            typename Sentinel1,
            typename InputIterator2,
            typename Sentinel2,
            typename OutputIterator,
            typename Compare = RAH2_NS::less>
        merge_result<InputIterator1, InputIterator2, OutputIterator> merge(
            InputIterator1 first1,
            Sentinel1 last1,
            InputIterator2 first2,
            Sentinel2 last2,
            OutputIterator result,
            Compare compare = {})
        {
            while ((first1 != last1) && (first2 != last2))
            {
                if (compare(*first2, *first1))
                {
                    RAH2_VALIDATE_COMPARE(
                        !compare(*first1, *first2)); // Validate that the compare function is sane.
                    *result = *first2;
                    ++first2;
                }
                else
                {
                    *result = *first1;
                    ++first1;
                }
                ++result;
            }

            // Check which list is empty and explicitly copy remaining items from the other list.
            // For performance reasons, only a single copy operation is invoked to avoid the potential overhead
            // introduced by chaining two copy operations together.  Even if a copy is of zero size there can
            // be overhead from calling memmove with a zero size copy.
            if (first1 == last1)
            {
                auto ret = RAH2_NS::ranges::copy(first2, last2, result);
                return {first1, ret.in, ret.out};
            }
            else
            {
                auto ret = RAH2_NS::ranges::copy(first1, last1, result);
                return {ret.in, first2, ret.out};
            }
        }

        template <typename InputRange1, typename InputRange2, typename OutputIterator, typename Compare = RAH2_NS::less>
        merge_result<
            RAH2_NS::ranges::borrowed_iterator_t<InputRange1>,
            RAH2_NS::ranges::borrowed_iterator_t<InputRange2>,
            OutputIterator>
        merge(InputRange1&& range1, InputRange2&& range2, OutputIterator result, Compare compare = {})
        {
            return RAH2_NS::ranges::merge(
                RAH2_NS::ranges::begin(range1),
                RAH2_NS::ranges::end(range1),
                RAH2_NS::ranges::begin(range2),
                RAH2_NS::ranges::end(range2),
                RAH2_STD::move(result),
                RAH2_STD::move(compare));
        }

        //////////////////////////////////////////////////////////////////////////////
        /// insertion_sort
        ///
        /// insertion_sort is an O(n^2) stable sorting algorithm that starts at the
        /// (k + 1) element and assumes the first (k) elements are sorted.
        /// Then copy_backwards from (k + 1) to the begining any elements where the
        /// (k + 1) element is less than [0, k] elements. The position of k when
        /// (k + 1) element is not less than k is the sorted position of the (k + 1) element.
        ///
        /// Example With Intermediate Steps:
        /// (k + 1) == 2 : [3, 2, 1] -> [3, 3, 1] -> [2, 3, 1]
        /// (k + 1) == 1 : [2, 3, 1] -> [2, 3, 3] -> [2, 2, 3] -> [1, 2, 3]
        ///              : [1, 2, 3]
        template <typename BidirectionalIterator, typename Sentinel, typename StrictWeakOrdering>
        void insertion_sort(BidirectionalIterator first, Sentinel last, StrictWeakOrdering compare)
        {
            using value_type = typename RAH2_STD::iterator_traits<BidirectionalIterator>::value_type;

            if (first != last)
            {
                BidirectionalIterator i = first;

                for (++i; i != last; ++i)
                {
                    value_type insertValue(RAH2_STD::move(*i));
                    BidirectionalIterator insertPosition = i;

                    for (BidirectionalIterator movePosition = i;
                         movePosition != first && compare(insertValue, *(--movePosition));
                         --insertPosition)
                    {
                        RAH2_VALIDATE_COMPARE(!compare(*movePosition, insertValue));
                        *insertPosition = RAH2_STD::move(*movePosition);
                    }

                    *insertPosition = RAH2_STD::move(insertValue);
                }
            }
        } // insertion_sort

        template <typename BidirectionalIterator, typename Sentinel>
        void insertion_sort(BidirectionalIterator first, Sentinel last)
        {
            using Less =
                RAH2_STD::less<typename RAH2_STD::iterator_traits<BidirectionalIterator>::value_type>;

            insertion_sort<BidirectionalIterator>(first, last, Less());

        } // insertion_sort

        namespace Internal
        {
            // Sorts a range whose initial (start - first) entries are already sorted.
            // This function is a useful helper to the tim_sort function.
            // This is the same as insertion_sort except that it has a start parameter which indicates
            // where the start of the unsorted data is.
            template <typename BidirectionalIterator, typename Sentinel, typename StrictWeakOrdering>
            void insertion_sort_already_started(
                BidirectionalIterator first,
                Sentinel last,
                BidirectionalIterator start,
                StrictWeakOrdering compare)
            {
                using value_type =
                    typename RAH2_STD::iterator_traits<BidirectionalIterator>::value_type;

                if (first != last) // if the range is non-empty...
                {
                    BidirectionalIterator iCurrent, iNext, iSorted = start - 1;

                    for (++iSorted; iSorted != last; ++iSorted)
                    {
                        value_type const temp(*iSorted);

                        iNext = iCurrent = iSorted;

                        for (--iCurrent; (iNext != first) && compare(temp, *iCurrent);
                             --iNext, --iCurrent)
                        {
                            RAH2_VALIDATE_COMPARE(!compare(
                                *iCurrent, temp)); // Validate that the compare function is sane.
                            *iNext = *iCurrent;
                        }

                        *iNext = temp;
                    }
                }
            }
        } // namespace Internal

        /// merge_sort_buffer
        ///
        /// Implements the MergeSort algorithm with a user-supplied buffer.
        /// The input buffer must be able to hold a number of items equal to 'last - first'.
        /// Note that merge_sort_buffer requires a random access iterator, which usually means
        /// an array (eg. vector, deque).
        ///
        /// The algorithm used for merge sort is not the standard merge sort.  It has been modified
        /// to improve performance for data that is already partially sorted.  In fact, if data
        /// is completely sorted, then performance is O(n), but even data with partially sorted
        /// regions can benefit from the modifications.
        ///
        /// 'InsertionSortLimit' specifies a size limit for which the algorithm will use insertion sort.
        /// Due to the overhead of merge sort, it is often faster to use insertion sort once the size of a region
        /// is fairly small.  However, insertion sort is not as efficient (in terms of assignments orcomparisons)
        /// so choosing a value that is too large will reduce performance.  Generally a value of 16 to 32 is reasonable,
        /// but the best choose will depend on the data being sorted.
        template <
            typename RandomAccessIterator,
            typename Sentinel,
            typename T,
            typename StrictWeakOrdering,
            typename difference_type,
            int InsertionSortLimit>
        class MergeSorter
        {
        public:
            static void
            sort(RandomAccessIterator first, Sentinel last, T* pBuffer, StrictWeakOrdering compare)
            {
                if (sort_impl(first, last, pBuffer, difference_type(0), compare) == RL_Buffer)
                {
                    difference_type const nCount = last - first;
                    RAH2_NS::ranges::copy(pBuffer, pBuffer + nCount, first);
                }
                RAH2_DEV_ASSERT((RAH2_NS::ranges::is_sorted.
                                 operator()<RandomAccessIterator, Sentinel, StrictWeakOrdering>(
                                     first, last, compare)));
            }

        private:
            static_assert(
                InsertionSortLimit > 1,
                "Sequences of length 1 are already sorted.  Use a larger value for "
                "InsertionSortLimit");

            enum ResultLocation
            {
                RL_SourceRange, // i.e. result is in the range defined by [first, last)
                RL_Buffer, // i.e. result is in pBuffer
            };

            // sort_impl
            //
            // This sort routine sorts the data in [first, last) and places the result in pBuffer or in the original range of the input.  The actual
            // location of the data is indicated by the enum returned.
            //
            // lastSortedEnd is used to specify a that data in the range [first, first + lastSortedEnd] is already sorted.  This information is used
            // to avoid unnecessary merge sorting of already sorted data.  lastSortedEnd is a hint, and can be an under estimate of the sorted elements
            // (i.e. it is legal to pass 0).
            static ResultLocation sort_impl(
                RandomAccessIterator first,
                Sentinel last,
                T* pBuffer,
                difference_type lastSortedEnd,
                StrictWeakOrdering compare)
            {
                difference_type const nCount = last - first;

                if (lastSortedEnd < 1)
                {
                    lastSortedEnd =
                        RAH2_NS::ranges::is_sorted_until<RandomAccessIterator, Sentinel, StrictWeakOrdering>(
                            first, last, compare)
                        - first;
                }

                // Sort the region unless lastSortedEnd indicates it is already sorted.
                if (lastSortedEnd < nCount)
                {
                    // If the size is less than or equal to InsertionSortLimit use insertion sort instead of recursing further.
                    if (nCount <= InsertionSortLimit)
                    {
                        RAH2_NS::ranges::Internal::
                            insertion_sort_already_started<RandomAccessIterator, Sentinel, StrictWeakOrdering>(
                                first, last, first + lastSortedEnd, compare);
                        return RL_SourceRange;
                    }
                    else
                    {
                        difference_type const nMid = nCount / 2;

                        ResultLocation firstHalfLocation = RL_SourceRange;
                        // Don't sort the first half if it is already sorted.
                        if (lastSortedEnd < nMid)
                        {
                            firstHalfLocation =
                                sort_impl(first, first + nMid, pBuffer, lastSortedEnd, compare);
                        }

                        ResultLocation const secondHalfLocation = sort_impl(
                            first + nMid, last, pBuffer + nMid, lastSortedEnd - nMid, compare);

                        return MergeSorter::merge_halves(
                            first, last, nMid, pBuffer, firstHalfLocation, secondHalfLocation, compare);
                    }
                }
                else
                {
                    RAH2_DEV_ASSERT((RAH2_NS::ranges::is_sorted.
                                     operator()<RandomAccessIterator, Sentinel, StrictWeakOrdering>(
                                         first, last, compare)));
                    return RL_SourceRange;
                }
            }

            // merge_halves
            //
            // Merge two sorted regions of elements.
            // The inputs to this method effectively define two large buffers.  The variables 'firstHalfLocation' and 'secondHalfLocation' define where the data to be
            // merged is located within the two buffers.  It is entirely possible that the two areas to be merged could be entirely located in either of the larger buffers.
            // Upon returning the merged results will be in one of the two buffers (indicated by the return result).
            static ResultLocation merge_halves(
                RandomAccessIterator first,
                Sentinel last,
                difference_type nMid,
                T* pBuffer,
                ResultLocation firstHalfLocation,
                ResultLocation secondHalfLocation,
                StrictWeakOrdering compare)
            {
                difference_type const nCount = last - first;
                if (firstHalfLocation == RL_SourceRange)
                {
                    if (secondHalfLocation == RL_SourceRange)
                    {
                        RAH2_NS::ranges::merge<
                            RandomAccessIterator,
                            RandomAccessIterator,
                            RandomAccessIterator,
                            Sentinel,
                            T*,
                            StrictWeakOrdering>(
                            first, first + nMid, first + nMid, last, pBuffer, compare);
                        RAH2_DEV_ASSERT(
                            (RAH2_NS::ranges::is_sorted.operator()<T*, T*, StrictWeakOrdering>(
                                pBuffer, pBuffer + nCount, compare)));
                        return RL_Buffer;
                    }
                    else
                    {
                        RAH2_NS::ranges::copy(first, first + nMid, pBuffer);
                        RAH2_NS::ranges::merge<T*, T*, T*, T*, RandomAccessIterator, StrictWeakOrdering>(
                            pBuffer, pBuffer + nMid, pBuffer + nMid, pBuffer + nCount, first, compare);
                        RAH2_DEV_ASSERT(
                            (RAH2_NS::ranges::is_sorted.
                             operator()<RandomAccessIterator, Sentinel, StrictWeakOrdering>(
                                 first, last, compare)));
                        return RL_SourceRange;
                    }
                }
                else
                {
                    if (secondHalfLocation == RL_SourceRange)
                    {
                        RAH2_NS::ranges::copy(first + nMid, last, pBuffer + nMid);
                        RAH2_NS::ranges::merge<T*, T*, T*, T*, RandomAccessIterator, StrictWeakOrdering>(
                            pBuffer, pBuffer + nMid, pBuffer + nMid, pBuffer + nCount, first, compare);
                        RAH2_DEV_ASSERT(
                            (RAH2_NS::ranges::is_sorted.
                             operator()<RandomAccessIterator, Sentinel, StrictWeakOrdering>(
                                 first, last, compare)));
                        return RL_SourceRange;
                    }
                    else
                    {
                        RAH2_NS::ranges::merge<T*, T*, T*, T*, RandomAccessIterator, StrictWeakOrdering>(
                            pBuffer, pBuffer + nMid, pBuffer + nMid, pBuffer + nCount, first, compare);
                        RAH2_DEV_ASSERT(
                            (RAH2_NS::ranges::is_sorted.
                             operator()<RandomAccessIterator, Sentinel, StrictWeakOrdering>(
                                 first, last, compare)));
                        return RL_SourceRange;
                    }
                }
            }
        };

        template <typename RandomAccessIterator, typename Sentinel, typename T, typename StrictWeakOrdering>
        void merge_sort_buffer(
            RandomAccessIterator first, Sentinel last, T* pBuffer, StrictWeakOrdering compare)
        {
            using difference_type =
                typename RAH2_STD::iterator_traits<RandomAccessIterator>::difference_type;
            MergeSorter<RandomAccessIterator, Sentinel, T, StrictWeakOrdering, difference_type, 16>::sort(
                first, last, pBuffer, compare);
        }

        template <typename RandomAccessIterator, typename Sentinel, typename T>
        void merge_sort_buffer(RandomAccessIterator first, Sentinel last, T* pBuffer)
        {
            using Less =
                RAH2_STD::less<typename RAH2_STD::iterator_traits<RandomAccessIterator>::value_type>;

            RAH2_NS::ranges::merge_sort_buffer<RandomAccessIterator, T, Less>(
                first, last, pBuffer, Less());
        }

        /// merge_sort
        ///
        /// Implements the MergeSort algorithm.
        /// This algorithm allocates memory via the user-supplied allocator. Use merge_sort_buffer
        /// function if you want a version which doesn't allocate memory.
        /// Note that merge_sort requires a random access iterator, which usually means
        /// an array (eg. vector, deque).
        ///
        template <typename RandomAccessIterator, typename Sentinel, typename Allocator, typename StrictWeakOrdering>
        void merge_sort(
            RandomAccessIterator first, Sentinel last, Allocator& allocator, StrictWeakOrdering compare)
        {
            using difference_type =
                typename RAH2_STD::iterator_traits<RandomAccessIterator>::difference_type;
            using value_type = typename RAH2_STD::iterator_traits<RandomAccessIterator>::value_type;

            difference_type const nCount = last - first;

            if (nCount > 1)
            {
                // We need to allocate an array of nCount value_type objects as a temporary buffer.
#ifdef RAH2_USE_EASTL
                value_type* const pBuffer = (value_type*)allocate_memory(
                    allocator, nCount * sizeof(value_type), EASTL_ALIGN_OF(value_type), 0);
#else
                value_type* const pBuffer = allocator.allocate(static_cast<size_t>(nCount));
#endif

                RAH2_STD::uninitialized_fill(pBuffer, pBuffer + nCount, value_type());

                RAH2_NS::ranges::merge_sort_buffer<RandomAccessIterator, Sentinel, value_type, StrictWeakOrdering>(
                    first, last, pBuffer, compare);

                RAH2_NS::ranges::destroy(pBuffer, pBuffer + nCount);
#ifdef RAH2_USE_EASTL
                EASTLFree(allocator, pBuffer, nCount * sizeof(value_type));
#else
                allocator.deallocate(pBuffer, static_cast<size_t>(nCount));
#endif
            }
        }

        template <typename RandomAccessIterator, typename Sentinel, typename Allocator>
        void merge_sort(RandomAccessIterator first, Sentinel last, Allocator& allocator)
        {
            using Less =
                RAH2_STD::less<typename RAH2_STD::iterator_traits<RandomAccessIterator>::value_type>;

            RAH2_NS::ranges::merge_sort<RandomAccessIterator, Sentinel, Allocator, Less>(
                first, last, allocator, Less());
        }

        /// partition
        ///
        /// Implements the partition algorithm.
        /// Rearranges the elements in the range [first, last), in such a way that all the elements
        /// for which pred returns true precede all those for which it returns false. The iterator
        /// returned points to the first element of the second group.
        /// The relative ordering within each group is not necessarily the same as before the call.
        /// See function stable_partition for a function with a similar behavior and stability in
        /// the ordering.
        ///
        /// To do: Implement a version that uses a faster BidirectionalIterator algorithm for the
        ///        case that the iterator range is a bidirectional iterator instead of just an
        ///        input iterator (one direction).
        ///
        template <typename InputIterator, typename Sentinel, typename Predicate>
        subrange<InputIterator, Sentinel>
        partition(InputIterator begin, Sentinel end, Predicate predicate)
        {
            if (begin != end)
            {
                while (predicate(*begin))
                {
                    if (++begin == end)
                        return {begin, end};
                }

                InputIterator middle = begin;

                while (++middle != end)
                {
                    if (predicate(*middle))
                    {
                        RAH2_STD::swap(*begin, *middle);
                        ++begin;
                    }
                }
            }

            return {begin, end};
        }

        template <typename InputRange, typename Predicate>
        auto partition(InputRange&& range, Predicate predicate)
        {
            return RAH2_NS::ranges::partition(
                RAH2_NS::ranges::begin(range), RAH2_NS::ranges::end(range), RAH2_STD::move(predicate));
        }

        /// stable_partition
        ///
        /// Performs the same function as @p partition() with the additional
        /// guarantee that the relative ordering of elements in each group is
        /// preserved.
        template <typename ForwardIterator, typename ForwardSentinel, typename Predicate>
        subrange<ForwardIterator>
        stable_partition(ForwardIterator first, ForwardSentinel last, Predicate pred)
        {
            first = RAH2_NS::ranges::find_if_not(first, last, pred);

            if (first == last)
                return {first, last};

            using value_type = typename RAH2_STD::iterator_traits<ForwardIterator>::value_type;

            auto const requested_size = RAH2_NS::ranges::distance(first, last);

#ifdef RAH2_USE_EASTL
            auto allocator = *RAH2_STD::get_default_allocator(0);
            value_type* const buffer = (value_type*)allocate_memory(
                allocator, requested_size * sizeof(value_type), EASTL_ALIGN_OF(value_type), 0);
#else
            auto allocator = RAH2_STD::allocator<value_type>();
            value_type* const buffer = allocator.allocate(static_cast<size_t>(requested_size));
#endif
            RAH2_STD::uninitialized_fill(buffer, buffer + requested_size, value_type());

            ForwardIterator result1 = first;
            value_type* result2 = buffer;

            *result2 = RAH2_STD::move(*first);
            ++result2;
            ++first;
            for (; first != last; ++first)
            {
                if (pred(*first))
                {
                    *result1 = RAH2_STD::move(*first);
                    ++result1;
                }
                else
                {
                    *result2 = RAH2_STD::move(*first);
                    ++result2;
                }
            }

            RAH2_NS::ranges::copy(buffer, result2, result1);

            RAH2_NS::ranges::destroy(buffer, buffer + requested_size);
#ifdef RAH2_USE_EASTL
            EASTLFree(allocator, buffer, requested_size * sizeof(value_type));
#else
            allocator.deallocate(buffer, requested_size * sizeof(value_type));
#endif

            return {result1, last};
        }

        template <typename ForwardRange, typename Predicate>
        auto stable_partition(ForwardRange&& range, Predicate pred)
        {
            return RAH2_NS::ranges::stable_partition(
                RAH2_NS::ranges::begin(range), RAH2_NS::ranges::end(range), RAH2_STD::move(pred));
        }

        /////////////////////////////////////////////////////////////////////
        // quick_sort
        //
        // We do the "introspection sort" variant of quick sort which is now
        // well-known and understood. You can read about this algorithm in
        // many articles on quick sort, but briefly what it does is a median-
        // of-three quick sort whereby the recursion depth is limited to a
        // some value (after which it gives up on quick sort and switches to
        // a heap sort) and whereby after a certain amount of sorting the
        // algorithm stops doing quick-sort and finishes the sorting via
        // a simple insertion sort.
        /////////////////////////////////////////////////////////////////////

        static int const kQuickSortLimit =
            28; // For sorts of random arrays over 100 items, 28 - 32 have been found to be good numbers on x86.

        namespace Internal
        {
            template <typename Size>
            Size Log2(Size n)
            {
                int i;
                for (i = 0; n; ++i)
                    n >>= 1;
                return i - 1;
            }
        } // namespace Internal

        template <typename RandomAccessIterator, typename Sentinel, typename T>
        RandomAccessIterator get_partition_impl(RandomAccessIterator first, Sentinel last, T&& pivotValue)
        {
            using PureT = RAH2_STD::decay_t<T>;

            for (;; ++first)
            {
                while (RAH2_STD::less<PureT>()(*first, pivotValue))
                {
                    RAH2_VALIDATE_COMPARE(!RAH2_STD::less<PureT>()(
                        pivotValue, *first)); // Validate that the compare function is sane.
                    ++first;
                }
                --last;

                while (RAH2_STD::less<PureT>()(pivotValue, *last))
                {
                    RAH2_VALIDATE_COMPARE(!RAH2_STD::less<PureT>()(
                        *last, pivotValue)); // Validate that the compare function is sane.
                    --last;
                }

                if (first >= last) // Random access iterators allow operator >=
                    return first;

                RAH2_NS::ranges::iter_swap(first, last);
            }
        }

        /// get_partition
        ///
        /// This function takes const T& instead of T because T may have special alignment
        /// requirements and some compilers (e.g. VC++) are don't respect alignment requirements
        /// for function arguments.
        ///
        template <typename RandomAccessIterator, typename Sentinel, typename T>
        RandomAccessIterator get_partition(RandomAccessIterator first, Sentinel last, T const& pivotValue)
        {
            // NOLINTNEXTLINE(performance-unnecessary-copy-initialization)
            T const pivotCopy(
                pivotValue); // Need to make a temporary because the sequence below is mutating.
            return get_partition_impl<RandomAccessIterator, Sentinel, T const&>(
                first, last, pivotCopy);
        }

        template <typename RandomAccessIterator, typename Sentinel, typename T>
        RandomAccessIterator get_partition(RandomAccessIterator first, Sentinel last, T&& pivotValue)
        {
            // Note: unlike the copy-constructible variant of get_partition... we can't create a temporary const move-constructible object
            return get_partition_impl<RandomAccessIterator, T&&>(
                first, last, RAH2_STD::move(pivotValue));
        }

        template <typename RandomAccessIterator, typename Sentinel, typename T, typename Compare>
        RandomAccessIterator
        get_partition_impl(RandomAccessIterator first, Sentinel last, T&& pivotValue, Compare compare)
        {
            for (;; ++first)
            {
                while (compare(*first, pivotValue))
                {
                    RAH2_VALIDATE_COMPARE(!compare(
                        pivotValue, *first)); // Validate that the compare function is sane.
                    ++first;
                }
                --last;

                while (compare(pivotValue, *last))
                {
                    RAH2_VALIDATE_COMPARE(
                        !compare(*last, pivotValue)); // Validate that the compare function is sane.
                    --last;
                }

                if (first >= last) // Random access iterators allow operator >=
                    return first;

                RAH2_NS::ranges::iter_swap(first, last);
            }
        }

        template <typename RandomAccessIterator, typename Sentinel, typename T, typename Compare>
        RandomAccessIterator
        get_partition(RandomAccessIterator first, Sentinel last, T const& pivotValue, Compare compare)
        {
            T const pivotCopy(
                pivotValue); // Need to make a temporary because the sequence below is mutating.
            return get_partition_impl<RandomAccessIterator, Sentinel, T const&, Compare>(
                first, last, pivotCopy, compare);
        }

        template <typename RandomAccessIterator, typename Sentinel, typename T, typename Compare>
        RandomAccessIterator
        get_partition(RandomAccessIterator first, Sentinel last, T&& pivotValue, Compare compare)
        {
            // Note: unlike the copy-constructible variant of get_partition... we can't create a temporary const move-constructible object
            return get_partition_impl<RandomAccessIterator, T&&, Compare>(
                first, last, RAH2_STD::forward<T>(pivotValue), compare);
        }

        namespace Internal
        {
            // This function is used by quick_sort and is not intended to be used by itself.
            // This is because the implementation below makes an assumption about the input
            // data that quick_sort satisfies but arbitrary data may not.
            // There is a standalone insertion_sort function.
            template <typename RandomAccessIterator, typename Sentinel>
            void insertion_sort_simple(RandomAccessIterator first, Sentinel last)
            {
                for (RandomAccessIterator current = first; current != last; ++current)
                {
                    using value_type =
                        typename RAH2_STD::iterator_traits<RandomAccessIterator>::value_type;

                    RandomAccessIterator end(current), prev(current);
                    value_type value(RAH2_STD::forward<value_type>(*current));

                    for (--prev; RAH2_STD::less<value_type>()(value, *prev);
                         --end,
                         --prev) // We skip checking for (prev >= first) because quick_sort (our caller) makes this unnecessary.
                    {
                        RAH2_VALIDATE_COMPARE(!RAH2_STD::less<value_type>()(
                            *prev, value)); // Validate that the compare function is sane.
                        *end = RAH2_STD::forward<value_type>(*prev);
                    }

                    *end = RAH2_STD::forward<value_type>(value);
                }
            }

            // This function is used by quick_sort and is not intended to be used by itself.
            // This is because the implementation below makes an assumption about the input
            // data that quick_sort satisfies but arbitrary data may not.
            // There is a standalone insertion_sort function.
            template <typename RandomAccessIterator, typename Sentinel, typename Compare>
            void insertion_sort_simple(RandomAccessIterator first, Sentinel last, Compare compare)
            {
                for (RandomAccessIterator current = first; current != last; ++current)
                {
                    using value_type =
                        typename RAH2_STD::iterator_traits<RandomAccessIterator>::value_type;

                    RandomAccessIterator end(current), prev(current);
                    value_type value(RAH2_STD::forward<value_type>(*current));

                    for (--prev; compare(value, *prev);
                         --end,
                         --prev) // We skip checking for (prev >= first) because quick_sort (our caller) makes this unnecessary.
                    {
                        RAH2_VALIDATE_COMPARE(
                            !compare(*prev, value)); // Validate that the compare function is sane.
                        *end = RAH2_STD::forward<value_type>(*prev);
                    }

                    *end = RAH2_STD::forward<value_type>(value);
                }
            }
        } // namespace Internal

        template <typename RandomAccessIterator, typename Sentinel>
        void partial_sort(RandomAccessIterator first, RandomAccessIterator middle, Sentinel last)
        {
            using difference_type =
                typename RAH2_STD::iterator_traits<RandomAccessIterator>::difference_type;
            using value_type = typename RAH2_STD::iterator_traits<RandomAccessIterator>::value_type;

            RAH2_NS::ranges::make_heap<RandomAccessIterator, RandomAccessIterator>(first, middle);

            for (RandomAccessIterator i = middle; i < last; ++i)
            {
                if (RAH2_STD::less<value_type>()(*i, *first))
                {
                    RAH2_VALIDATE_COMPARE(!RAH2_STD::less<value_type>()(
                        *first, *i)); // Validate that the compare function is sane.
                    value_type temp(RAH2_STD::forward<value_type>(*i));
                    *i = RAH2_STD::forward<value_type>(*first);
                    RAH2_NS::ranges::adjust_heap<RandomAccessIterator, difference_type, value_type>(
                        first,
                        difference_type(0),
                        difference_type(middle - first),
                        difference_type(0),
                        RAH2_STD::forward<value_type>(temp));
                }
            }

            RAH2_NS::ranges::sort_heap<RandomAccessIterator, RandomAccessIterator>(first, middle);
        }

        template <typename RandomAccessRange, typename RandomAccessIterator>
        void partial_sort(RandomAccessRange&& range, RandomAccessIterator middle)
        {
            return RAH2_NS::ranges::partial_sort(
                RAH2_NS::ranges::begin(range), middle, RAH2_NS::ranges::end(range));
        }

        template <typename RandomAccessIterator, typename Sentinel, typename Compare>
        void partial_sort(
            RandomAccessIterator first, RandomAccessIterator middle, Sentinel last, Compare compare)
        {
            using difference_type =
                typename RAH2_STD::iterator_traits<RandomAccessIterator>::difference_type;
            using value_type = typename RAH2_STD::iterator_traits<RandomAccessIterator>::value_type;

            RAH2_NS::ranges::make_heap<RandomAccessIterator, RandomAccessIterator, Compare>(
                first, middle, compare);

            for (RandomAccessIterator i = middle; i < last; ++i)
            {
                if (compare(*i, *first))
                {
                    RAH2_VALIDATE_COMPARE(
                        !compare(*first, *i)); // Validate that the compare function is sane.
                    value_type temp(RAH2_STD::forward<value_type>(*i));
                    *i = RAH2_STD::forward<value_type>(*first);
                    RAH2_NS::ranges::adjust_heap<RandomAccessIterator, difference_type, value_type, Compare>(
                        first,
                        difference_type(0),
                        difference_type(middle - first),
                        difference_type(0),
                        RAH2_STD::forward<value_type>(temp),
                        compare);
                }
            }

            RAH2_NS::ranges::sort_heap<RandomAccessIterator, RandomAccessIterator, Compare>(
                first, middle, compare);
        }

        template <typename RandomAccessRange, typename RandomAccessIterator, typename Compare>
        void partial_sort(RandomAccessRange&& range, RandomAccessIterator middle, Compare compare)
        {
            return RAH2_NS::ranges::partial_sort(
                RAH2_NS::ranges::begin(range),
                middle,
                RAH2_NS::ranges::end(range),
                RAH2_STD::move(compare));
        }

        template <
            typename RandomAccessIterator,
            typename Sentinel,
            RAH2_STD::enable_if_t<
                random_access_iterator<RandomAccessIterator>
                && sentinel_for<Sentinel, RandomAccessIterator>>* = nullptr>
        RandomAccessIterator
        nth_element(RandomAccessIterator first, RandomAccessIterator nth, Sentinel last)
        {
            using value_type = typename RAH2_STD::iterator_traits<RandomAccessIterator>::value_type;
            auto result = RAH2_NS::ranges::next(first, last);
            auto lasti = result;
            while ((lasti - first) > 5)
            {
                value_type const midValue(RAH2_NS::ranges::median<value_type>(
                    *first, *(first + (lasti - first) / 2), *(lasti - 1)));
                RandomAccessIterator const midPos(
                    RAH2_NS::ranges::get_partition<RandomAccessIterator, Sentinel, value_type>(
                        first, lasti, midValue));

                if (midPos <= nth)
                    first = midPos;
                else
                    lasti = midPos;
            }

            RAH2_NS::ranges::insertion_sort<RandomAccessIterator>(first, lasti);
            return result;
        }

        template <typename RandomAccessRange>
        iterator_t<RandomAccessRange>
        nth_element(RandomAccessRange&& range, iterator_t<RandomAccessRange> nth)
        {
            return RAH2_NS::ranges::nth_element(
                RAH2_NS::ranges::begin(range), RAH2_STD::move(nth), RAH2_NS::ranges::end(range));
        }

        template <typename RandomAccessIterator, typename Sentinel, typename Compare>
        RandomAccessIterator nth_element(
            RandomAccessIterator first, RandomAccessIterator nth, Sentinel last, Compare compare)
        {
            using value_type = typename RAH2_STD::iterator_traits<RandomAccessIterator>::value_type;
            auto result = RAH2_NS::ranges::next(first, last);
            auto lasti = result;
            while ((lasti - first) > 5)
            {
                value_type const midValue(RAH2_NS::ranges::median<value_type, Compare>(
                    *first, *(first + (lasti - first) / 2), *(lasti - 1), compare));
                RandomAccessIterator const midPos(
                    RAH2_NS::ranges::get_partition<RandomAccessIterator, Sentinel, value_type, Compare>(
                        first, lasti, midValue, compare));

                if (midPos <= nth)
                    first = midPos;
                else
                    lasti = midPos;
            }

            RAH2_NS::ranges::insertion_sort<RandomAccessIterator, Sentinel, Compare>(
                first, lasti, compare);
            return result;
        }

        template <
            typename RandomAccessRange,
            typename Compare,
            RAH2_STD::enable_if_t<random_access_range<RandomAccessRange>>* = nullptr>
        iterator_t<RandomAccessRange>
        nth_element(RandomAccessRange&& range, iterator_t<RandomAccessRange> nth, Compare compare)
        {
            return RAH2_NS::ranges::nth_element(
                RAH2_NS::ranges::begin(range),
                RAH2_STD::move(nth),
                RAH2_NS::ranges::end(range),
                RAH2_STD::move(compare));
        }

        namespace Internal
        {
            template <typename RandomAccessIterator, typename Sentinel, typename Size, typename PivotValueType>
            void quick_sort_impl_helper(RandomAccessIterator first, Sentinel last, Size kRecursionCount)
            {
                using value_type =
                    typename RAH2_STD::iterator_traits<RandomAccessIterator>::value_type;

                while (((last - first) > kQuickSortLimit) && (kRecursionCount > 0))
                {
                    RandomAccessIterator const position(
                        RAH2_NS::ranges::get_partition<RandomAccessIterator, Sentinel, value_type>(
                            first,
                            last,
                            RAH2_STD::forward<PivotValueType>(RAH2_NS::ranges::median<value_type>(
                                RAH2_STD::forward<value_type>(*first),
                                RAH2_STD::forward<value_type>(*(first + (last - first) / 2)),
                                RAH2_STD::forward<value_type>(*(last - 1))))));

                    RAH2_NS::ranges::Internal::
                        quick_sort_impl_helper<RandomAccessIterator, Sentinel, Size, PivotValueType>(
                            position, last, --kRecursionCount);
                    last = position;
                }

                if (kRecursionCount == 0)
                    RAH2_NS::ranges::partial_sort<RandomAccessIterator>(first, last, last);
            }

            template <typename RandomAccessIterator, typename Sentinel, typename Size, typename Compare, typename PivotValueType>
            void quick_sort_impl_helper(
                RandomAccessIterator first, Sentinel last, Size kRecursionCount, Compare compare)
            {
                using value_type =
                    typename RAH2_STD::iterator_traits<RandomAccessIterator>::value_type;

                while (((last - first) > kQuickSortLimit) && (kRecursionCount > 0))
                {
                    RandomAccessIterator const position(
                        RAH2_NS::ranges::get_partition<RandomAccessIterator, Sentinel, value_type, Compare>(
                            first,
                            last,
                            RAH2_STD::forward<PivotValueType>(
                                RAH2_NS::ranges::median<value_type, Compare>(
                                    RAH2_STD::forward<value_type>(*first),
                                    RAH2_STD::forward<value_type>(*(first + (last - first) / 2)),
                                    RAH2_STD::forward<value_type>(*(last - 1)),
                                    compare)),
                            compare));

                    RAH2_NS::ranges::Internal::
                        quick_sort_impl_helper<RandomAccessIterator, Sentinel, Size, Compare, PivotValueType>(
                            position, last, --kRecursionCount, compare);
                    last = position;
                }

                if (kRecursionCount == 0)
                    RAH2_NS::ranges::partial_sort<RandomAccessIterator, Sentinel, Compare>(
                        first, last, last, compare);
            }

            template <typename RandomAccessIterator, typename Sentinel, typename Size>
            void quick_sort_impl(
                RandomAccessIterator first,
                Sentinel last,
                Size kRecursionCount,
                typename RAH2_STD::enable_if<RAH2_STD::is_copy_constructible<
                    typename RAH2_STD::iterator_traits<RandomAccessIterator>::value_type>::value>::type* =
                    nullptr)
            {
                using value_type =
                    typename RAH2_STD::iterator_traits<RandomAccessIterator>::value_type;

                // copy constructors require const value_type
                quick_sort_impl_helper<RandomAccessIterator, Sentinel, Size, value_type const>(
                    first, last, kRecursionCount);
            }

            template <typename RandomAccessIterator, typename Sentinel, typename Size>
            void quick_sort_impl(
                RandomAccessIterator first,
                Sentinel last,
                Size kRecursionCount,
                typename RAH2_STD::enable_if<
                    RAH2_STD::is_move_constructible<
                        typename RAH2_STD::iterator_traits<RandomAccessIterator>::value_type>::value
                    && !RAH2_STD::is_copy_constructible<typename RAH2_STD::iterator_traits<
                        RandomAccessIterator>::value_type>::value>::type* = nullptr)
            {
                using value_type =
                    typename RAH2_STD::iterator_traits<RandomAccessIterator>::value_type;

                // move constructors require non-const value_type
                quick_sort_impl_helper<RandomAccessIterator, Size, value_type>(
                    first, last, kRecursionCount);
            }

            template <typename RandomAccessIterator, typename Sentinel, typename Size, typename Compare>
            void quick_sort_impl(
                RandomAccessIterator first,
                Sentinel last,
                Size kRecursionCount,
                Compare compare,
                typename RAH2_STD::enable_if<RAH2_STD::is_copy_constructible<
                    typename RAH2_STD::iterator_traits<RandomAccessIterator>::value_type>::value>::type* =
                    nullptr)
            {
                using value_type =
                    typename RAH2_STD::iterator_traits<RandomAccessIterator>::value_type;

                // copy constructors require const value_type
                quick_sort_impl_helper<RandomAccessIterator, Sentinel, Size, Compare, value_type const>(
                    first, last, kRecursionCount, compare);
            }

            template <typename RandomAccessIterator, typename Sentinel, typename Size, typename Compare>
            void quick_sort_impl(
                RandomAccessIterator first,
                Sentinel last,
                Size kRecursionCount,
                Compare compare,
                typename RAH2_STD::enable_if<
                    RAH2_STD::is_move_constructible<
                        typename RAH2_STD::iterator_traits<RandomAccessIterator>::value_type>::value
                    && !RAH2_STD::is_copy_constructible<typename RAH2_STD::iterator_traits<
                        RandomAccessIterator>::value_type>::value>::type* = nullptr)
            {
                using value_type =
                    typename RAH2_STD::iterator_traits<RandomAccessIterator>::value_type;

                // move constructors require non-const value_type
                quick_sort_impl_helper<RandomAccessIterator, Size, Compare, value_type>(
                    first, last, kRecursionCount, compare);
            }
        } // namespace Internal

        /// quick_sort
        ///
        /// This is an unstable sort.
        /// quick_sort sorts the elements in [first, last) into ascending order,
        /// meaning that if i and j are any two valid iterators in [first, last)
        /// such that i precedes j, then *j is not less than *i. quick_sort is not
        /// guaranteed to be stable. That is, suppose that *i and *j are equivalent:
        /// neither one is less than the other. It is not guaranteed that the
        /// relative order of these two elements will be preserved by sort.
        ///
        /// We implement the "introspective" variation of quick-sort. This is
        /// considered to be the best general-purpose variant, as it avoids
        /// worst-case behaviour and optimizes the final sorting stage by
        /// switching to an insertion sort.
        ///
        template <
            typename RandomAccessIterator,
            typename Sentinel,
            RAH2_STD::enable_if_t<
                random_access_iterator<RandomAccessIterator>
                && sentinel_for<Sentinel, RandomAccessIterator>>* = nullptr>
        void quick_sort(RandomAccessIterator first, Sentinel last)
        {
            using difference_type =
                typename RAH2_STD::iterator_traits<RandomAccessIterator>::difference_type;

            if (first != last)
            {
                RAH2_NS::ranges::Internal::quick_sort_impl<RandomAccessIterator, Sentinel, difference_type>(
                    first, last, 2 * Internal::Log2(last - first));

                if ((last - first) > static_cast<difference_type>(kQuickSortLimit))
                {
                    RAH2_NS::ranges::insertion_sort<RandomAccessIterator, RandomAccessIterator>(
                        first, first + kQuickSortLimit);
                    RAH2_NS::ranges::Internal::insertion_sort_simple<RandomAccessIterator, Sentinel>(
                        first + kQuickSortLimit, last);
                }
                else
                    RAH2_NS::ranges::insertion_sort<RandomAccessIterator, Sentinel>(first, last);
            }
        }

        template <typename RandomAccessIterator, typename Sentinel, typename Compare>
        void quick_sort(RandomAccessIterator first, Sentinel last, Compare compare)
        {
            using difference_type =
                typename RAH2_STD::iterator_traits<RandomAccessIterator>::difference_type;

            if (first != last)
            {
                RAH2_NS::ranges::Internal::
                    quick_sort_impl<RandomAccessIterator, Sentinel, difference_type, Compare>(
                        first, last, 2 * Internal::Log2(last - first), compare);

                if ((last - first) > static_cast<difference_type>(kQuickSortLimit))
                {
                    RAH2_NS::ranges::insertion_sort<RandomAccessIterator, RandomAccessIterator, Compare>(
                        first, first + kQuickSortLimit, compare);
                    RAH2_NS::ranges::Internal::insertion_sort_simple<RandomAccessIterator, Sentinel, Compare>(
                        first + kQuickSortLimit, last, compare);
                }
                else
                    RAH2_NS::ranges::insertion_sort<RandomAccessIterator, Sentinel, Compare>(
                        first, last, compare);
            }
        }

        /// sort
        ///
        /// We use quick_sort by default. See quick_sort for details.
        ///
        template <
            typename RandomAccessIterator,
            typename Sentinel,
            RAH2_STD::enable_if_t<
                random_access_iterator<RandomAccessIterator>
                && sentinel_for<Sentinel, RandomAccessIterator>>* = nullptr>
        void sort(RandomAccessIterator first, Sentinel last)
        {
            RAH2_NS::ranges::quick_sort(first, last);
        }
        template <typename RandomAccessRange>
        void sort(RandomAccessRange&& range)
        {
            sort(RAH2_NS::ranges::begin(range), RAH2_NS::ranges::end(range));
        }

        template <typename RandomAccessIterator, typename Sentinel, typename Compare>
        void sort(RandomAccessIterator first, Sentinel last, Compare compare)
        {
            RAH2_NS::ranges::quick_sort<RandomAccessIterator, Sentinel, Compare>(
                first, last, compare);
        }

        template <
            typename RandomAccessRange,
            typename Compare,
            RAH2_STD::enable_if_t<random_access_range<RandomAccessRange>>* = nullptr>
        void sort(RandomAccessRange&& range, Compare compare)
        {
            sort(RAH2_NS::ranges::begin(range), RAH2_NS::ranges::end(range), RAH2_STD::move(compare));
        }

        /// stable_sort
        ///
        /// We use merge_sort by default. See merge_sort for details.
        /// Beware that the used merge_sort -- and thus stable_sort -- allocates
        /// memory during execution. Try using merge_sort_buffer if you want
        /// to avoid memory allocation.
        ///
        template <typename RandomAccessIterator, typename Sentinel, typename StrictWeakOrdering>
        void stable_sort(RandomAccessIterator first, Sentinel last, StrictWeakOrdering compare)
        {
#ifdef RAH2_USE_EASTL
            RAH2_NS::ranges::merge_sort<RandomAccessIterator, Sentinel, RAHAllocatorType, StrictWeakOrdering>(
                first, last, *RAH2_STD::get_default_allocator(0), compare);
#else
            using Allocator =
                RAH2_STD::allocator<typename RAH2_STD::iterator_traits<RandomAccessIterator>::value_type>;
            Allocator allocator;
            RAH2_NS::ranges::merge_sort<RandomAccessIterator, Sentinel, Allocator, StrictWeakOrdering>(
                first, last, allocator, compare);
#endif
        }

        template <
            typename RandomAccessRange,
            typename StrictWeakOrdering,
            RAH2_STD::enable_if_t<random_access_range<RandomAccessRange>>* = nullptr>
        void stable_sort(RandomAccessRange&& range, StrictWeakOrdering compare)
        {
            RAH2_NS::ranges::stable_sort(
                RAH2_NS::ranges::begin(range), RAH2_NS::ranges::end(range), RAH2_STD::move(compare));
        }

        template <
            typename RandomAccessIterator,
            typename Sentinel,
            RAH2_STD::enable_if_t<
                random_access_iterator<RandomAccessIterator>
                && sentinel_for<Sentinel, RandomAccessIterator>>* = nullptr>
        void stable_sort(RandomAccessIterator first, Sentinel last)
        {
#ifdef RAH2_USE_EASTL
            RAH2_NS::ranges::merge_sort<RandomAccessIterator, Sentinel, EASTLAllocatorType>(
                first, last, *RAH2_STD::get_default_allocator(0));
#else
            using Allocator =
                RAH2_STD::allocator<typename RAH2_STD::iterator_traits<RandomAccessIterator>::value_type>;
            RAH2_NS::ranges::merge_sort<RandomAccessIterator, Allocator>(first, last, Allocator());
#endif
        }

        template <typename RandomAccessRange>
        void stable_sort(RandomAccessRange&& range)
        {
            stable_sort(RAH2_NS::ranges::begin(range), RAH2_NS::ranges::end(range));
        }

        template <typename RandomAccessIterator, typename Sentinel, typename Allocator, typename StrictWeakOrdering>
        void stable_sort(
            RandomAccessIterator first, Sentinel last, Allocator& allocator, StrictWeakOrdering compare)
        {
            RAH2_NS::ranges::merge_sort<RandomAccessIterator, Allocator, StrictWeakOrdering>(
                first, last, allocator, compare);
        }
    } // namespace ranges
} // namespace RAH2_NS
