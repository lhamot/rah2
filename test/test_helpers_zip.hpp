#pragma once

#include "test_helpers.hpp"

template <class Func>
void foreach_range_combination2()
{
#if TEST_LEVEL == 0
    Func{}.template call<Common, RAH2_NS::random_access_iterator_tag, true, Common, RAH2_NS::random_access_iterator_tag, true>();

#elif TEST_LEVEL == 1
    Func{}.template call<Sentinel, RAH2_STD::input_iterator_tag, false, Sentinel, RAH2_STD::input_iterator_tag, false>();
    Func{}.template call<Sentinel, RAH2_STD::forward_iterator_tag, false, Sentinel, RAH2_STD::forward_iterator_tag, false>();
    Func{}
        .template call<
            Sentinel,
            RAH2_NS::bidirectional_iterator_tag,
            false,
            Sentinel,
            RAH2_NS::bidirectional_iterator_tag,
            false>();
    Func{}
        .template call<
            Sentinel,
            RAH2_NS::random_access_iterator_tag,
            false,
            Sentinel,
            RAH2_NS::random_access_iterator_tag,
            false>();
    Func{}.template call<Sentinel, RAH2_NS::contiguous_iterator_tag, false, Sentinel, RAH2_NS::contiguous_iterator_tag, false>();
    Func{}.template call<Common, RAH2_NS::forward_iterator_tag, false, Common, RAH2_NS::forward_iterator_tag, false>();
    Func{}.template call<Common, RAH2_NS::bidirectional_iterator_tag, false, Common, RAH2_NS::bidirectional_iterator_tag, false>();
    Func{}.template call<Sentinel, RAH2_NS::input_iterator_tag, true, Sentinel, RAH2_NS::input_iterator_tag, true>();
    Func{}.template call<Sentinel, RAH2_NS::forward_iterator_tag, true, Sentinel, RAH2_NS::forward_iterator_tag, true>();
    Func{}
        .template call<
            Sentinel,
            RAH2_NS::bidirectional_iterator_tag,
            true,
            Sentinel,
            RAH2_NS::bidirectional_iterator_tag,
            true>();
    Func{}
        .template call<
            Sentinel,
            RAH2_NS::random_access_iterator_tag,
            true,
            Sentinel,
            RAH2_NS::random_access_iterator_tag,
            true>();
    Func{}.template call<Sentinel, RAH2_NS::contiguous_iterator_tag, true, Sentinel, RAH2_NS::contiguous_iterator_tag, true>();
    Func{}.template call<Common, RAH2_NS::forward_iterator_tag, true, Common, RAH2_NS::forward_iterator_tag, true>();
    Func{}.template call<Common, RAH2_NS::bidirectional_iterator_tag, true, Common, RAH2_NS::bidirectional_iterator_tag, true>();
    Func{}.template call<Common, RAH2_NS::random_access_iterator_tag, true, Common, RAH2_NS::random_access_iterator_tag, true>();
    Func{}.template call<Common, RAH2_NS::contiguous_iterator_tag, true, Common, RAH2_NS::contiguous_iterator_tag, true>();

#else
    // The MSVC compiler take a LOT of memory to build all range combinations so, with MSVC
    // we do not test a combination (B, A) if (A, B) as already been tested.
    // It is tested with g++ and clang anyway.
#ifdef _MSC_VER
#define RANGE1 Sentinel, RAH2_NS::input_iterator_tag, false
    Func{}.template call<RANGE1, Sentinel, RAH2_STD::input_iterator_tag, false>();
    Func{}.template call<RANGE1, Sentinel, RAH2_STD::forward_iterator_tag, false>();
    Func{}.template call<RANGE1, Sentinel, RAH2_NS::bidirectional_iterator_tag, false>();
    Func{}.template call<RANGE1, Sentinel, RAH2_NS::random_access_iterator_tag, false>();
    Func{}.template call<RANGE1, Sentinel, RAH2_NS::contiguous_iterator_tag, false>();
    Func{}.template call<RANGE1, Common, RAH2_NS::forward_iterator_tag, false>();
    Func{}.template call<RANGE1, Common, RAH2_NS::bidirectional_iterator_tag, false>();
    Func{}.template call<RANGE1, Sentinel, RAH2_NS::input_iterator_tag, true>();
    Func{}.template call<RANGE1, Sentinel, RAH2_NS::forward_iterator_tag, true>();
    Func{}.template call<RANGE1, Sentinel, RAH2_NS::bidirectional_iterator_tag, true>();
    Func{}.template call<RANGE1, Sentinel, RAH2_NS::random_access_iterator_tag, true>();
    Func{}.template call<RANGE1, Sentinel, RAH2_NS::contiguous_iterator_tag, true>();
    Func{}.template call<RANGE1, Common, RAH2_NS::forward_iterator_tag, true>();
    Func{}.template call<RANGE1, Common, RAH2_NS::bidirectional_iterator_tag, true>();
    Func{}.template call<RANGE1, Common, RAH2_NS::random_access_iterator_tag, true>();
    Func{}.template call<RANGE1, Common, RAH2_NS::contiguous_iterator_tag, true>();

#undef RANGE1
#define RANGE1 Sentinel, RAH2_NS::forward_iterator_tag, false
    Func{}.template call<RANGE1, Sentinel, RAH2_STD::forward_iterator_tag, false>();
    Func{}.template call<RANGE1, Sentinel, RAH2_NS::bidirectional_iterator_tag, false>();
    Func{}.template call<RANGE1, Sentinel, RAH2_NS::random_access_iterator_tag, false>();
    Func{}.template call<RANGE1, Sentinel, RAH2_NS::contiguous_iterator_tag, false>();
    Func{}.template call<RANGE1, Common, RAH2_NS::forward_iterator_tag, false>();
    Func{}.template call<RANGE1, Common, RAH2_NS::bidirectional_iterator_tag, false>();
    Func{}.template call<RANGE1, Sentinel, RAH2_NS::input_iterator_tag, true>();
    Func{}.template call<RANGE1, Sentinel, RAH2_NS::forward_iterator_tag, true>();
    Func{}.template call<RANGE1, Sentinel, RAH2_NS::bidirectional_iterator_tag, true>();
    Func{}.template call<RANGE1, Sentinel, RAH2_NS::random_access_iterator_tag, true>();
    Func{}.template call<RANGE1, Sentinel, RAH2_NS::contiguous_iterator_tag, true>();
    Func{}.template call<RANGE1, Common, RAH2_NS::forward_iterator_tag, true>();
    Func{}.template call<RANGE1, Common, RAH2_NS::bidirectional_iterator_tag, true>();
    Func{}.template call<RANGE1, Common, RAH2_NS::random_access_iterator_tag, true>();
    Func{}.template call<RANGE1, Common, RAH2_NS::contiguous_iterator_tag, true>();

#undef RANGE1
#define RANGE1 Sentinel, RAH2_NS::bidirectional_iterator_tag, false
    Func{}.template call<RANGE1, Sentinel, RAH2_NS::bidirectional_iterator_tag, false>();
    Func{}.template call<RANGE1, Sentinel, RAH2_NS::random_access_iterator_tag, false>();
    Func{}.template call<RANGE1, Sentinel, RAH2_NS::contiguous_iterator_tag, false>();
    Func{}.template call<RANGE1, Common, RAH2_NS::forward_iterator_tag, false>();
    Func{}.template call<RANGE1, Common, RAH2_NS::bidirectional_iterator_tag, false>();
    Func{}.template call<RANGE1, Sentinel, RAH2_NS::input_iterator_tag, true>();
    Func{}.template call<RANGE1, Sentinel, RAH2_NS::forward_iterator_tag, true>();
    Func{}.template call<RANGE1, Sentinel, RAH2_NS::bidirectional_iterator_tag, true>();
    Func{}.template call<RANGE1, Sentinel, RAH2_NS::random_access_iterator_tag, true>();
    Func{}.template call<RANGE1, Sentinel, RAH2_NS::contiguous_iterator_tag, true>();
    Func{}.template call<RANGE1, Common, RAH2_NS::forward_iterator_tag, true>();
    Func{}.template call<RANGE1, Common, RAH2_NS::bidirectional_iterator_tag, true>();
    Func{}.template call<RANGE1, Common, RAH2_NS::random_access_iterator_tag, true>();
    Func{}.template call<RANGE1, Common, RAH2_NS::contiguous_iterator_tag, true>();

#undef RANGE1
#define RANGE1 Sentinel, RAH2_NS::random_access_iterator_tag, false
    Func{}.template call<RANGE1, Sentinel, RAH2_NS::random_access_iterator_tag, false>();
    Func{}.template call<RANGE1, Sentinel, RAH2_NS::contiguous_iterator_tag, false>();
    Func{}.template call<RANGE1, Common, RAH2_NS::forward_iterator_tag, false>();
    Func{}.template call<RANGE1, Common, RAH2_NS::bidirectional_iterator_tag, false>();
    Func{}.template call<RANGE1, Sentinel, RAH2_NS::input_iterator_tag, true>();
    Func{}.template call<RANGE1, Sentinel, RAH2_NS::forward_iterator_tag, true>();
    Func{}.template call<RANGE1, Sentinel, RAH2_NS::bidirectional_iterator_tag, true>();
    Func{}.template call<RANGE1, Sentinel, RAH2_NS::random_access_iterator_tag, true>();
    Func{}.template call<RANGE1, Sentinel, RAH2_NS::contiguous_iterator_tag, true>();
    Func{}.template call<RANGE1, Common, RAH2_NS::forward_iterator_tag, true>();
    Func{}.template call<RANGE1, Common, RAH2_NS::bidirectional_iterator_tag, true>();
    Func{}.template call<RANGE1, Common, RAH2_NS::random_access_iterator_tag, true>();
    Func{}.template call<RANGE1, Common, RAH2_NS::contiguous_iterator_tag, true>();

#undef RANGE1
#define RANGE1 Sentinel, RAH2_NS::contiguous_iterator_tag, false
    Func{}.template call<RANGE1, Sentinel, RAH2_NS::contiguous_iterator_tag, false>();
    Func{}.template call<RANGE1, Common, RAH2_NS::forward_iterator_tag, false>();
    Func{}.template call<RANGE1, Common, RAH2_NS::bidirectional_iterator_tag, false>();
    Func{}.template call<RANGE1, Sentinel, RAH2_NS::input_iterator_tag, true>();
    Func{}.template call<RANGE1, Sentinel, RAH2_NS::forward_iterator_tag, true>();
    Func{}.template call<RANGE1, Sentinel, RAH2_NS::bidirectional_iterator_tag, true>();
    Func{}.template call<RANGE1, Sentinel, RAH2_NS::random_access_iterator_tag, true>();
    Func{}.template call<RANGE1, Sentinel, RAH2_NS::contiguous_iterator_tag, true>();
    Func{}.template call<RANGE1, Common, RAH2_NS::forward_iterator_tag, true>();
    Func{}.template call<RANGE1, Common, RAH2_NS::bidirectional_iterator_tag, true>();
    Func{}.template call<RANGE1, Common, RAH2_NS::random_access_iterator_tag, true>();
    Func{}.template call<RANGE1, Common, RAH2_NS::contiguous_iterator_tag, true>();

#undef RANGE1
#define RANGE1 Common, RAH2_NS::forward_iterator_tag, false
    Func{}.template call<RANGE1, Common, RAH2_NS::forward_iterator_tag, false>();
    Func{}.template call<RANGE1, Common, RAH2_NS::bidirectional_iterator_tag, false>();
    Func{}.template call<RANGE1, Sentinel, RAH2_NS::input_iterator_tag, true>();
    Func{}.template call<RANGE1, Sentinel, RAH2_NS::forward_iterator_tag, true>();
    Func{}.template call<RANGE1, Sentinel, RAH2_NS::bidirectional_iterator_tag, true>();
    Func{}.template call<RANGE1, Sentinel, RAH2_NS::random_access_iterator_tag, true>();
    Func{}.template call<RANGE1, Sentinel, RAH2_NS::contiguous_iterator_tag, true>();
    Func{}.template call<RANGE1, Common, RAH2_NS::forward_iterator_tag, true>();
    Func{}.template call<RANGE1, Common, RAH2_NS::bidirectional_iterator_tag, true>();
    Func{}.template call<RANGE1, Common, RAH2_NS::random_access_iterator_tag, true>();
    Func{}.template call<RANGE1, Common, RAH2_NS::contiguous_iterator_tag, true>();

#undef RANGE1
#define RANGE1 Common, RAH2_NS::bidirectional_iterator_tag, false
    Func{}.template call<RANGE1, Common, RAH2_NS::bidirectional_iterator_tag, false>();
    Func{}.template call<RANGE1, Sentinel, RAH2_NS::input_iterator_tag, true>();
    Func{}.template call<RANGE1, Sentinel, RAH2_NS::forward_iterator_tag, true>();
    Func{}.template call<RANGE1, Sentinel, RAH2_NS::bidirectional_iterator_tag, true>();
    Func{}.template call<RANGE1, Sentinel, RAH2_NS::random_access_iterator_tag, true>();
    Func{}.template call<RANGE1, Sentinel, RAH2_NS::contiguous_iterator_tag, true>();
    Func{}.template call<RANGE1, Common, RAH2_NS::forward_iterator_tag, true>();
    Func{}.template call<RANGE1, Common, RAH2_NS::bidirectional_iterator_tag, true>();
    Func{}.template call<RANGE1, Common, RAH2_NS::random_access_iterator_tag, true>();
    Func{}.template call<RANGE1, Common, RAH2_NS::contiguous_iterator_tag, true>();

#undef RANGE1
#define RANGE1 Sentinel, RAH2_NS::input_iterator_tag, true
    Func{}.template call<RANGE1, Sentinel, RAH2_NS::input_iterator_tag, true>();
    Func{}.template call<RANGE1, Sentinel, RAH2_NS::forward_iterator_tag, true>();
    Func{}.template call<RANGE1, Sentinel, RAH2_NS::bidirectional_iterator_tag, true>();
    Func{}.template call<RANGE1, Sentinel, RAH2_NS::random_access_iterator_tag, true>();
    Func{}.template call<RANGE1, Sentinel, RAH2_NS::contiguous_iterator_tag, true>();
    Func{}.template call<RANGE1, Common, RAH2_NS::forward_iterator_tag, true>();
    Func{}.template call<RANGE1, Common, RAH2_NS::bidirectional_iterator_tag, true>();
    Func{}.template call<RANGE1, Common, RAH2_NS::random_access_iterator_tag, true>();
    Func{}.template call<RANGE1, Common, RAH2_NS::contiguous_iterator_tag, true>();

#undef RANGE1
#define RANGE1 Sentinel, RAH2_NS::forward_iterator_tag, true
    Func{}.template call<RANGE1, Sentinel, RAH2_NS::forward_iterator_tag, true>();
    Func{}.template call<RANGE1, Sentinel, RAH2_NS::bidirectional_iterator_tag, true>();
    Func{}.template call<RANGE1, Sentinel, RAH2_NS::random_access_iterator_tag, true>();
    Func{}.template call<RANGE1, Sentinel, RAH2_NS::contiguous_iterator_tag, true>();
    Func{}.template call<RANGE1, Common, RAH2_NS::forward_iterator_tag, true>();
    Func{}.template call<RANGE1, Common, RAH2_NS::bidirectional_iterator_tag, true>();
    Func{}.template call<RANGE1, Common, RAH2_NS::random_access_iterator_tag, true>();
    Func{}.template call<RANGE1, Common, RAH2_NS::contiguous_iterator_tag, true>();

#undef RANGE1
#define RANGE1 Sentinel, RAH2_NS::bidirectional_iterator_tag, true
    Func{}.template call<RANGE1, Sentinel, RAH2_NS::bidirectional_iterator_tag, true>();
    Func{}.template call<RANGE1, Sentinel, RAH2_NS::random_access_iterator_tag, true>();
    Func{}.template call<RANGE1, Sentinel, RAH2_NS::contiguous_iterator_tag, true>();
    Func{}.template call<RANGE1, Common, RAH2_NS::forward_iterator_tag, true>();
    Func{}.template call<RANGE1, Common, RAH2_NS::bidirectional_iterator_tag, true>();
    Func{}.template call<RANGE1, Common, RAH2_NS::random_access_iterator_tag, true>();
    Func{}.template call<RANGE1, Common, RAH2_NS::contiguous_iterator_tag, true>();

#undef RANGE1
#define RANGE1 Sentinel, RAH2_NS::random_access_iterator_tag, true
    Func{}.template call<RANGE1, Sentinel, RAH2_NS::random_access_iterator_tag, true>();
    Func{}.template call<RANGE1, Sentinel, RAH2_NS::contiguous_iterator_tag, true>();
    Func{}.template call<RANGE1, Common, RAH2_NS::forward_iterator_tag, true>();
    Func{}.template call<RANGE1, Common, RAH2_NS::bidirectional_iterator_tag, true>();
    Func{}.template call<RANGE1, Common, RAH2_NS::random_access_iterator_tag, true>();
    Func{}.template call<RANGE1, Common, RAH2_NS::contiguous_iterator_tag, true>();

#undef RANGE1
#define RANGE1 Sentinel, RAH2_NS::contiguous_iterator_tag, true
    Func{}.template call<RANGE1, Sentinel, RAH2_NS::contiguous_iterator_tag, true>();
    Func{}.template call<RANGE1, Common, RAH2_NS::forward_iterator_tag, true>();
    Func{}.template call<RANGE1, Common, RAH2_NS::bidirectional_iterator_tag, true>();
    Func{}.template call<RANGE1, Common, RAH2_NS::random_access_iterator_tag, true>();
    Func{}.template call<RANGE1, Common, RAH2_NS::contiguous_iterator_tag, true>();

#undef RANGE1
#define RANGE1 Common, RAH2_NS::forward_iterator_tag, true
    Func{}.template call<RANGE1, Common, RAH2_NS::forward_iterator_tag, true>();
    Func{}.template call<RANGE1, Common, RAH2_NS::bidirectional_iterator_tag, true>();
    Func{}.template call<RANGE1, Common, RAH2_NS::random_access_iterator_tag, true>();
    Func{}.template call<RANGE1, Common, RAH2_NS::contiguous_iterator_tag, true>();

#undef RANGE1
#define RANGE1 Common, RAH2_NS::bidirectional_iterator_tag, true
    Func{}.template call<RANGE1, Common, RAH2_NS::bidirectional_iterator_tag, true>();
    Func{}.template call<RANGE1, Common, RAH2_NS::random_access_iterator_tag, true>();
    Func{}.template call<RANGE1, Common, RAH2_NS::contiguous_iterator_tag, true>();

#undef RANGE1
#define RANGE1 Common, RAH2_NS::random_access_iterator_tag, true
    Func{}.template call<RANGE1, Common, RAH2_NS::random_access_iterator_tag, true>();
    Func{}.template call<RANGE1, Common, RAH2_NS::contiguous_iterator_tag, true>();

#undef RANGE1
#define RANGE1 Common, RAH2_NS::contiguous_iterator_tag, true
    Func{}.template call<RANGE1, Common, RAH2_NS::contiguous_iterator_tag, true>();
#else

#define TEST_ALL_RANGE_TYPES(SENT, CAT, SIZED)                                                      \
    Func{}.template call<SENT, CAT, SIZED, Sentinel, RAH2_STD::input_iterator_tag, false>();        \
    Func{}.template call<SENT, CAT, SIZED, Sentinel, RAH2_STD::forward_iterator_tag, false>();      \
    Func{}.template call<SENT, CAT, SIZED, Sentinel, RAH2_NS::bidirectional_iterator_tag, false>(); \
    Func{}.template call<SENT, CAT, SIZED, Sentinel, RAH2_NS::random_access_iterator_tag, false>(); \
    Func{}.template call<SENT, CAT, SIZED, Sentinel, RAH2_NS::contiguous_iterator_tag, false>();    \
    Func{}.template call<SENT, CAT, SIZED, Common, RAH2_NS::forward_iterator_tag, false>();         \
    Func{}.template call<SENT, CAT, SIZED, Common, RAH2_NS::bidirectional_iterator_tag, false>();   \
    Func{}.template call<SENT, CAT, SIZED, Sentinel, RAH2_NS::input_iterator_tag, true>();          \
    Func{}.template call<SENT, CAT, SIZED, Sentinel, RAH2_NS::forward_iterator_tag, true>();        \
    Func{}.template call<SENT, CAT, SIZED, Sentinel, RAH2_NS::bidirectional_iterator_tag, true>();  \
    Func{}.template call<SENT, CAT, SIZED, Sentinel, RAH2_NS::random_access_iterator_tag, true>();  \
    Func{}.template call<SENT, CAT, SIZED, Sentinel, RAH2_NS::contiguous_iterator_tag, true>();     \
    Func{}.template call<SENT, CAT, SIZED, Common, RAH2_NS::forward_iterator_tag, true>();          \
    Func{}.template call<SENT, CAT, SIZED, Common, RAH2_NS::bidirectional_iterator_tag, true>();    \
    Func{}.template call<SENT, CAT, SIZED, Common, RAH2_NS::random_access_iterator_tag, true>();    \
    Func{}.template call<SENT, CAT, SIZED, Common, RAH2_NS::contiguous_iterator_tag, true>();

    TEST_ALL_RANGE_TYPES(Sentinel, RAH2_NS::input_iterator_tag, false)
    TEST_ALL_RANGE_TYPES(Sentinel, RAH2_NS::forward_iterator_tag, false)
    TEST_ALL_RANGE_TYPES(Sentinel, RAH2_NS::bidirectional_iterator_tag, false)
    TEST_ALL_RANGE_TYPES(Sentinel, RAH2_NS::random_access_iterator_tag, false)
    TEST_ALL_RANGE_TYPES(Sentinel, RAH2_NS::contiguous_iterator_tag, false)

    TEST_ALL_RANGE_TYPES(Common, RAH2_NS::forward_iterator_tag, false)
    TEST_ALL_RANGE_TYPES(Common, RAH2_NS::bidirectional_iterator_tag, false)

    TEST_ALL_RANGE_TYPES(Sentinel, RAH2_NS::input_iterator_tag, true)
    TEST_ALL_RANGE_TYPES(Sentinel, RAH2_NS::forward_iterator_tag, true)
    TEST_ALL_RANGE_TYPES(Sentinel, RAH2_NS::bidirectional_iterator_tag, true)
    TEST_ALL_RANGE_TYPES(Sentinel, RAH2_NS::random_access_iterator_tag, true)
    TEST_ALL_RANGE_TYPES(Sentinel, RAH2_NS::contiguous_iterator_tag, true)

    TEST_ALL_RANGE_TYPES(Common, RAH2_NS::forward_iterator_tag, true)
    TEST_ALL_RANGE_TYPES(Common, RAH2_NS::bidirectional_iterator_tag, true)
    TEST_ALL_RANGE_TYPES(Common, RAH2_NS::random_access_iterator_tag, true)
    TEST_ALL_RANGE_TYPES(Common, RAH2_NS::contiguous_iterator_tag, true)
#endif

#endif
}

template <class MakeRange>
struct test_2_inputs_adaptor
{
    template <CommonOrSent Sentinel, typename Cat, bool Sized, CommonOrSent Sentinel2, typename Cat2, bool Sized2>
    void call()
    {
        using MakeR =
            typename MakeRange::template impl<Sentinel, Cat, Sized, Sentinel2, Cat2, Sized2>;
        auto t1 = MakeR();
        auto r1 = t1.make();
        using ExpectedCat = typename MakeR::expected_cat;
        check_range_cat<ExpectedCat, RAH2_STD::remove_reference_t<decltype(r1)>>::test(r1);
        AssertEqual<RAH2_NS::ranges::common_range<decltype(r1)>, t1.is_common>();
        AssertEqual<RAH2_NS::ranges::sized_range<decltype(r1)>, t1.is_sized>();
        AssertEqual<RAH2_NS::ranges::borrowed_range<decltype(r1)>, t1.is_borrowed>();
    }
};
