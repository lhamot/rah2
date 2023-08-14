#pragma once

#include "test_helpers.hpp"

template <class Func>
void foreach_range_combination2()
{
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
}

template <
    bool DoTest,
    CommonOrSent Sentinel,
    typename Cat,
    bool Sized,
    CommonOrSent Sentinel2,
    typename Cat2,
    bool Sized2,
    template <CommonOrSent, typename, bool, CommonOrSent, typename, bool>
    class MakeR,
    typename Check>
struct call_on_range_if_true2
{
    static void test()
    {
        Check{}.template call<Sentinel, Cat, Sized, Sentinel2, Cat2, Sized2, MakeR>();
    }
};

template <
    CommonOrSent Sentinel,
    typename Cat,
    bool Sized,
    CommonOrSent Sentinel2,
    typename Cat2,
    bool Sized2,
    template <CommonOrSent, typename, bool, CommonOrSent, typename, bool>
    class MakeR,
    typename Check>
struct call_on_range_if_true2<false, Sentinel, Cat, Sized, Sentinel2, Cat2, Sized2, MakeR, Check>
{
    static void test()
    {
    }
};

template <template <CommonOrSent, typename, bool, CommonOrSent, typename, bool> class MakeRange>
struct test_zip_range
{
    struct CheckView
    {
        template <
            CommonOrSent Sentinel,
            typename Cat,
            bool Sized,
            CommonOrSent Sentinel2,
            typename Cat2,
            bool Sized2,
            template <CommonOrSent, typename, bool, CommonOrSent, typename, bool>
            class MakeR>
        void call() const
        {
            auto t1 = MakeR<Sentinel, Cat, Sized, Sentinel2, Cat2, Sized2>();
            auto r1 = t1.make();
            using ExpectedCat =
                typename MakeR<Sentinel, Cat, Sized, Sentinel2, Cat2, Sized2>::expected_cat;
            check_range_cat<ExpectedCat, std::remove_reference_t<decltype(r1)>>::test(r1);
            AssertEqual<RAH2_NS::ranges::common_range<decltype(r1)>, t1.is_common>();
            AssertEqual<RAH2_NS::ranges::sized_range<decltype(r1)>, t1.is_sized>();
            AssertEqual<RAH2_NS::ranges::borrowed_range<decltype(r1)>, t1.is_borrowed>();
        }
    };
    template <CommonOrSent Sentinel, typename Cat, bool Sized, CommonOrSent Sentinel2, typename Cat2, bool Sized2>
    void call()
    {
        constexpr bool do_test = MakeRange<Sentinel, Cat, Sized, Sentinel2, Cat2, Sized2>::do_test;
        call_on_range_if_true2<do_test, Sentinel, Cat, Sized, Sentinel2, Cat2, Sized2, MakeRange, CheckView>::
            test();
    }
};

/*
template <template <CommonOrSent, typename, bool, CommonOrSent, typename, bool> class MakeRange>
struct test_zip_range // When the adaptor take two ranges
{
    template <CommonOrSent Sentinel, typename Cat, bool Sized, CommonOrSent, typename, bool>
    void call()
    {
        foreach_range_combination2<test_range<MakeRange<Sentinel, Cat, Sized>::template type>>();
    }
};
*/
