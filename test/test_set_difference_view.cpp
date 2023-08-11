#include "test_helpers.hpp"

#ifdef RAH2_USE_EASTL

#include <EASTL/algorithm.h>

#else

#include <algorithm>

#endif

template <CommonOrSent CS, typename Tag, bool Sized>
struct make_set_difference_view
{
    template <CommonOrSent CS2, typename Tag2, bool Sized2>
    struct type
    {
        using BaseRange1 = test_view<CS, Tag, Sized>;
        using BaseRange2 = test_view<CS2, Tag2, Sized2>;
        auto make()
        {
            return RAH2_NS::views::set_difference(
                make_test_view<CS, Tag, Sized>(), make_test_view<CS2, Tag2, Sized2>());
        }
        static constexpr bool is_sized = false;
        static constexpr bool is_common = false;
        static constexpr bool do_test = true;
        static constexpr bool is_borrowed = false;
        using expected_cat = RAH2_NS::ranges::cap_iterator_tag<
            RAH2_NS::ranges::common_iterator_tag<Tag, Tag2>,
            RAH2_STD::forward_iterator_tag,
            RAH2_NS::forward_iterator_tag>;
    };
};
void test_set_difference_view()
{
    {
        testSuite.test_case("sample");
        /// [views::set_difference]
        RAH2_STD::vector<int> in1 = {1, 2, 3, 4, 5, 6};
        RAH2_STD::vector<int> in2 = {2, 4, 6, 7, 8, 9, 10};
        RAH2_STD::vector<int> out;
        for (int val : RAH2_NS::views::set_difference(in1, in2) | RAH2_NS::views::common)
            out.push_back(val);
        assert(out == RAH2_STD::vector<int>({1, 3, 5}));
        /// [views::set_difference]
    }

    {
        /// views::set_difference
        auto test_set_difference = [](RAH2_STD::vector<int> const& in1,
                                      RAH2_STD::vector<int> const& in2,
                                      RAH2_STD::vector<int> const& expected)
        {
            RAH2_STD::vector<int> out;
            for (int const val : RAH2_NS::views::set_difference(in1, in2) | RAH2_NS::views::common)
                out.push_back(val);
            assert(out == expected);
        };

        test_set_difference({}, {2, 4, 6, 7, 8, 9, 10}, {});
        test_set_difference({1, 2, 3, 4, 5, 6}, {}, {1, 2, 3, 4, 5, 6});
        test_set_difference({1, 2, 3, 4, 5, 6, 7}, {2, 4, 6}, {1, 3, 5, 7});
        test_set_difference({1, 2, 4, 6}, {3, 5, 7}, {1, 2, 4, 6});
        test_set_difference({1, 2, 4, 6}, {1, 2, 4, 6}, {});
        test_set_difference({1, 2, 4, 6, 7, 8, 9}, {1, 2, 4, 6}, {7, 8, 9});

        for (int x = 0; x < 100; ++x)
        {
            RAH2_STD::vector<int> in1;
            RAH2_STD::vector<int> in2;
            auto const size1 = static_cast<size_t>(rand() % 100);
            auto const size2 = static_cast<size_t>(rand() % 100);
            for (size_t i = 0; i < size1; ++i)
                in1.push_back(rand() % 100);
            for (size_t i = 0; i < size2; ++i)
                in2.push_back(rand() % 100);
            RAH2_NS::ranges::sort(in1);
            RAH2_NS::ranges::sort(in2);
            RAH2_STD::vector<int> outRef;
            RAH2_STD::set_difference(
                begin(in1), end(in1), begin(in2), end(in2), RAH2_STD::back_inserter(outRef));
            RAH2_STD::vector<int> out;
            for (int val : in1 | RAH2_NS::views::set_difference(in2) | RAH2_NS::views::common)
                out.push_back(val);
            assert(out == outRef);
        }
    }

    testSuite.test_case("concepts");
    foreach_range_combination<test_range2<make_set_difference_view>>();
}
