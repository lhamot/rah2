#include "test_helpers.hpp"

template <CommonOrSent CS, typename Tag, bool Sized>
struct make_concat_view
{
    template <CommonOrSent CS2, typename Tag2, bool Sized2>
    struct type
    {
        using BaseRange1 = test_view<CS, Tag, Sized>;
        using BaseRange2 = test_view<CS2, Tag2, Sized2>;
        auto make()
        {
            return RAH2_NS::views::concat(
                make_test_view<CS, Tag, Sized>(), make_test_view<CS2, Tag2, Sized2>());
        }
        static constexpr bool is_sized =
            RAH2_NS::ranges::sized_range<BaseRange1> && RAH2_NS::ranges::sized_range<BaseRange2>;
        static constexpr bool is_common = false;
        static constexpr bool do_test = true;
        static constexpr bool is_borrowed = false;
        using expected_cat = RAH2_NS::ranges::common_iterator_tag<
            RAH2_NS::ranges::common_iterator_tag<Tag, Tag2>,
            RAH2_NS::forward_iterator_tag>;
    };
};
void test_concat_view()
{
    {
        testSuite.test_case("sample");
        /// [concat]
        RAH2_STD::vector<int> inputA{0, 1, 2, 3};
        RAH2_STD::vector<int> inputB{4, 5, 6};
        RAH2_STD::vector<int> inputC{7, 8, 9, 10, 11};
        {
            RAH2_STD::vector<int> result;
            for (int i : RAH2_NS::views::concat(inputA))
                result.push_back(i);
            assert(result == RAH2_STD::vector<int>({0, 1, 2, 3}));
        }
        {
            RAH2_STD::vector<int> result;
            for (int i : RAH2_NS::views::concat(inputA, inputB) | RAH2_NS::views::common)
                result.push_back(i);
            assert(result == RAH2_STD::vector<int>({0, 1, 2, 3, 4, 5, 6}));
        }
        {
            RAH2_STD::vector<int> result;
            for (int i : RAH2_NS::views::concat(inputA, inputB, inputC) | RAH2_NS::views::common)
                result.push_back(i);
            assert(result == RAH2_STD::vector<int>({0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11}));
        }
        /// [concat]
    }
    {
        RAH2_STD::vector<int> inputA{};
        RAH2_STD::vector<int> inputB{1, 2, 3, 4};
        {
            RAH2_STD::vector<int> result;
            for (int i : RAH2_NS::views::concat(inputA, inputB) | RAH2_NS::views::common)
                result.push_back(i);
            assert(result == RAH2_STD::vector<int>({1, 2, 3, 4}));
        }
        {
            RAH2_STD::vector<int> result;
            for (int i : RAH2_NS::views::concat(inputA, inputB) | RAH2_NS::views::common)
                result.push_back(i);
            assert(result == RAH2_STD::vector<int>({1, 2, 3, 4}));
        }
        {
            RAH2_STD::vector<int> result;
            for (int i : RAH2_NS::views::concat(inputA, inputA) | RAH2_NS::views::common)
                result.push_back(i);
            assert(result == RAH2_STD::vector<int>({}));
        }
    }

    testSuite.test_case("concepts");
    foreach_range_combination<test_range2<make_concat_view>>();
}
