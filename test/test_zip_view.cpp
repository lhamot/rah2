#include "test_helpers_zip.hpp"

template <CommonOrSent CS, typename Tag, bool Sized>
struct make_zip_view1
{
    auto make()
    {
        return RAH2_NS::views::zip(make_test_view<CS, Tag, Sized>());
    }
    using BaseRange = test_view<CS, Tag, Sized>;
    static constexpr bool is_sized = RAH2_NS::ranges::sized_range<BaseRange>;
    static constexpr bool is_common =
        RAH2_NS::ranges::common_range<
            BaseRange> || (RAH2_NS::ranges::sized_range<BaseRange> && RAH2_NS::ranges::random_access_range<BaseRange>);
    static constexpr bool do_test = true;
    static constexpr bool is_borrowed = RAH2_NS::ranges::enable_borrowed_range<BaseRange>;
    using expected_cat = RAH2_NS::ranges::details::
        cap_iterator_tag<Tag, RAH2_STD::input_iterator_tag, RAH2_NS::random_access_iterator_tag>;
};
struct make_zip_view2
{
    template <CommonOrSent CS, typename Tag, bool Sized, CommonOrSent CS2, typename Tag2, bool Sized2>
    struct impl
    {
        auto make()
        {
            return RAH2_NS::views::zip(
                make_test_view<CS, Tag, Sized>(), make_test_view<CS2, Tag2, Sized2>());
        }
        using BaseRange1 = test_view<CS, Tag, Sized>;
        using BaseRange2 = test_view<CS2, Tag2, Sized2>;
        static constexpr bool is_sized =
            RAH2_NS::ranges::sized_range<BaseRange1> && RAH2_NS::ranges::sized_range<BaseRange2>;
        static constexpr bool is_common =
            (RAH2_NS::ranges::sized_range<BaseRange1> && RAH2_NS::ranges::random_access_range<BaseRange1>)&&(
                RAH2_NS::ranges::sized_range<BaseRange2> && RAH2_NS::ranges::random_access_range<BaseRange2>);
        static constexpr bool do_test = true;
        static constexpr bool is_borrowed =
            RAH2_NS::ranges::enable_borrowed_range<
                BaseRange1> && RAH2_NS::ranges::enable_borrowed_range<BaseRange2>;
        using expected_cat = RAH2_NS::ranges::details::cap_iterator_tag<
            RAH2_NS::ranges::details::common_iterator_tag<Tag, Tag2>,
            RAH2_STD::input_iterator_tag,
            RAH2_NS::random_access_iterator_tag>;
    };
};
void test_zip_view()
{
    {
        testSuite.test_case("sample");
        /// [zip]
        RAH2_STD::vector<int> inputA{1, 2, 3, 4};
        RAH2_STD::vector<double> inputB{2.5, 4.5, 6.5, 8.5};
        RAH2_STD::vector<char> inputC{'a', 'b', 'c', 'd', 'e', 'f', 'g'};
        RAH2_STD::vector<RAH2_STD::tuple<int, double, char>> result;
        for (auto&& a_b_c : RAH2_NS::views::zip(inputA, inputB, inputC) | RAH2_NS::views::common)
            result.emplace_back(a_b_c);
        assert(
            result
            == (RAH2_STD::vector<RAH2_STD::tuple<int, double, char>>{
                {1, 2.5, 'a'}, {2, 4.5, 'b'}, {3, 6.5, 'c'}, {4, 8.5, 'd'}}));
        /// [zip]
    }

    {
        testSuite.test_case("various");
        RAH2_STD::vector<int> inputA{1, 2, 3, 4};
        RAH2_STD::vector<uint8_t> inputB{false, true, true, false};
        auto range = RAH2_NS::views::zip(inputA, inputB)
                     | RAH2_NS::views::filter([](auto a_b) { return RAH2_STD::get<1>(a_b); });
        RAH2_STD::vector<RAH2_STD::tuple<int, uint8_t>> result;
        auto b = RAH2_NS::ranges::begin(range);
        auto e = RAH2_NS::ranges::end(range);
        auto o = RAH2_NS::back_inserter(result);
        for (; b != e; ++b, ++o)
        {
            *o = *b;
        }
        // RAH2_NS::ranges::copy(range, RAH2_NS::back_inserter(result));
        assert(RAH2_NS::ranges::equal(
            result, RAH2_STD::vector<RAH2_STD::tuple<int, uint8_t>>({{2, true}, {3, true}})));
    }

    testSuite.test_case("concepts");
    foreach_range_combination<test_range<make_zip_view1>>();
    foreach_range_combination2<test_2_inputs_adaptor<make_zip_view2>>();
}
