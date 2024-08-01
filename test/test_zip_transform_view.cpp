#include "test_helpers_zip.hpp"

#include <sstream>

template <CommonOrSent CS, typename Tag, bool Sized>
struct make_zip_transform_view1
{
    auto make()
    {
        return RAH2_NS::views::zip_transform(
            [](auto a) { return a + 1; }, make_test_view<CS, Tag, Sized>());
    }
    using BaseRange = test_view<CS, Tag, Sized>;
    static constexpr bool is_sized = RAH2_NS::ranges::sized_range<BaseRange>;
    static constexpr bool is_common = RAH2_NS::ranges::common_range<BaseRange>
                                      || (RAH2_NS::ranges::sized_range<BaseRange>
                                          && RAH2_NS::ranges::random_access_range<BaseRange>);
    static constexpr bool do_test = true;
    static constexpr bool is_borrowed = RAH2_NS::ranges::enable_borrowed_range<BaseRange>;
    using expected_cat = RAH2_NS::ranges::details::
        cap_iterator_tag<Tag, RAH2_STD::input_iterator_tag, RAH2_NS::random_access_iterator_tag>;
};
struct make_zip_transform_view2
{
    template <CommonOrSent CS, typename Tag, bool Sized, CommonOrSent CS2, typename Tag2, bool Sized2>
    struct impl
    {
        auto make()
        {
            return RAH2_NS::views::zip_transform(
                [](auto a, auto b) { return a + b; },
                make_test_view<CS, Tag, Sized>(),
                make_test_view<CS2, Tag2, Sized2>());
        }
        using BaseRange1 = test_view<CS, Tag, Sized>;
        using BaseRange2 = test_view<CS2, Tag2, Sized2>;
        static constexpr bool is_sized =
            RAH2_NS::ranges::sized_range<BaseRange1> && RAH2_NS::ranges::sized_range<BaseRange2>;
        static constexpr bool is_common = (RAH2_NS::ranges::sized_range<BaseRange1>
                                           && RAH2_NS::ranges::random_access_range<BaseRange1>)
                                          && (RAH2_NS::ranges::sized_range<BaseRange2>
                                              && RAH2_NS::ranges::random_access_range<BaseRange2>);
        static constexpr bool is_borrowed = RAH2_NS::ranges::enable_borrowed_range<BaseRange1>
                                            && RAH2_NS::ranges::enable_borrowed_range<BaseRange2>;
        using expected_cat = RAH2_NS::ranges::details::cap_iterator_tag<
            RAH2_NS::ranges::details::common_iterator_tag<Tag, Tag2>,
            RAH2_STD::input_iterator_tag,
            RAH2_NS::random_access_iterator_tag>;
    };
};

void test_zip_transform_view()
{
    testSuite.test_case("sample");
    /// [zip_transform]
    RAH2_STD::vector<int> inputA{1, 2, 3, 4};
    RAH2_STD::vector<double> inputB{2.5, 4.5, 6.5, 8.5};
    RAH2_STD::vector<char> inputC{'a', 'b', 'c', 'd', 'e', 'f', 'g'};
    RAH2_STD::vector<std::string> result;
    auto func = [](int a, double d, char c)
    {
        std::stringstream ss;
        ss << a << d << c;
        return ss.str();
    };
    for (auto a_b_c :
         RAH2_NS::views::zip_transform(func, inputA, inputB, inputC) | RAH2_NS::views::common)
        result.emplace_back(a_b_c);
    assert(result == (RAH2_STD::vector<std::string>{{"12.5a"}, {"24.5b"}, {"36.5c"}, {"48.5d"}}));
    /// [zip_transform]

    testSuite.test_case("concepts");
    foreach_range_combination<test_range<make_zip_transform_view1>>();
    foreach_range_combination2<test_2_inputs_adaptor<make_zip_transform_view2>>();
}
