#include <rah2/ranges.hpp>

#include <sstream>

#ifdef RAH2_USE_EASTL

#include <EASTL/array.h>
#include <EASTL/algorithm.h>

#else

#include <array>
#include <algorithm>

#endif

#include "test_helpers.hpp"

template <CommonOrSent CS, typename Tag, bool Sized>
struct make_enumerate_view
{
    auto make()
    {
        return RAH2_NS::views::enumerate(make_test_view<CS, Tag, Sized>());
    }
    using BaseRange = test_view<CS, Tag, Sized>;
    static constexpr bool is_sized = RAH2_NS::ranges::sized_range<BaseRange>;
    static constexpr bool is_common =
        RAH2_NS::ranges::common_range<BaseRange> && RAH2_NS::ranges::sized_range<BaseRange>;
    static constexpr bool do_test = true;
    static constexpr bool is_borrowed = RAH2_NS::ranges::enable_borrowed_range<BaseRange>;
    using expected_cat =
        RAH2_NS::ranges::cap_iterator_tag<Tag, RAH2_STD::input_iterator_tag, RAH2_NS::random_access_iterator_tag>;
};
void test_enumerate_view()
{
    {
        testSuite.test_case("sample");
        /// [enumerate]
        RAH2_STD::vector<int> input{4, 5, 6, 7};
        RAH2_STD::vector<RAH2_STD::tuple<intptr_t, int>> result;
        RAH2_NS::views::enumerate(input);
        for (auto i_value : RAH2_NS::views::enumerate(input))
            result.emplace_back(i_value);
        assert(
            result
            == (RAH2_STD::vector<RAH2_STD::tuple<intptr_t, int>>{{0, 4}, {1, 5}, {2, 6}, {3, 7}}));
        /// [enumerate]
    }
    {
        testSuite.test_case("sample_pipeable");
        /// [enumerate_pipeable]
        RAH2_STD::vector<int> input{4, 5, 6, 7};
        RAH2_STD::vector<RAH2_STD::tuple<intptr_t, int>> result;
        for (auto i_value : input | RAH2_NS::views::enumerate | RAH2_NS::views::common)
            result.emplace_back(i_value);
        assert(
            result
            == (RAH2_STD::vector<RAH2_STD::tuple<intptr_t, int>>{{0, 4}, {1, 5}, {2, 6}, {3, 7}}));
        /// [enumerate_pipeable]
    }

    {
        testSuite.test_case("various");
        // This can't work since enumerate return an rvalue pairs since map_key want an lvalue
        bool bools[] = {false, true, true, false, false, true};
        auto range =
            bools | RAH2_NS::views::enumerate
            | RAH2_NS::views::filter([](auto&& index_bool) { return RAH2_STD::get<1>(index_bool); })
            | RAH2_NS::views::keys;

        RAH2_STD::vector<intptr_t> ref;
        RAH2_NS::ranges::copy(range, RAH2_NS::back_inserter(ref));
        assert(ref == (RAH2_STD::vector<intptr_t>{1, 2, 5}));
    }

    testSuite.test_case("concepts");
    foreach_range_combination<test_range<make_enumerate_view>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct make_adjacent_view
{
    auto make()
    {
        return RAH2_NS::views::adjacent<3>(make_test_view<CS, Tag, Sized>());
    }
    using BaseRange = test_view<CS, Tag, Sized>;
    static constexpr bool is_sized = RAH2_NS::ranges::sized_range<BaseRange>;
    static constexpr bool is_common =
        RAH2_NS::ranges::common_range<BaseRange> && RAH2_NS::ranges::bidirectional_range<BaseRange>;
    static constexpr bool do_test = RAH2_NS::ranges::forward_range<BaseRange>;
    static constexpr bool is_borrowed = RAH2_NS::ranges::enable_borrowed_range<BaseRange>;
    using expected_cat =
        RAH2_NS::ranges::cap_iterator_tag<Tag, RAH2_STD::forward_iterator_tag, RAH2_NS::random_access_iterator_tag>;
};
void test_adjacent_view()
{
    {
        testSuite.test_case("sample");
        /// [adjacent]
        RAH2_STD::vector<int> in{0, 1, 2, 3, 4, 5};
        RAH2_STD::vector<RAH2_STD::vector<int>> out;
        for (auto&& abc : RAH2_NS::views::adjacent<3>(in))
        {
            out.push_back({RAH2_STD::get<0>(abc), RAH2_STD::get<1>(abc), RAH2_STD::get<2>(abc)});
        }
        assert(
            out
            == (RAH2_STD::vector<RAH2_STD::vector<int>>{{0, 1, 2}, {1, 2, 3}, {2, 3, 4}, {3, 4, 5}}));
        /// [adjacent]
    }
    {
        testSuite.test_case("non_common");
        // adjacent With non common_range
        RAH2_STD::vector<RAH2_STD::vector<int>> out;
        for (auto&& abc : RAH2_NS::views::iota(0) | RAH2_NS::views::take(6)
                              | RAH2_NS::views::adjacent<3> | RAH2_NS::views::common)
        {
            out.push_back({RAH2_STD::get<0>(abc), RAH2_STD::get<1>(abc), RAH2_STD::get<2>(abc)});
        }
        assert(
            out
            == (RAH2_STD::vector<RAH2_STD::vector<int>>{{0, 1, 2}, {1, 2, 3}, {2, 3, 4}, {3, 4, 5}}));
    }
    {
        testSuite.test_case("N > size()");
        // adjacent With N > view.size()
        RAH2_STD::vector<RAH2_STD::vector<int>> out;
        for (auto&& abc : RAH2_NS::views::iota(0) | RAH2_NS::views::take(6)
                              | RAH2_NS::views::adjacent<45> | RAH2_NS::views::common)
        {
            out.push_back({RAH2_STD::get<0>(abc), RAH2_STD::get<1>(abc), RAH2_STD::get<2>(abc)});
        }
        assert(out.empty());
    }
    {
        testSuite.test_case("N == 0");
        // adjacent With N == 0
        RAH2_STD::vector<RAH2_STD::vector<int>> out;
        for (auto&& abc :
             RAH2_NS::views::iota(0) | RAH2_NS::views::take(6) | RAH2_NS::views::adjacent<0>)
        {
            static_assert(
                RAH2_STD::tuple_size<RAH2_STD::remove_reference_t<decltype(abc)>>::value == 0,
                "tuple should be empty");
            out.emplace_back();
        }
        assert(out.empty());
    }

    testSuite.test_case("concepts");
    foreach_range_combination<test_range<make_adjacent_view>>();
}

struct Add
{
    auto operator()(int i) const
    {
        return i;
    }
    template <typename... Args>
    auto operator()(int i, Args... ints) const
    {
        return i + (*this)(ints...);
    }
};
template <CommonOrSent CS, typename Tag, bool Sized>
struct make_adjacent_transform_view
{
    auto make()
    {
        return RAH2_NS::views::adjacent_transform<3>(
            make_test_view<CS, Tag, Sized>(), [](auto a, auto b, auto c) { return a + b + c; });
    }
    using BaseRange = test_view<CS, Tag, Sized>;
    static constexpr bool is_sized = RAH2_NS::ranges::sized_range<BaseRange>;
    static constexpr bool is_common =
        RAH2_NS::ranges::common_range<BaseRange> && RAH2_NS::ranges::bidirectional_range<BaseRange>;
    static constexpr bool do_test = RAH2_NS::ranges::forward_range<BaseRange>;
    static constexpr bool is_borrowed = RAH2_NS::ranges::enable_borrowed_range<BaseRange>;
    using expected_cat =
        RAH2_NS::ranges::cap_iterator_tag<Tag, RAH2_STD::forward_iterator_tag, RAH2_NS::random_access_iterator_tag>;
};
void test_adjacent_transform()
{
    {
        testSuite.test_case("sample");
        /// [adjacent_transform]
        RAH2_STD::vector<int> in{0, 1, 2, 3, 4, 5};
        RAH2_STD::vector<int> out;
        for (auto abc : RAH2_NS::views::adjacent_transform<3>(
                            in, [](auto a, auto b, auto c) { return a + b + c; })
                            | RAH2_NS::views::common)
        {
            out.push_back(abc);
        }
        assert(out == (RAH2_STD::vector<int>{3, 6, 9, 12}));
        /// [adjacent_transform]
    }
    {
        testSuite.test_case("non common_range");
        // adjacent_transform With non common_range
        RAH2_STD::vector<int> out;
        for (auto abc : RAH2_NS::views::iota(0) | RAH2_NS::views::take(6)
                            | RAH2_NS::views::adjacent_transform<3>([](auto a, auto b, auto c)
                                                                    { return a + b + c; })
                            | RAH2_NS::views::common)
        {
            out.push_back(abc);
        }
        assert(out == (RAH2_STD::vector<int>{3, 6, 9, 12}));
    }
    {
        testSuite.test_case("N > size()");
        // adjacent_transform With N > view.size()
        RAH2_STD::vector<int> out;
        for (auto abc : RAH2_NS::views::iota(0) | RAH2_NS::views::take(6)
                            | RAH2_NS::views::adjacent_transform<45>(Add{}) | RAH2_NS::views::common)
        {
            out.push_back(abc);
        }
        assert(out.empty());
    }
    {
        testSuite.test_case("N == 0");
        // adjacent_transform With N == 0
        RAH2_STD::vector<RAH2_STD::vector<int>> out;
        for (auto&& abc : RAH2_NS::views::iota(0) | RAH2_NS::views::take(6)
                              | RAH2_NS::views::adjacent_transform<0>([](auto i) { return i + 1; }))
        {
            static_assert(
                RAH2_STD::tuple_size<RAH2_STD::remove_reference_t<decltype(abc)>>::value == 0,
                "tuple should be empty");
            out.emplace_back();
        }
        assert(out.empty());
    }
    testSuite.test_case("concepts");
    foreach_range_combination<test_range<make_adjacent_transform_view>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct make_slide_view
{
    auto make()
    {
        return RAH2_NS::views::slide(make_test_view<CS, Tag, Sized>(), 3);
    }
    using BaseRange = test_view<CS, Tag, Sized>;
    static constexpr bool is_sized = RAH2_NS::ranges::sized_range<BaseRange>;
    static constexpr bool is_common =
        RAH2_NS::ranges::common_range<BaseRange> && RAH2_NS::ranges::bidirectional_range<BaseRange>;
    static constexpr bool do_test = RAH2_NS::ranges::forward_range<BaseRange>;
    static constexpr bool is_borrowed = false;
    using expected_cat =
        RAH2_NS::ranges::cap_iterator_tag<Tag, RAH2_STD::input_iterator_tag, RAH2_NS::random_access_iterator_tag>;
};
void test_slide_view()
{
    {
        testSuite.test_case("sample");
        /// [sliding]
        RAH2_STD::vector<int> in{0, 1, 2, 3, 4, 5};
        RAH2_STD::vector<RAH2_STD::vector<int>> out;
        for (auto subRange : RAH2_NS::views::slide(in, 3))
        {
            out.emplace_back();
            RAH2_STD::copy(
                RAH2_NS::ranges::begin(subRange),
                RAH2_NS::ranges::end(subRange),
                RAH2_STD::back_inserter(out.back()));
        }
        assert(
            out
            == (RAH2_STD::vector<RAH2_STD::vector<int>>{{0, 1, 2}, {1, 2, 3}, {2, 3, 4}, {3, 4, 5}}));
        /// [sliding]
    }
    {
        testSuite.test_case("non common_range");
        // slide with non common_range
        RAH2_STD::vector<RAH2_STD::vector<int>> out;
        auto r = RAH2_NS::views::iota(0) | RAH2_NS::views::take(6) | RAH2_NS::views::slide(3);
        auto it = RAH2_NS::ranges::begin(r);
        auto e = RAH2_NS::ranges::end(r);
        for (; it != e; ++it)
        {
            out.emplace_back();
            auto&& subRange = *it;
            RAH2_NS::ranges::copy(subRange, RAH2_NS::back_inserter(out.back()));
        }
        assert(
            out
            == (RAH2_STD::vector<RAH2_STD::vector<int>>{{0, 1, 2}, {1, 2, 3}, {2, 3, 4}, {3, 4, 5}}));
    }

    {
        testSuite.test_case("sample_pipeable");
        /// [sliding_pipeable]
        RAH2_STD::vector<int> in{0, 1, 2, 3, 4, 5};
        RAH2_STD::vector<RAH2_STD::vector<int>> out;
        for (auto subRange : in | RAH2_NS::views::slide(3))
        {
            out.emplace_back();
            RAH2_STD::copy(
                RAH2_NS::ranges::begin(subRange),
                RAH2_NS::ranges::end(subRange),
                RAH2_STD::back_inserter(out.back()));
        }
        assert(
            out
            == (RAH2_STD::vector<RAH2_STD::vector<int>>{{0, 1, 2}, {1, 2, 3}, {2, 3, 4}, {3, 4, 5}}));
        /// [sliding_pipeable]
    }

    {
        testSuite.test_case("various");
        RAH2_STD::vector<int> in{0, 1, 2, 3, 4, 5};
        RAH2_STD::vector<RAH2_STD::vector<int>> out;
        auto range =
            in | RAH2_NS::views::cycle | RAH2_NS::views::slide(3) | RAH2_NS::views::take(in.size());
        for (auto subRange : range | RAH2_NS::views::common)
        {
            out.emplace_back();
            RAH2_STD::copy(
                RAH2_NS::ranges::begin(subRange),
                RAH2_NS::ranges::end(subRange),
                RAH2_STD::back_inserter(out.back()));
        }
        assert(
            out
            == (RAH2_STD::vector<RAH2_STD::vector<int>>{
                {0, 1, 2},
                {1, 2, 3},
                {2, 3, 4},
                {3, 4, 5},
                {4, 5, 0},
                {5, 0, 1},
            }));
    }

    {
        RAH2_STD::vector<int> in{0, 1, 2, 3};
        RAH2_STD::vector<RAH2_STD::vector<int>> out;
        for (auto subRange : RAH2_NS::views::slide(in, 4))
        {
            out.emplace_back();
            RAH2_STD::copy(
                RAH2_NS::ranges::begin(subRange),
                RAH2_NS::ranges::end(subRange),
                RAH2_STD::back_inserter(out.back()));
        }
        assert(out == (RAH2_STD::vector<RAH2_STD::vector<int>>{{0, 1, 2, 3}}));
    }

    {
        RAH2_STD::vector<int> in{0, 1};
        RAH2_STD::vector<RAH2_STD::vector<int>> out;
        for (auto subRange : RAH2_NS::views::slide(in, 4))
        {
            out.emplace_back();
            RAH2_STD::copy(
                RAH2_NS::ranges::begin(subRange),
                RAH2_NS::ranges::end(subRange),
                RAH2_STD::back_inserter(out.back()));
        }
        assert(out.empty());
    }

    {
        RAH2_STD::vector<int> in{0, 1, 2, 3};
        RAH2_STD::vector<RAH2_STD::vector<int>> out;
        for (auto subRange : RAH2_NS::views::slide(in, 0))
        {
            out.emplace_back();
            RAH2_STD::copy(
                RAH2_NS::ranges::begin(subRange),
                RAH2_NS::ranges::end(subRange),
                RAH2_STD::back_inserter(out.back()));
        }
        assert(out == (RAH2_STD::vector<RAH2_STD::vector<int>>{{0}, {1}, {2}, {3}}));
    }

    {
        RAH2_STD::vector<int> in{0, 1, 2, 3};
        RAH2_STD::vector<RAH2_STD::vector<int>> out;
        for (auto subRange : in | RAH2_NS::views::slide(1))
        {
            out.emplace_back();
            RAH2_STD::copy(
                RAH2_NS::ranges::begin(subRange),
                RAH2_NS::ranges::end(subRange),
                RAH2_STD::back_inserter(out.back()));
        }
        assert(
            out
            == (RAH2_STD::vector<RAH2_STD::vector<int>>{
                {0},
                {1},
                {2},
                {3},
            }));
    }
    testSuite.test_case("concepts");
    foreach_range_combination<test_range<make_slide_view>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct make_chunk_view
{
    auto make()
    {
        return RAH2_NS::views::chunk(make_test_view<CS, Tag, Sized>(), 3);
    }
    using BaseRange = test_view<CS, Tag, Sized>;
    static constexpr bool is_sized = RAH2_NS::ranges::sized_range<BaseRange>;
    static constexpr bool is_common = RAH2_NS::ranges::common_range<BaseRange>;
    static constexpr bool do_test = true;
    static constexpr bool is_borrowed = false;
    using expected_cat =
        RAH2_NS::ranges::cap_iterator_tag<Tag, RAH2_STD::input_iterator_tag, RAH2_NS::random_access_iterator_tag>;
};
void test_chunk_view()
{
    {
        testSuite.test_case("sample");
        /// [chunk]
        RAH2_STD::vector<int> vec_01234{0, 1, 2, 3, 4};
        RAH2_STD::vector<RAH2_STD::vector<int>> result;
        for (auto elts : RAH2_NS::views::chunk(vec_01234, 2))
            result.emplace_back(RAH2_NS::ranges::begin(elts), RAH2_NS::ranges::end(elts));
        assert(result == RAH2_STD::vector<RAH2_STD::vector<int>>({{0, 1}, {2, 3}, {4}}));
        /// [chunk]
    }
    {
        testSuite.test_case("sample_pipeable");
        /// [chunk_pipeable]
        RAH2_STD::vector<int> vec_01234{0, 1, 2, 3, 4};
        RAH2_STD::vector<RAH2_STD::vector<int>> result;
        for (auto elts : vec_01234 | RAH2_NS::views::chunk(2))
            result.emplace_back(RAH2_NS::ranges::begin(elts), RAH2_NS::ranges::end(elts));
        assert(result == RAH2_STD::vector<RAH2_STD::vector<int>>({{0, 1}, {2, 3}, {4}}));
        /// [chunk_pipeable]
    }
    {
        testSuite.test_case("non-common_view");
        /// Chunk with non-common_view
        auto vec_01234 = RAH2_NS::views::iota(0) | RAH2_NS::views::take(5);
        RAH2_STD::vector<RAH2_STD::vector<int>> result;
        for (auto elts : RAH2_NS::views::chunk(vec_01234, 2) | RAH2_NS::views::common)
        {
            result.emplace_back();
            auto& back = result.back();
            for (auto i : elts | RAH2_NS::views::common)
            {
                back.emplace_back(i);
            }
        }
        assert(result == RAH2_STD::vector<RAH2_STD::vector<int>>({{0, 1}, {2, 3}, {4}}));
    }
    testSuite.test_case("concepts");
    foreach_range_combination<test_range<make_chunk_view>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct make_stride_view
{
    auto make()
    {
        return RAH2_NS::views::stride(make_test_view<CS, Tag, Sized>(), 3);
    }
    using BaseRange = test_view<CS, Tag, Sized>;
    static constexpr bool is_sized = RAH2_NS::ranges::sized_range<BaseRange>;
    static constexpr bool is_common = RAH2_NS::ranges::common_range<BaseRange>
                                      && (RAH2_NS::ranges::sized_range<BaseRange>
                                          || !RAH2_NS::ranges::bidirectional_range<BaseRange>);
    static constexpr bool do_test = true;
    static constexpr bool is_borrowed = RAH2_NS::ranges::enable_borrowed_range<BaseRange>;
    using expected_cat =
        RAH2_NS::ranges::cap_iterator_tag<Tag, RAH2_STD::input_iterator_tag, RAH2_NS::random_access_iterator_tag>;
};
void test_stride_view()
{
    {
        testSuite.test_case("sample");
        /// [stride]
        RAH2_STD::vector<int> vec{0, 1, 2, 3, 4, 5, 6, 7};
        RAH2_STD::vector<int> result;
        for (int const i : RAH2_NS::views::stride(vec, 2))
            result.push_back(i);
        assert(result == RAH2_STD::vector<int>({0, 2, 4, 6}));
        /// [stride]
    }
    {
        testSuite.test_case("sample_pipeable");
        /// [stride_pipeable]
        RAH2_STD::vector<int> vec{0, 1, 2, 3, 4, 5, 6, 7};
        RAH2_STD::vector<int> result;
        for (int const i : vec | RAH2_NS::views::stride(2))
            result.push_back(i);
        assert(result == RAH2_STD::vector<int>({0, 2, 4, 6}));
        /// [stride_pipeable]
    }

    testSuite.test_case("concepts");
    foreach_range_combination<test_range<make_stride_view>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct make_ref_view
{
    using BaseRange = test_view<CS, Tag, Sized>;
    BaseRange base;
    auto make()
    {
        return RAH2_NS::views::ref(base);
    }
    static constexpr bool is_sized = RAH2_NS::ranges::sized_range<BaseRange>;
    static constexpr bool is_common = RAH2_NS::ranges::common_range<BaseRange>;
    static constexpr bool do_test = true;
    static constexpr bool is_borrowed = true;
    using expected_cat =
        RAH2_NS::ranges::cap_iterator_tag<Tag, RAH2_STD::input_iterator_tag, RAH2_NS::contiguous_iterator_tag>;
};
void test_ref_view()
{
    {
        testSuite.test_case("sample");
        /// [ref]
        RAH2_STD::vector<int> vec{0, 1, 2, 2, 3};
        RAH2_STD::vector<int> out;
        auto const ref = RAH2_NS::views::ref(vec);
        for (auto&& val : ref)
        {
            out.push_back(val);
        }
        assert(out == (RAH2_STD::vector<int>{0, 1, 2, 2, 3}));
        /// [ref]
    }

    testSuite.test_case("concepts");
    foreach_range_combination<test_range<make_ref_view>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct make_subrange
{
    using BaseRange = test_view<CS, Tag, Sized>;
    BaseRange base;
    auto make()
    {
        return RAH2_NS::ranges::make_subrange(base.begin(), base.end());
    }
    static constexpr bool is_sized = RAH2_NS::sized_sentinel_for<
        RAH2_NS::ranges::sentinel_t<BaseRange>,
        RAH2_NS::ranges::iterator_t<BaseRange>>;
    static constexpr bool is_common = RAH2_NS::ranges::common_range<BaseRange>;
    static constexpr bool do_test = true;
    static constexpr bool is_borrowed = true;
    using expected_cat = Tag;
};
void test_subrange()
{
    {
        testSuite.test_case("sample");
        /// [subrange]
        RAH2_STD::vector<int> vec{0, 1, 2, 2, 3};
        auto sub = RAH2_NS::ranges::make_subrange(vec.begin() + 1, vec.begin() + 4);
        RAH2_STD::vector<int> out;
        for (auto&& val : sub)
        {
            out.push_back(val);
        }
        assert(out == (RAH2_STD::vector<int>{1, 2, 2}));
        /// [subrange]
    }

    testSuite.test_case("concepts");
    foreach_range_combination<test_range<make_subrange>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct make_unbounded_view
{
    using BaseRange = test_view<CS, Tag, Sized>;
    BaseRange base;
    auto make()
    {
        return RAH2_NS::views::unbounded(RAH2_NS::ranges::begin(base));
    }
    static constexpr bool is_sized = false;
    static constexpr bool is_common = false;
    static constexpr bool do_test = true;
    static constexpr bool is_borrowed = true;
    using expected_cat =
        RAH2_NS::ranges::cap_iterator_tag<Tag, RAH2_STD::input_iterator_tag, RAH2_NS::contiguous_iterator_tag>;
};
void test_unbounded_view()
{
    {
        testSuite.test_case("sample");
        /// [unbounded_view]
        RAH2_STD::vector<int> vec{0, 1, 2, 2, 3};
        RAH2_STD::vector<int> out;
        auto v = RAH2_NS::views::unbounded(RAH2_NS::ranges::begin(vec));
        for (auto&& val : v | RAH2_NS::views::take(5) | RAH2_NS::views::common)
        {
            out.push_back(val);
        }
        assert(out == (RAH2_STD::vector<int>{0, 1, 2, 2, 3}));
        /// [unbounded_view]
    }

    testSuite.test_case("concepts");
    foreach_range_combination<test_range<make_unbounded_view>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct make_cycle_view
{
    using BaseRange = test_view<CS, Tag, Sized>;
    auto make()
    {
        return RAH2_NS::views::cycle(make_test_view<CS, Tag, Sized>());
    }
    static constexpr bool is_sized = false;
    static constexpr bool is_common = false;
    static constexpr bool do_test = RAH2_NS::ranges::forward_range<BaseRange>;
    static constexpr bool is_borrowed = false;
    using expected_cat = RAH2_STD::conditional_t<
        CS == CommonOrSent::Common,
        RAH2_NS::ranges::cap_iterator_tag<Tag, RAH2_STD::input_iterator_tag, RAH2_NS::bidirectional_iterator_tag>,
        RAH2_NS::ranges::cap_iterator_tag<Tag, RAH2_STD::input_iterator_tag, RAH2_NS::forward_iterator_tag>>;
};
void test_cycle_view()
{
    {
        testSuite.test_case("sample");
        /// [cycle]
        RAH2_STD::vector<int> in{0, 1, 2};
        auto cy = RAH2_NS::views::cycle(in);
        RAH2_STD::vector<int> out;
        RAH2_STD::copy_n(cy.begin(), 8, RAH2_STD::back_inserter(out));
        assert(out == RAH2_STD::vector<int>({0, 1, 2, 0, 1, 2, 0, 1}));
        /// [cycle]
        STATIC_ASSERT((RAH2_NS::ranges::bidirectional_range<decltype(cy)>));
        STATIC_ASSERT((not RAH2_NS::ranges::random_access_range<decltype(cy)>));
    }

    {
        /// [cycle_pipeable]
        RAH2_STD::vector<int> in{0, 1, 2};
        auto cy = in | RAH2_NS::views::cycle;
        RAH2_STD::vector<int> out;
        RAH2_STD::copy_n(cy.begin(), 8, RAH2_STD::back_inserter(out));
        assert(out == RAH2_STD::vector<int>({0, 1, 2, 0, 1, 2, 0, 1}));
        /// [cycle_pipeable]
    }

    {
        RAH2_STD::vector<int> in{0, 1, 2};
        auto cy = RAH2_NS::views::cycle(in) | RAH2_NS::views::take(8);
        RAH2_STD::vector<int> out;
        // static_assert(RAH2_NS::range<decltype(RAH2_NS::back_inserter(out))>, "dkjh");
        RAH2_NS::ranges::copy(cy, RAH2_NS::back_inserter(out));
        assert(out == RAH2_STD::vector<int>({0, 1, 2, 0, 1, 2, 0, 1}));
    }

    testSuite.test_case("concepts");
    foreach_range_combination<test_range<make_cycle_view>>();
}

template <CommonOrSent CS, typename Tag, bool Sized>
struct make_slice_view
{
    using BaseRange = test_view<CS, Tag, Sized>;
    auto make()
    {
        return RAH2_NS::views::slice(make_test_view<CS, Tag, Sized>(), 1, 6);
    }
    static constexpr bool is_sized = RAH2_NS::ranges::sized_range<BaseRange>;
    static constexpr bool is_common =
        RAH2_NS::ranges::random_access_range<BaseRange> && RAH2_NS::ranges::sized_range<BaseRange>;
    static constexpr bool do_test = true;
    static constexpr bool is_borrowed = false;
    using expected_cat = Tag;
};
void test_slice_view()
{
    {
        testSuite.test_case("sample");
        /// [slice]
        RAH2_STD::vector<int> vec{0, 1, 2, 3, 4, 5, 6, 7};
        RAH2_STD::vector<int> result;
        for (int i : RAH2_NS::views::slice(vec, 2, 6))
            result.push_back(i);
        assert(result == RAH2_STD::vector<int>({2, 3, 4, 5}));
        RAH2_STD::vector<int> result2;
        for (int i : RAH2_NS::views::slice(vec, 2, 6))
            result2.push_back(i);
        assert(result2 == RAH2_STD::vector<int>({2, 3, 4, 5}));
        /// [slice]
    }
    {
        /// [slice_pipeable]
        RAH2_STD::vector<int> vec{0, 1, 2, 3, 4, 5, 6, 7};
        RAH2_STD::vector<int> result;
        for (int i : vec | RAH2_NS::views::slice(2, 6))
            result.push_back(i);
        assert(result == RAH2_STD::vector<int>({2, 3, 4, 5}));
        /// [slice_pipeable]
    }

    testSuite.test_case("concepts");
    foreach_range_combination<test_range<make_slice_view>>();
}
