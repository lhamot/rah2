//
// Copyright (c) 2019 Loïc HAMOT
//
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//
#pragma once

#include <cassert>
#include <tuple>

#include "range_bases.hpp"
#include "range_algorithms.hpp"

namespace RAH_NAMESPACE
{

    // **************************** Range conversions *********************************************

    /// @brief Return a container of type C, filled with the content of range
    ///
    /// @snippet test.cpp rah::to
    template <typename C, typename R>
    auto to(R&& range)
    {
        return C(RAH_NAMESPACE::begin(range), RAH_NAMESPACE::end(range));
    }

    /// @brief Return a container of type C, filled with the content of range
    /// @remark pipeable syntax
    ///
    /// @snippet test.cpp rah::to_container_pipeable
    template <typename C>
    auto to()
    {
        return make_pipeable([=](auto&& range)
                             { return to<C>(RAH_STD::forward<decltype(range)>(range)); });
    }

    // **************************************** pipeable **********************************************

    template <typename Func>
    struct pipeable
    {
        Func func;
    };

    template <typename MakeRange>
    auto make_pipeable(MakeRange&& make_range)
    {
        return pipeable<MakeRange>{std::forward<MakeRange>(make_range)};
    }

    template <typename R, typename MakeRange>
    auto operator|(R&& range, pipeable<MakeRange> const& adapter)
        -> decltype(adapter.func(RAH_STD::forward<R>(range)))
    {
        return adapter.func(RAH_STD::forward<R>(range));
    }

    // ************************************ iterator_facade *******************************************

    namespace details
    {

        // Small optional impl for C++14 compilers
        template <typename T>
        struct optional
        {
            optional() = default;
            optional(optional const& other)
            {
                if (other.has_value())
                {
                    new (getPtr()) T(other.get());
                    is_allocated_ = true;
                }
            }
            optional(optional&& other)
            {
                (*this) = RAH_STD::move(other);
            }
            optional& operator=(optional const& other)
            {
                if (has_value())
                {
                    if (other.has_value())
                    {
                        // Handle the case where T is not copy assignable
                        reset();
                        new (getPtr()) T(other.get());
                        is_allocated_ = true;
                    }
                    else
                        reset();
                }
                else
                {
                    if (other.has_value())
                    {
                        new (getPtr()) T(other.get());
                        is_allocated_ = true;
                    }
                }
                return *this;
            }
            optional& operator=(optional&& other)
            {
                if (has_value())
                {
                    if (other.has_value())
                    {
                        // A lambda with const capture is not move assignable
                        reset();
                        new (getPtr()) T(RAH_STD::move(other.get()));
                        is_allocated_ = true;
                    }
                    else
                        reset();
                }
                else
                {
                    if (other.has_value())
                    {
                        new (getPtr()) T(RAH_STD::move(other.get()));
                        is_allocated_ = true;
                    }
                }
                return *this;
            }
            optional(T const& other)
            {
                new (getPtr()) T(other);
                is_allocated_ = true;
            }
            optional(T&& other)
            {
                new (getPtr()) T(RAH_STD::move(other));
                is_allocated_ = true;
            }
            optional& operator=(T const& other)
            {
                reset();
                new (getPtr()) T(other);
                is_allocated_ = true;
                return *this;
            }
            optional& operator=(T&& other)
            {
                reset();
                new (getPtr()) T(RAH_STD::move(other));
                is_allocated_ = true;
                return *this;
            }
            ~optional()
            {
                reset();
            }

            bool has_value() const
            {
                return is_allocated_;
            }

            void reset()
            {
                if (is_allocated_)
                {
                    destruct_value();
                    is_allocated_ = false;
                }
            }

            T& get()
            {
                assert(is_allocated_);
                return *getPtr();
            }

            T const& get() const
            {
                assert(is_allocated_);
                return *getPtr();
            }

            T& operator*()
            {
                return get();
            }
            T const& operator*() const
            {
                return get();
            }
            T* operator->()
            {
                assert(is_allocated_);
                return getPtr();
            }
            T const* operator->() const
            {
                assert(is_allocated_);
                return getPtr();
            }

        private:
            T* getPtr()
            {
                return (T*)&value_;
            }
            T const* getPtr() const
            {
                return (T const*)&value_;
            }
            void destruct_value()
            {
                get().~T();
            }
            RAH_STD::aligned_storage_t<sizeof(T), RAH_STD::alignment_of<T>::value> value_;
            bool is_allocated_ = false;
        };

        template <class Reference>
        struct pointer_type
        {
            using type = RAH_NAMESPACE::details::optional<Reference>;
            template <typename Ref>
            static type to_pointer(Ref&& ref)
            {
                return RAH_STD::forward<Ref>(ref);
            }
        };

        template <class T>
        struct pointer_type<T&> // "real" references
        {
            using type = T*;
            template <typename Ref>
            static type to_pointer(Ref&& ref)
            {
                return &ref;
            }
        };
    } // namespace details

#undef RAH_SELF
#undef RAH_SELF_CONST
#define RAH_SELF (*static_cast<I*>(this))
#define RAH_SELF_CONST (*static_cast<I const*>(this))

    struct sentinel_iterator
    {
    };

    template <typename I, typename S, typename R, typename C>
    struct iterator_facade;

    template <typename I, typename S, typename R>
    struct iterator_facade<I, S, R, RAH_STD::input_iterator_tag>
    {
        DeleteCheck<iterator_facade<I, S, R, RAH_STD::input_iterator_tag>> deleteCheck;

        using iterator_category = RAH_STD::input_iterator_tag;
        using value_type = RAH_STD::remove_reference_t<R>;
        using difference_type = intptr_t;
        using pointer = typename details::pointer_type<R>::type;
        using reference = R;

        static_assert(not RAH_STD::is_reference<value_type>::value, "value_type can't be a reference");

        template <typename It>
        bool operator!=(It&& rho) const
        {
            return !(RAH_SELF_CONST == rho);
        }
        //bool operator!=(S const& rho) const
        //{
        //    return !(RAH_SELF_CONST == rho);
        //}
        auto operator->() const
        {
            return RAH_NAMESPACE::details::pointer_type<reference>::to_pointer(*RAH_SELF_CONST);
        }

    protected:
        ~iterator_facade() = default;
    };

    template <typename I, typename S, typename R>
    struct iterator_facade<I, S, R, RAH_STD::forward_iterator_tag>
        : iterator_facade<I, S, R, RAH_STD::input_iterator_tag>
    {
        using iterator_category = RAH_STD::forward_iterator_tag;
    };

    template <typename I, typename S, typename R>
    struct iterator_facade<I, S, R, RAH_STD::output_iterator_tag>
    {
        DeleteCheck<iterator_facade<I, S, R, RAH_STD::output_iterator_tag>> deleteCheck;

        using iterator_category = RAH_STD::output_iterator_tag;
        using value_type = RAH_STD::remove_reference_t<R>;
        using difference_type = intptr_t;
        using pointer = value_type*;
        using reference = R;

        static_assert(not RAH_STD::is_reference<value_type>::value, "value_type can't be a reference");

        auto& operator++()
        {
            return *this;
        }
        auto& operator*() const
        {
            return *this;
        }
        bool operator!=(I const&) const
        {
            return true;
        }
        bool operator==(I const&) const
        {
            return false;
        }

        template <typename V>
        auto const& operator=(V&& value) const
        {
            RAH_SELF_CONST.put(RAH_STD::forward<V>(value));
            return RAH_SELF_CONST;
        }
    };

    template <typename I, typename S, typename R>
    struct iterator_facade<I, S, R, RAH_STD::bidirectional_iterator_tag>
        : iterator_facade<I, S, R, RAH_STD::forward_iterator_tag>
    {
        using iterator_category = RAH_STD::bidirectional_iterator_tag;
    };

    template <typename I, typename S, typename R>
    struct iterator_facade<I, S, R, RAH_STD::random_access_iterator_tag>
        : iterator_facade<I, S, R, RAH_STD::bidirectional_iterator_tag>
    {
        DeleteCheck<iterator_facade<I, S, R, RAH_STD::random_access_iterator_tag>> deleteCheck;
        using iterator_category = RAH_STD::random_access_iterator_tag;
    };

    // ********************************** inserter ***********************************************

    /// @see rah::back_inserter
    template <typename C>
    struct insert_iterator
        : iterator_facade<insert_iterator<C>, void, range_reference_t<C>, RAH_STD::output_iterator_tag>
    {
        C* container_;
        using Iterator = RAH_NAMESPACE::iterator_t<C>;
        Iterator iter_;
        template <typename I>
        insert_iterator(C& container, I&& iter)
            : container_(&container)
            , iter_(RAH_STD::forward<I>(iter))
        {
        }
        template <typename V>
        void put(V&& value) const
        {
            container_->insert(iter_, value);
        }
    };

    /// @brief Make a range which insert into the back of the a container
    ///
    /// @snippet test.cpp rah::back_inserter
    template <typename C, typename I>
    auto inserter(C&& container, I&& iter)
    {
        using Container = RAH_STD::remove_reference_t<C>;
        auto begin = insert_iterator<Container>(container, RAH_STD::forward<I>(iter));
        auto end = insert_iterator<Container>(container, RAH_STD::forward<I>(iter));
        return RAH_NAMESPACE::make_subrange(begin, end);
    }

    namespace views
    {

        // ******************************* istream_view ******************************************************

        template <typename Val, class CharT, class Traits = std::char_traits<CharT>>
        struct basic_istream_view : public view_interface<basic_istream_view<Val, CharT, Traits>>
        {
            std::istream* stream_ = nullptr;
            Val value_;

            basic_istream_view(std::istream* stream)
                : stream_(stream)
            {
            }

            struct sentinel
            {
            };

            struct iterator : iterator_facade<iterator, sentinel, Val, RAH_STD::input_iterator_tag>
            {
                basic_istream_view* istream_;

                iterator(basic_istream_view* istream)
                    : istream_(istream)
                {
                }

                iterator(const iterator&) = delete;
                iterator(iterator&&) = default;
                iterator& operator=(const iterator&) = delete;
                iterator& operator=(iterator&&) = default;
                ~iterator() = default;

                Val operator*() const
                {
                    return istream_->value_;
                }

                iterator const& operator++()
                {
                    *(istream_->stream_) >> istream_->value_;
                    return *this;
                }
                bool operator==(sentinel const&) const
                {
                    return !(*istream_->stream_);
                }
            };

            constexpr auto begin()
            {
                *stream_ >> value_;
                return iterator(this);
            }

            constexpr auto end()
            {
                return sentinel{};
            }
        };

        template <class Val>
        using istream_view = basic_istream_view<Val, char>;

        template <class Val>
        using wistream_view = basic_istream_view<Val, wchar_t>;

        template <class Val, typename S>
        auto istream(S& stream)
        {
            return basic_istream_view<Val, typename S::char_type, typename S::traits_type>(&stream);
        }

        // ********************************* ref_view *********************************************

        template <typename R>
        struct ref_view : view_interface<ref_view<R>>
        {
            R* ref_ = nullptr;
            ref_view(R* ref = nullptr)
                : ref_(ref)
            {
            }
            ref_view(ref_view const&) = default;
            ref_view& operator=(ref_view const&) = default;
            ref_view(ref_view&&) = default;
            ref_view& operator=(ref_view&&) = default;
            auto begin() const
            {
                return RAH_NAMESPACE::begin(*ref_);
            }
            auto end() const
            {
                return RAH_NAMESPACE::end(*ref_);
            }
            bool empty() const
            {
                return RAH_NAMESPACE::empty(*ref_);
            }
            bool size() const
            {
                return RAH_NAMESPACE::size(*ref_);
            }
            bool data() const
            {
                return RAH_NAMESPACE::data(*ref_);
            }
        };

        template <typename R>
        auto ref(R&& range)
        {
            static_assert(not std::is_rvalue_reference_v<R>, "range can't be a rvalue reference");
            return RAH_NAMESPACE::views::ref_view<std::remove_reference_t<R>>(&range);
        }

        inline auto ref()
        {
            return make_pipeable(
                [](auto&& range)
                { return RAH_NAMESPACE::views::ref(RAH_STD::forward<decltype(range)>(range)); });
        }

        // ********************************* owning_view ******************************************

        template <typename R>
        class owning_view : view_interface<ref_view<R>>
        {
            R range_;

        public:
            explicit owning_view(R r)
                : range_(std::move(r))
            {
            }
            owning_view(owning_view const&) = delete;
            owning_view& operator=(owning_view const&) = delete;
            owning_view(owning_view&&) = default;
            owning_view& operator=(owning_view&&) = default;
            ~owning_view() = default;
            R& base()
            {
                return range_;
            }
            R const& base() const
            {
                return range_;
            }
            auto begin() const
            {
                return range_.begin();
            }
            auto end() const
            {
                return range_.end();
            }
            bool empty() const
            {
                return RAH_NAMESPACE::empty(range_);
            }
            bool size() const
            {
                return RAH_NAMESPACE::size(range_);
            }
            bool data() const
            {
                return RAH_NAMESPACE::data(range_);
            }
        };

        template <typename R>
        auto owning(R&& range)
        {
            static_assert(not std::is_lvalue_reference_v<R>, "range can't be a lvalue reference");
            return RAH_NAMESPACE::views::owning_view<std::remove_reference_t<R>>(
                std::forward<R>(range));
        }

        inline auto owning()
        {
            return make_pipeable(
                [](auto&& range)
                { return RAH_NAMESPACE::views::owning(RAH_STD::forward<decltype(range)>(range)); });
        }

        // ********************************** all *************************************************

        template <typename R, typename = RAH_STD::enable_if_t<view<RAH_STD::remove_reference_t<R>>>>
        auto all(R&& range) -> decltype(RAH_STD::forward<R>(range))
        {
            return RAH_STD::forward<R>(range);
        }
        template <
            typename R,
            typename = RAH_STD::enable_if_t<not view<RAH_STD::remove_reference_t<R>>, int>,
            typename = RAH_STD::enable_if_t<RAH_STD::is_lvalue_reference_v<R>, int>>
        auto all(R&& range, int = 0)
        {
            return RAH_NAMESPACE::views::ref(RAH_STD::forward<R>(range));
        }

        template <
            typename R,
            typename = RAH_STD::enable_if_t<not view<RAH_STD::remove_reference_t<R>>, int>,
            typename = RAH_STD::enable_if_t<not RAH_STD::is_lvalue_reference_v<R>, int>>
        auto all(R&& range, short = 0)
        {
            return RAH_STD::forward<R>(range);
        }

        template <typename V>
        auto all(std::initializer_list<V>& range)
        {
            return RAH_NAMESPACE::views::ref(range);
        }

        inline auto all()
        {
            return make_pipeable([=](auto&& range)
                                 { return all(RAH_STD::forward<decltype(range)>(range)); });
        }

        // ******************************************* take ***********************************************

        template <typename R>
        class take_view : public view_interface<take_view<R>>
        {
            R inputView_;
            size_t count_;

        public:
            using base_iterator = RAH_NAMESPACE::iterator_t<R>;
            using base_sentinel = RAH_NAMESPACE::sentinel_t<R>;
            struct sentinel
            {
                base_sentinel sent;
                int64_t count;

                //sentinel(base_sentinel _sentinel, int64_t _count)
                //    : sentinel(std::move(_sentinel))
                //    , sent(_count)
                //{
                //}
            };

            struct iterator : iterator_facade<
                                  iterator,
                                  sentinel,
                                  RAH_NAMESPACE::range_reference_t<R>,
                                  RAH_NAMESPACE::range_iter_categ_t<R>>
            {
                base_iterator iter_;
                int64_t count_ = int64_t();

                iterator()
                {
                }
                iterator(base_iterator iter, int64_t count)
                    : iter_(iter)
                    , count_(count)
                {
                }

                iterator& operator++()
                {
                    ++iter_;
                    ++count_;
                    return *this;
                }
                iterator& operator+=(intptr_t off)
                {
                    iter_ += off;
                    count_ += off;
                    return *this;
                }
                iterator& operator--()
                {
                    --iter_;
                    --count_;
                    return *this;
                }
                auto operator-(iterator const& r) const
                {
                    return RAH_STD::min<intptr_t>(iter_ - r.iter_, count_ - r.count_);
                }
                auto operator*() const -> decltype(*iter_)
                {
                    return *iter_;
                }
                bool operator==(iterator const& r) const
                {
                    // Have to stop when to count over OR if the range is end
                    return count_ == r.count_ or iter_ == r.iter_;
                }
                bool operator==(sentinel const& r) const
                {
                    // Have to stop when to count over OR if the range is end
                    return count_ == r.count or iter_ == r.sent;
                }
            };

            take_view(R inputView, size_t count)
                : inputView_(inputView)
                , count_(count)
            {
            }

            auto begin()
            {
                return iterator(RAH_NAMESPACE::begin(inputView_), 0);
            }

            template <typename Q = R, typename = std::enable_if_t<common_range<Q>>>
            auto end()
            {
                static_assert(std::is_same_v<iterator_t<R>, sentinel_t<R>>, "not common_range");
                return iterator(RAH_NAMESPACE::end(inputView_), count_);
            }

            template <typename Q = R, typename = std::enable_if_t<not common_range<Q>>>
            auto end(int = 0)
            {
                return sentinel{RAH_NAMESPACE::end(inputView_), static_cast<int64_t>(count_)};
            }
        };

        template <typename R>
        auto take(R&& range, size_t count)
        {
            auto rangeView = all(std::forward<R>(range));
            return take_view<decltype(rangeView)>(rangeView, count);
        }

        inline auto take(size_t count)
        {
            return make_pipeable([=](auto&& range)
                                 { return take(RAH_STD::forward<decltype(range)>(range), count); });
        }

        // ******************************************* sliding ********************************************

        template <typename R, bool IsCommonRange>
        struct slide_view;

        // common_range. iterator/iterator
        template <typename R>
        struct slide_view<R, true> : view_interface<slide_view<R, true>>
        {
            R inputView_;
            intptr_t count_;

            using base_iterator = iterator_t<R>;

            struct iterator
                : iterator_facade<iterator, void, subrange<base_iterator>, typename RAH_NAMESPACE::range_iter_categ_t<R>>
            {
                // Actually store a closed range [begin, last]
                //   to avoid to exceed the end iterator of the underlying range
                // The last valid iterator will have subRangeLast_ equal to the end iterator of base
                // So the "past-the-end" range will overcome the end iterator of base
                base_iterator subRangeBegin_;
                base_iterator subRangeLast_;

                iterator() = default;
                iterator(base_iterator subRangeBegin, base_iterator subRangeLast)
                    : subRangeBegin_(std::move(subRangeBegin))
                    , subRangeLast_(std::move(subRangeLast))
                {
                }

                iterator& operator++()
                {
                    ++subRangeBegin_;
                    ++subRangeLast_;
                    return *this;
                }
                iterator& operator+=(intptr_t off)
                {
                    subRangeBegin_ += off;
                    subRangeLast_ += off;
                    return *this;
                }
                iterator& operator--()
                {
                    --subRangeBegin_;
                    --subRangeLast_;
                    return *this;
                }
                auto operator-(iterator const& r) const
                {
                    return subRangeBegin_ - r.subRangeBegin_;
                }
                auto operator*() const
                {
                    base_iterator endIter = subRangeLast_;
                    ++endIter;
                    return make_subrange(subRangeBegin_, endIter);
                }
                bool operator==(iterator const& r) const
                {
                    return subRangeBegin_ == r.subRangeBegin_;
                }
            };

            slide_view(R inputView, intptr_t count)
                : inputView_(std::move(inputView))
                , count_(count)
            {
            }

            auto begin()
            {
                auto const rangeEnd = RAH_NAMESPACE::end(inputView_);
                if (count_ == 0)
                {
                    return iterator(rangeEnd, rangeEnd);
                }
                auto const subRangeBegin = RAH_NAMESPACE::begin(inputView_);
                auto subRangeLast = subRangeBegin;
                auto const left = RAH_NAMESPACE::advance(subRangeLast, count_, rangeEnd);
                if (left != 0)
                {
                    return iterator(rangeEnd, rangeEnd);
                }
                --subRangeLast;
                return iterator(subRangeBegin, subRangeLast);
            }

            auto end()
            {
                auto const rangeEnd = RAH_NAMESPACE::end(inputView_);
                if (count_ == 0)
                {
                    return iterator(rangeEnd, rangeEnd);
                }
                auto const subRangeBegin = RAH_NAMESPACE::begin(inputView_);
                auto subRangeFirst = rangeEnd;
                auto const left = RAH_NAMESPACE::advance(subRangeFirst, -count_, subRangeBegin);
                if (left != 0)
                {
                    return iterator(rangeEnd, rangeEnd);
                }
                ++subRangeFirst;
                return iterator(subRangeFirst, rangeEnd);
            }
        };

        // No common_range. iterator/sentinel
        template <typename R>
        struct slide_view<R, false> : view_interface<slide_view<R, false>>
        {
            R inputView_;
            intptr_t count_;

            using base_iterator = iterator_t<R>;
            using base_sentinel = sentinel_t<R>;

            struct sentinel
            {
                base_sentinel sent;
            };
            struct iterator
                : iterator_facade<iterator, sentinel, subrange<base_iterator>, RAH_NAMESPACE::range_iter_categ_t<R>>
            {
                // Actually store a closed range [begin, last]
                //   to avoid to exceed the end iterator of the underlying range
                // The last valid iterator will have subRangeLast_ equal to the end iterator of base
                // So the "past-the-end" range will overcome the end iterator of base
                base_iterator subRangeBegin_;
                base_iterator subRangeLast_;

                iterator() = default;
                iterator(base_iterator subRangeBegin, base_iterator subRangeLast)
                    : subRangeBegin_(std::move(subRangeBegin))
                    , subRangeLast_(std::move(subRangeLast))
                {
                }

                iterator& operator++()
                {
                    ++subRangeBegin_;
                    ++subRangeLast_;
                    return *this;
                }
                iterator& operator+=(intptr_t off)
                {
                    subRangeBegin_ += off;
                    subRangeLast_ += off;
                    return *this;
                }
                iterator& operator--()
                {
                    --subRangeBegin_;
                    --subRangeLast_;
                    return *this;
                }
                auto operator-(iterator const& r) const
                {
                    return subRangeBegin_ - r.subRangeBegin_;
                }
                auto operator*() const
                {
                    base_iterator endIter = subRangeLast_;
                    ++endIter;
                    return make_subrange(subRangeBegin_, endIter);
                }
                bool operator==(iterator const& r) const
                {
                    return subRangeBegin_ == r.subRangeBegin_ or subRangeLast_ == r.subRangeLast_;
                }
                bool operator==(sentinel const& r) const
                {
                    return subRangeLast_ == r.sent;
                }
            };

            slide_view(R inputView, intptr_t count)
                : inputView_(std::move(inputView))
                , count_(count)
            {
            }

            auto begin()
            {
                auto const rangeEnd = RAH_NAMESPACE::end(inputView_);
                auto const subRangeBegin = RAH_NAMESPACE::begin(inputView_);
                auto subRangeLast = subRangeBegin;
                auto const left = RAH_NAMESPACE::advance(subRangeLast, count_, rangeEnd);
                if (left != 0)
                {
                    // subRangeLast should be att end
                    return iterator(subRangeLast, subRangeLast);
                }
                --subRangeLast;
                return iterator(subRangeBegin, subRangeLast);
            }

            auto end()
            {
                return sentinel{RAH_NAMESPACE::end(inputView_)};
            }
        };

        template <typename R>
        auto slide(R&& range, size_t n)
        {
            auto rangeView = all(range);
            return slide_view<decltype(rangeView), RAH_NAMESPACE::common_range<remove_cvref_t<R>>>(
                std::move(rangeView), n);
        }

        inline auto slide(size_t n)
        {
            return make_pipeable([=](auto&& range)
                                 { return slide(RAH_STD::forward<decltype(range)>(range), n); });
        }

        // ******************************************* drop_exactly ***************************************

        template <typename R>
        auto drop_exactly(R&& range, size_t count)
        {
            auto iter1 = RAH_NAMESPACE::begin(range);
            auto iter2 = RAH_NAMESPACE::end(range);
            RAH_STD::advance(iter1, count);
            return make_subrange(iter1, iter2);
        }

        inline auto drop_exactly(size_t count)
        {
            return make_pipeable(
                [=](auto&& range)
                { return drop_exactly(RAH_STD::forward<decltype(range)>(range), count); });
        }

        // ******************************************* drop ***********************************************

        template <typename R>
        class drop_view : view_interface<drop_view<R>>
        {
            R base_;
            size_t drop_count_ = 0;

        public:
            using iterator = RAH_NAMESPACE::iterator_t<R>;

            drop_view() = default;
            drop_view(R v, size_t drop_count)
                : base_(std::move(v))
                , drop_count_(drop_count)
            {
            }

            iterator begin()
            {
                auto iter = RAH_NAMESPACE::begin(base_);
                RAH_NAMESPACE::advance(iter, drop_count_, RAH_NAMESPACE::end(base_));
                return iter;
            }

            iterator end()
            {
                return RAH_NAMESPACE::end(base_);
            }
        };

        template <typename R>
        auto drop(R&& range, size_t count)
        {
            auto ref = RAH_NAMESPACE::views::all(std::forward<R>(range));
            return drop_view<decltype(ref)>(std::move(ref), count);
        }

        inline auto drop(size_t count)
        {
            return make_pipeable([=](auto&& range)
                                 { return drop(RAH_STD::forward<decltype(range)>(range), count); });
        }

        // ******************************************* drop_while *********************************

        template <typename R, typename F>
        class drop_while_view : view_interface<drop_while_view<R, F>>
        {
            R base_;
            F pred_;

        public:
            using iterator = RAH_NAMESPACE::iterator_t<R>;
            using sentinel = RAH_NAMESPACE::sentinel_t<R>;

            drop_while_view() = default;
            drop_while_view(R v, F func)
                : base_(std::move(v))
                , pred_(std::move(func))
            {
            }

            iterator begin()
            {
                auto iter = RAH_NAMESPACE::begin(base_);
                auto end = RAH_NAMESPACE::end(base_);
                while (pred_(*iter) and iter != end)
                {
                    ++iter;
                }
                return iter;
            }

            sentinel end()
            {
                return RAH_NAMESPACE::end(base_);
            }
        };

        /// @brief A view of elements from an underlying sequence, beginning at the first element
        /// for which the predicate returns false.
        ///
        /// @snippet test.cpp rah::views::drop_while
        /// @snippet test.cpp rah::views::drop_while_pipeable
        template <typename R, typename P>
        auto drop_while(R&& range, P&& predicate)
        {
            auto ref = RAH_NAMESPACE::views::all(std::forward<R>(range));
            return drop_while_view<decltype(ref), RAH_NAMESPACE::remove_cvref_t<P>>(
                std::move(ref), std::forward<P>(predicate));
        }

        template <typename P>
        auto drop_while(P&& predicate)
        {
            return make_pipeable(
                [pred = std::forward<P>(predicate)](auto&& range)
                { return drop_while(RAH_STD::forward<decltype(range)>(range), std::move(pred)); });
        }

        // ************************************ split_view ****************************************

        template <typename R, typename P>
        class split_view : public view_interface<split_view<R, P>>
        {
            R base_;
            P pattern_;
            using inner_iterator = RAH_NAMESPACE::iterator_t<R>;
            using inner_sentinel = RAH_NAMESPACE::sentinel_t<R>;
            details::optional<subrange<RAH_NAMESPACE::iterator_t<R>>> cached_begin_;

            auto find_next(inner_iterator const& it)
            {
                auto sub = RAH_NAMESPACE::search(
                    RAH_NAMESPACE::make_subrange(it, RAH_NAMESPACE::end(base_)), pattern_);
                auto b = sub.begin();
                auto e = sub.end();

                if (b != RAH_NAMESPACE::end(base_) and RAH_NAMESPACE::empty(pattern_))
                {
                    ++b;
                    ++e;
                }

                return RAH_NAMESPACE::make_subrange(b, e);
            }

        public:
            struct sentinel
            {
                inner_sentinel inner_;
            };
            struct iterator
                : iterator_facade<iterator, sentinel, range_reference_t<R>, std::input_iterator_tag>
            {
                split_view* parent_;
                inner_iterator cur_;
                subrange<inner_iterator> next_;
                bool trailing_empty_ = false;

                iterator(split_view* parent, inner_iterator cur, subrange<inner_iterator> next)
                    : parent_(parent)
                    , cur_(cur)
                    , next_(next)
                {
                }

                iterator& operator++()
                {
                    cur_ = next_.begin();

                    if (cur_ != RAH_NAMESPACE::end(parent_->base_))
                    {
                        cur_ = next_.end();
                        if (cur_ == RAH_NAMESPACE::end(parent_->base_))
                        {
                            trailing_empty_ = true;
                            next_ = {cur_, cur_};
                        }
                        else
                            next_ = parent_->find_next(cur_);
                    }
                    else
                        trailing_empty_ = false;

                    return *this;
                }

                auto operator*()
                {
                    return subrange<inner_iterator>{cur_, next_.begin()};
                }

                friend bool operator==(iterator const& x, iterator const& y)
                {
                    return x.cur_ == y.cur_ and x.trailing_empty_ == y.trailing_empty_;
                }
                friend bool operator==(iterator const& x, sentinel const& y)
                {
                    return x.cur_ == y.inner_;
                }
            };

            split_view(R base, P pattern)
                : base_(std::move(base))
                , pattern_(std::move(pattern))
            {
            }

            iterator begin()
            {
                if (!cached_begin_.has_value())
                    cached_begin_ = find_next(RAH_NAMESPACE::begin(base_));
                return iterator(this, RAH_NAMESPACE::begin(base_), *cached_begin_);
            }

            sentinel end()
            {
                return sentinel{RAH_NAMESPACE::end(base_)};
            }
        };

        template <typename R, typename P>
        auto split(R&& range, P&& pattern)
        {
            auto ref = RAH_NAMESPACE::views::all(std::forward<R>(range));
            return split_view<decltype(ref), RAH_NAMESPACE::remove_cvref_t<P>>(
                std::move(ref), std::forward<P>(pattern));
        }

        template <typename P>
        auto split(P&& pattern)
        {
            return make_pipeable(
                [p = std::forward<P>(pattern)](auto&& range)
                { return split(RAH_STD::forward<decltype(range)>(range), std::move(p)); });
        }

        // ******************************************* counted ************************************

        template <typename I>
        struct counted_iterator : iterator_facade<
                                      counted_iterator<I>,
                                      void,
                                      decltype(*details::declval<I>()),
                                      typename RAH_STD::iterator_traits<I>::iterator_category>
        {
            I iter_;
            size_t count_ = size_t();

            counted_iterator() = default;
            counted_iterator(I iter, size_t count)
                : iter_(iter)
                , count_(count)
            {
            }

            counted_iterator& operator++()
            {
                ++iter_;
                ++count_;
                return *this;
            }
            counted_iterator& operator+=(intptr_t off)
            {
                iter_ += off;
                count_ += off;
                return this;
            }
            counted_iterator& operator--()
            {
                --iter_;
                --count_;
                return *this;
            }
            auto operator-(counted_iterator const& r) const
            {
                return count_ - r.count_;
            }
            auto operator*() const -> decltype(*iter_)
            {
                return *iter_;
            }
            bool operator==(counted_iterator const& r) const
            {
                return count_ == r.count_;
            }
        };

        template <typename I>
        auto counted(I&& it, size_t n, decltype(++it, 0) = 0)
        {
            using iterator = counted_iterator<RAH_STD::remove_reference_t<I>>;
            iterator iter1(it, 0);
            iterator iter2(it, n);
            return make_subrange(iter1, iter2);
        }

        // ******************************************* unbounded ******************************************

        // TODO : make it RAH_NAMESPACE::sentinel_range_base

        template <typename I>
        struct unbounded_iterator : iterator_facade<
                                        unbounded_iterator<I>,
                                        void,
                                        decltype(*details::declval<I>()),
                                        typename RAH_STD::iterator_traits<I>::iterator_category>
        {
            I iter_;
            bool end_;

            unbounded_iterator() = default;
            unbounded_iterator(I iter, bool end)
                : iter_(iter)
                , end_(end)
            {
            }

            unbounded_iterator& operator++()
            {
                ++iter_;
                return *this;
            }

            unbounded_iterator& operator+=(intptr_t off)
            {
                iter_ += off;
                return *this;
            }
            unbounded_iterator& operator--()
            {
                --iter_;
            }
            int64_t operator-(unbounded_iterator const& r) const
            {
                if (end_)
                {
                    if (r.end_)
                        return intptr_t{};
                    else
                        return RAH_STD::numeric_limits<intptr_t>::min();
                }
                else
                {
                    if (r.end_)
                        return RAH_STD::numeric_limits<intptr_t>::max();
                    else
                        return iter_ - r.iter_;
                }
            }

            auto operator*() const -> decltype(*iter_)
            {
                return *iter_;
            }
            bool operator==(unbounded_iterator const& r) const
            {
                return end_ ? r.end_ : (r.end_ ? false : r.iter_ == iter_);
            }
        };

        template <typename I>
        auto unbounded(I&& it)
        {
            using iterator = unbounded_iterator<RAH_STD::remove_reference_t<I>>;
            iterator iter1(it, false);
            iterator iter2(it, true);
            return make_subrange(iter1, iter2);
        }

        // ********************************** iota ************************************************

        /// @see rah::iota
        template <typename T>
        struct iota_iterator
            : iterator_facade<iota_iterator<T>, void, T, RAH_STD::random_access_iterator_tag>
        {
            T val_ = T();

            iota_iterator()
            {
            }
            iota_iterator(T val)
                : val_(val)
            {
            }
            ~iota_iterator()
            {
            }

            iota_iterator& operator++()
            {
                ++val_;
                return *this;
            }
            iota_iterator& operator+=(intptr_t value)
            {
                val_ += T(value);
                return *this;
            }
            iota_iterator& operator-=(intptr_t value)
            {
                val_ -= T(value);
                return *this;
            }
            iota_iterator& operator--()
            {
                --val_;
                return *this;
            }
            auto operator-(iota_iterator const& other) const
            {
                return (val_ - other.val_);
            }
            auto operator*() const
            {
                return val_;
            }
            bool operator==(iota_iterator const& other) const
            {
                return val_ == other.val_;
            }
        };

        template <typename T = size_t>
        constexpr auto iota(T start, T stop)
        {

            return subrange<iota_iterator<T>>{start, stop};
        }

        template <typename T = size_t>
        constexpr auto iota(T start = 0)
        {
            // TODO : Make the range to be a RAH_NAMESPACE::sentinel_range_base
            auto stop = start;
            --stop;
            return subrange<iota_iterator<T>>{start, stop};
        }

        // ********************************** irange **********************************************

        /// @see rah::irange
        template <typename T>
        struct irange_iterator
            : iterator_facade<irange_iterator<T>, void, T, RAH_STD::random_access_iterator_tag>
        {
            T val_ = T();
            T step_ = T(1);

            irange_iterator() = default;
            irange_iterator(T val, T step)
                : val_(val)
                , step_(step)
            {
            }

            irange_iterator& operator++()
            {
                val_ += step_;
                return *this;
            }
            irange_iterator& operator+=(intptr_t value)
            {
                val_ += T(step_ * value);
                return *this;
            }
            irange_iterator& operator--()
            {
                val_ -= step_;
                return *this;
            }
            auto operator-(irange_iterator const& other) const
            {
                return (val_ - other.val_) / step_;
            }
            auto operator*() const
            {
                return val_;
            }
            constexpr bool operator==(irange_iterator const& other) const
            {
                return val_ == other.val_;
            }
        };

        template <typename T = size_t>
        auto irange(T start, T stop, T step)
        {
            assert(step != 0);
            auto diff = (stop - start);
            diff = ((diff + (step - 1)) / step) * step;
            return subrange<irange_iterator<T>>{{start, step}, {start + diff, step}};
        }

        template <typename T = size_t>
        auto irange(T start, T step)
        {
            // TODO : Make the range to be a RAH_NAMESPACE::sentinel_range_base
            assert(step != 0);
            auto stop = start;
            --stop;
            return irange(start, stop, step);
        }

        // ********************************** repeat ******************************************************

        // TODO : Make the range to be a RAH_NAMESPACE::sentinel_range_base

        /// @see rah::repeat
        template <typename V>
        struct repeat_iterator
            : iterator_facade<repeat_iterator<V>, void, V const&, RAH_STD::forward_iterator_tag>
        {
            V val_ = V();

            repeat_iterator() = default;
            template <typename U>
            repeat_iterator(U val)
                : val_(RAH_STD::forward<U>(val))
            {
            }
            ~repeat_iterator()
            {
            }

            repeat_iterator& operator++()
            {
                return *this;
            }
            repeat_iterator& operator+(intptr_t value)
            {
                return *this;
            }
            repeat_iterator& operator--()
            {
                return *this;
            }
            V const& operator*() const
            {
                return val_;
            }
            bool operator==(repeat_iterator) const
            {
                return false;
            }
        };

        template <typename V>
        auto repeat(V&& value)
        {
            return subrange<repeat_iterator<RAH_STD::remove_const_t<RAH_STD::remove_reference_t<V>>>>{
                {value}, {value}};
        }

        // ********************************** join ********************************************************

        template <typename R>
        class join_view : public view_interface<join_view<R>>
        {
            R base_;
            using Iterator1 = iterator_t<R>;
            using Iterator2 = sentinel_t<R>;
            using SubRangeType = RAH_NAMESPACE::range_reference_t<R>;
            using SubRangeRefTraits = RAH_NAMESPACE::details::pointer_type<SubRangeType>;
            using SubRangeTypePtr = typename SubRangeRefTraits::type;
            using SubIterBegin = iterator_t<SubRangeType>;
            using SubIterEnd = sentinel_t<SubRangeType>;
            Iterator1 rangeIter_;
            Iterator2 rangeEnd_;
            SubRangeTypePtr subrange_;
            SubIterBegin subRangeIter;
            SubIterEnd subRangeEnd;
            bool init_ = false;

            void next_valid()
            {
                view_interface<join_view<R>>::deleteCheck.check();
                while (subRangeIter == subRangeEnd)
                {
                    ++rangeIter_;
                    if (rangeIter_ == rangeEnd_)
                        return;
                    else
                    {
                        subrange_ = SubRangeRefTraits::to_pointer(*rangeIter_);
                        subRangeIter = RAH_NAMESPACE::begin(*subrange_);
                        subRangeEnd = RAH_NAMESPACE::end(*subrange_);
                    }
                }
            }

        public:
            struct iterator
                : iterator_facade<iterator, void, range_reference_t<range_reference_t<R>>, RAH_STD::forward_iterator_tag>
            {
                // R range_;
                join_view* view_ = nullptr;

                iterator()
                {
                }

                iterator(iterator const&) = default;
                iterator(iterator&&) = default;
                iterator& operator=(iterator const&) = default;
                iterator& operator=(iterator&&) = default;
                ~iterator() = default;

                explicit iterator(join_view* base)
                    : view_(base)
                {
                    if (view_->rangeIter_ == view_->rangeEnd_)
                        return;
                    view_->next_valid();
                }

                iterator& operator++()
                {
                    iterator_facade<
                        iterator,
                        void,
                        range_reference_t<range_reference_t<R>>,
                        RAH_STD::forward_iterator_tag>::deleteCheck.check();
                    view_->view_interface<join_view<R>>::deleteCheck.check();
                    ++view_->subRangeIter;
                    view_->next_valid();
                    iterator_facade<
                        iterator,
                        void,
                        range_reference_t<range_reference_t<R>>,
                        RAH_STD::forward_iterator_tag>::deleteCheck.check();
                    view_->view_interface<join_view<R>>::deleteCheck.check();
                    return *this;
                }
                auto operator*() const -> decltype(*view_->subRangeIter)
                {
                    view_->view_interface<join_view<R>>::deleteCheck.check();
                    return *view_->subRangeIter;
                }
                bool operator==(iterator const& other) const
                {
                    iterator_facade<
                        iterator,
                        void,
                        range_reference_t<range_reference_t<R>>,
                        RAH_STD::forward_iterator_tag>::deleteCheck.check();
                    view_->view_interface<join_view<R>>::deleteCheck.check();
                    if (other.view_ == nullptr)
                    {
                        if (view_ == nullptr)
                            return true;
                        else
                            return view_->rangeIter_ == view_->rangeEnd_;
                    }
                    else if (view_ == nullptr)
                    {
                        return other == *this;
                    }
                    assert(false && "join_view iterator can only be compared to the end");
                    return false;
                }
            };

            join_view() = default;

            join_view(R base)
                : base_(std::move(base))
            {
            }

            join_view(join_view const& v)
                : base_(v.base_)
                , rangeIter_(v.rangeIter_)
                , rangeEnd_(v.rangeEnd_)
                , subrange_(v.subrange_)
                , subRangeIter(v.subRangeIter)
                , subRangeEnd(v.subRangeEnd)
            {
                assert(v.init_ == false);
            }
            join_view& operator=(join_view const& v)
            {
                assert(init_ == false);
                assert(v.init_ == false);
                base_ = v.base_;
                rangeIter_ = v.rangeIter_;
                rangeEnd_ = v.rangeEnd_;
                subrange_ = v.subrange_;
                subRangeIter = v.subRangeIter;
                subRangeEnd = v.subRangeEnd;
            }
            join_view(join_view&& v)
            {
                (*this) = std::move(v);
            }
            join_view& operator=(join_view&& v)
            {
                assert(init_ == false);
                assert(v.init_ == false);
                base_ = std::move(v.base_);
                rangeIter_ = std::move(v.rangeIter_);
                rangeEnd_ = std::move(v.rangeEnd_);
                subrange_ = std::move(v.subrange_);
                subRangeIter = std::move(v.subRangeIter);
                subRangeEnd = std::move(v.subRangeEnd);
                return *this;
            }

            bool empty() const
            {
                return RAH_NAMESPACE::empty(base_);
            }

            auto base() const
            {
                return base_;
            }

            auto begin()
            {
                init_ = true;
                rangeIter_ = RAH_NAMESPACE::begin(base_);
                rangeEnd_ = RAH_NAMESPACE::end(base_);
                subrange_ = SubRangeRefTraits::to_pointer(*rangeIter_);
                subRangeIter = RAH_NAMESPACE::begin(*subrange_);
                subRangeEnd = RAH_NAMESPACE::end(*subrange_);

                return iterator(this);
            }

            auto end()
            {
                return iterator();
            }
        };

        template <typename R>
        auto join(R&& range_of_ranges)
        {
            auto rangeRef = RAH_NAMESPACE::views::all(range_of_ranges);
            return join_view<decltype(rangeRef)>(std::move(rangeRef));
        }

        inline auto join()
        {
            return make_pipeable([](auto&& range) { return join(range); });
        }

        // ********************************** cycle ********************************************************

        template <typename R>
        class cycle_view : public view_interface<cycle_view<R>>
        {
            R base_;
            using base_iterator = iterator_t<R>;
            using base_sentinel = iterator_t<R>;

        public:
            struct sentinel
            {
                base_sentinel sent;
            };

            struct iterator
                : iterator_facade<
                      iterator,
                      sentinel,
                      RAH_NAMESPACE::range_reference_t<R>,
                      common_iterator_tag<RAH_STD::bidirectional_iterator_tag, range_iter_categ_t<R>>>
            {
                cycle_view* view_;
                base_iterator beginIter_;
                base_iterator endIter_;
                base_iterator iter_;

                iterator() = default;
                iterator(cycle_view* v, base_iterator iter)
                    : view_(v)
                    , beginIter_(RAH_NAMESPACE::begin(v->base_))
                    , endIter_(RAH_NAMESPACE::end(v->base_))
                    , iter_(iter)
                {
                }

                iterator& operator++()
                {
                    if (view_ != nullptr)
                    {
                        ++iter_;
                        while (iter_ == endIter_)
                        {
                            iter_ = RAH_NAMESPACE::begin(view_->base_);
                        }
                    }
                    return *this;
                }
                iterator& operator--()
                {
                    assert(view_ != nullptr);
                    while (iter_ == beginIter_)
                    {
                        iter_ = RAH_NAMESPACE::end(view_->base_);
                    }
                    --iter_;
                    return *this;
                }
                auto operator*() const -> decltype(*iter_)
                {
                    assert(view_ != nullptr);
                    return *iter_;
                }
                bool operator==(iterator const& other) const
                {
                    return iter_ == other.iter_;
                }
                bool operator==(sentinel const& other) const
                {
                    return iter_ == other.sent;
                }
            };

            cycle_view() = default;
            cycle_view(R base)
                : base_(std::move(base))
            {
            }

            iterator begin()
            {
                return iterator(this, RAH_NAMESPACE::begin(base_));
            }

            sentinel end() const
            {
                return sentinel{RAH_NAMESPACE::end(base_)};
            }

            auto rbegin()
            {
                return std::make_reverse_iterator(iterator(this, RAH_NAMESPACE::end(base_)));
            }

            sentinel rend()
            {
                return sentinel();
            }
        };

        template <typename R>
        auto cycle(R&& range)
        {
            auto ref = RAH_NAMESPACE::views::all(std::forward<R>(range));
            return cycle_view<decltype(ref)>(std::move(ref));
        }

        inline auto cycle()
        {
            return make_pipeable([](auto&& range)
                                 { return cycle(RAH_STD::forward<decltype(range)>(range)); });
        }

        // ********************************** generate ****************************************************

        // TODO : Make the range to be a RAH_NAMESPACE::sentinel_range_base

        template <typename F>
        struct generate_iterator
            : iterator_facade<generate_iterator<F>, void, decltype(details::declval<F>()()), RAH_STD::forward_iterator_tag>
        {
            mutable RAH_NAMESPACE::details::optional<F> func_;

            generate_iterator()
            {
            }
            generate_iterator(F func)
                : func_(RAH_STD::move(func))
            {
            }

            generate_iterator& operator++()
            {
                return *this;
            }
            decltype((*func_)()) operator*() const
            {
                return (*func_)();
            }
            bool operator==(generate_iterator const&) const
            {
                return false;
            }
        };

        template <typename F>
        auto generate(F&& func)
        {
            using Functor = RAH_STD::remove_cv_t<RAH_STD::remove_reference_t<F>>;
            return subrange<generate_iterator<Functor>>(RAH_STD::forward<F>(func), {});
        }

        template <typename F>
        auto generate_n(size_t count, F&& func)
        {
            return generate(RAH_STD::forward<F>(func)) | take(count);
        }

        // ******************************************* transform ******************************************

        template <typename R, typename F>
        class transform_view : public view_interface<transform_view<R, F>>
        {
            R base_;
            RAH_NAMESPACE::details::optional<F> func_;

        public:
            using reference =
                decltype(details::declval<F>()(details::declval<range_reference_t<R>>()));
            struct iterator : iterator_facade<iterator, void, reference, range_iter_categ_t<R>>
            {
                using difference_type = intptr_t;
                using reference =
                    decltype(details::declval<F>()(details::declval<range_reference_t<R>>()));
                using pointer = typename details::pointer_type<reference>::type;
                using value_type = RAH_STD::remove_reference_t<reference>;
                using iterator_category = range_iter_categ_t<R>;

                iterator_t<R> iter_;
                RAH_NAMESPACE::details::optional<F> func_;

                iterator()
                {
                }
                iterator(iterator_t<R> const& iter, F const& func)
                    : iter_(iter)
                    , func_(func)
                {
                }

                iterator& operator=(iterator const& ot)
                {
                    iter_ = ot.iter_;
                    func_ = ot.func_;
                    return *this;
                }

                iterator& operator++()
                {
                    ++iter_;
                    return *this;
                }
                iterator& operator+=(intptr_t off)
                {
                    iter_ += off;
                    return *this;
                }
                iterator& operator-=(intptr_t off)
                {
                    iter_ -= off;
                    return *this;
                }
                iterator& operator--()
                {
                    --iter_;
                    return *this;
                }
                auto operator-(iterator const& r) const
                {
                    return iter_ - r.iter_;
                }
                auto operator*() const -> decltype((*func_)(*iter_))
                {
                    return (*func_)(*iter_);
                }
                bool operator==(iterator const& r) const
                {
                    return iter_ == r.iter_;
                }
            };

            transform_view() = default;
            transform_view(R base, F func)
                : base_(std::move(base))
                , func_(std::move(func))
            {
            }

            auto begin()
            {
                return iterator(RAH_NAMESPACE::begin(base_), *func_);
            }

            auto end()
            {
                return iterator(RAH_NAMESPACE::end(base_), *func_);
            }
        };

        template <typename R, typename F>
        constexpr auto transform(R&& range, F&& func)
        {
            auto ref = RAH_NAMESPACE::views::all(std::forward<R>(range));
            return transform_view<decltype(ref), RAH_NAMESPACE::remove_cvref_t<F>>(
                std::move(ref), std::forward<F>(func));
        }

        template <typename F>
        constexpr auto transform(F&& func)
        {
            return make_pipeable(
                [=](auto&& range)
                { return transform(RAH_STD::forward<decltype(range)>(range), func); });
        }

        // ******************************************* set_difference *************************************

        template <typename InputIt1, typename InputIt2>
        struct set_difference_iterator : iterator_facade<
                                             set_difference_iterator<InputIt1, InputIt2>,
                                             void,
                                             typename RAH_STD::iterator_traits<InputIt1>::reference,
                                             RAH_STD::forward_iterator_tag>
        {
            InputIt1 first1_;
            InputIt1 last1_;
            InputIt2 first2_;
            InputIt2 last2_;

            set_difference_iterator(InputIt1 first1, InputIt1 last1, InputIt2 first2, InputIt2 last2)
                : first1_(first1)
                , last1_(last1)
                , first2_(first2)
                , last2_(last2)
            {
                next_value();
            }

            void next_value()
            {
                while (first2_ != last2_ and first1_ != last1_)
                {
                    if (*first1_ < *first2_)
                        break;
                    else if (*first1_ == *first2_)
                    {
                        ++first1_;
                        ++first2_;
                    }
                    else
                        ++first2_;
                }
            }

            set_difference_iterator& operator++()
            {
                ++first1_;
                next_value();
                return *this;
            }
            auto operator*() const -> decltype(*first1_)
            {
                return *first1_;
            }
            bool operator==(set_difference_iterator const& r) const
            {
                return first1_ == r.first1_;
            }
        };

        template <typename R1, typename R2>
        auto set_difference(R1&& range1, R2&& range2)
        {
            using Iter1 = iterator_t<R1>;
            using Iter2 = iterator_t<R2>;
            using Iterator = set_difference_iterator<Iter1, Iter2>;
            return subrange<Iterator>{
                {Iterator(
                    RAH_NAMESPACE::begin(range1),
                    RAH_NAMESPACE::end(range1),
                    RAH_NAMESPACE::begin(range2),
                    RAH_NAMESPACE::end(range2))},
                {Iterator(
                    RAH_NAMESPACE::end(range1),
                    RAH_NAMESPACE::end(range1),
                    RAH_NAMESPACE::end(range2),
                    RAH_NAMESPACE::end(range2))},
            };
        }

        template <typename R2>
        auto set_difference(R2&& range2)
        {
            return make_pipeable(
                [r2 = range2 | views::all()](auto&& range)
                { return set_difference(RAH_STD::forward<decltype(range)>(range), r2); });
        }

        // ********************************** for_each ****************************************************

        template <typename R, typename F>
        auto for_each(R&& range, F&& func)
        {
            return range | RAH_NAMESPACE::views::transform(func) | RAH_NAMESPACE::views::join();
        }

        template <typename F>
        inline auto for_each(F&& func)
        {
            return make_pipeable(
                [=](auto&& range) {
                    return RAH_NAMESPACE::views::for_each(
                        RAH_STD::forward<decltype(range)>(range), func);
                });
        }

        // ***************************************** slice ************************************************

        template <typename R>
        class slice_view : view_interface<slice_view<R>>
        {
            R base_;
            intptr_t begin_idx_;
            intptr_t end_idx_;
            using iterator = RAH_NAMESPACE::iterator_t<R>;

            static auto find_iter(iterator iter, intptr_t idx)
            {
                assert(idx >= 0 && "Can't use negative index");
                RAH_STD::advance(iter, idx);
                return iter;
            }

        public:
            slice_view() = default;
            slice_view(R v, intptr_t begin_idx, intptr_t end_idx)
                : base_(std::move(v))
                , begin_idx_(begin_idx)
                , end_idx_(end_idx)
            {
            }

            auto begin()
            {
                return find_iter(RAH_NAMESPACE::begin(base_), begin_idx_);
            }
            auto end()
            {
                return find_iter(RAH_NAMESPACE::begin(base_), end_idx_);
            }
        };

        template <typename R>
        auto slice(R&& range, intptr_t begin_idx, intptr_t end_idx)
        {
            static_assert(
                not RAH_STD::is_same<range_iter_categ_t<R>, RAH_STD::forward_iterator_tag>::value,
                "Can't use slice on non-bidirectional iterators. Try to use views::drop and "
                "views::take");
            auto ref = RAH_NAMESPACE::views::all(std::forward<R>(range));
            return slice_view<decltype(ref)>{std::move(ref), begin_idx, end_idx};
        }

        inline auto slice(intptr_t begin, intptr_t end)
        {
            return make_pipeable(
                [=](auto&& range)
                { return slice(RAH_STD::forward<decltype(range)>(range), begin, end); });
        }

        // ***************************************** stride ***********************************************

        template <typename R>
        struct stride_iterator
            : iterator_facade<stride_iterator<R>, void, range_reference_t<R>, range_iter_categ_t<R>>
        {
            iterator_t<R> iter_;
            sentinel_t<R> end_;
            size_t step_;

            stride_iterator(iterator_t<R> const& iter, sentinel_t<R> const& end, size_t step)
                : iter_(iter)
                , end_(end)
                , step_(step)
            {
            }

            stride_iterator& operator++()
            {
                for (size_t i = 0; i < step_ && iter_ != end_; ++i)
                    ++iter_;
                return *this;
            }

            stride_iterator& operator--()
            {
                for (size_t i = 0; i < step_; ++i)
                    --iter_;
                return *this;
            }

            stride_iterator& operator+=(intptr_t value)
            {
                iter_ += step_ * value;
                return *this;
            }
            auto operator*() const -> decltype(*iter_)
            {
                return *iter_;
            }
            bool operator==(stride_iterator const& other) const
            {
                return iter_ == other.iter_;
            }
            auto operator-(stride_iterator const& other) const
            {
                return (iter_ - other.iter_) / step_;
            }
        };

        template <typename R>
        auto stride(R&& range, size_t step)
        {
            auto&& views = all(RAH_STD::forward<R>(range));
            auto iter = RAH_NAMESPACE::begin(views);
            auto endIter = RAH_NAMESPACE::end(views);
            return subrange<stride_iterator<RAH_STD::remove_reference_t<R>>>{
                {iter, endIter, step}, {endIter, endIter, step}};
        }

        inline auto stride(size_t step)
        {
            return make_pipeable([=](auto&& range)
                                 { return stride(RAH_STD::forward<decltype(range)>(range), step); });
        }

        // ***************************************** reverse **********************************************

        // TODO : Make the range to be a RAH_NAMESPACE::sentinel_range_base IF NEEDED

        template <typename R>
        class reverse_view : public view_interface<reverse_view<R>>
        {
            R base_;

        public:
            reverse_view() = default;
            reverse_view(R base)
                : base_(std::move(base))
            {
            }

            auto begin()
            {
                return RAH_NAMESPACE::rbegin(base_);
            }

            auto end()
            {
                return RAH_NAMESPACE::rend(base_);
            }
        };

        template <typename R>
        auto reverse(R&& range)
        {
            static_assert(
                RAH_NAMESPACE::bidirectional_range<R>, "reverse expect a bidirectional_range");
            auto ref = RAH_NAMESPACE::views::all(std::forward<R>(range));
            return reverse_view<decltype(ref)>(std::move(ref));
            // static_assert(
            //    RAH_STD::is_same_v<iterator_t<R>, sentinel_t<R>>,
            //    "reverse range can't apply on iterator/sentinal range");
            //return make_subrange(
            //    RAH_STD::make_reverse_iterator(RAH_NAMESPACE::end(range)),
            //    RAH_STD::make_reverse_iterator(RAH_NAMESPACE::begin(range)));
        }

        inline auto reverse()
        {
            return make_pipeable([=](auto&& range)
                                 { return reverse(RAH_STD::forward<decltype(range)>(range)); });
        }

        // ********************************** empty ******************************************************

        template <typename T>
        struct empty_iter : iterator_facade<empty_iter<T>, void, T&, RAH_STD::forward_iterator_tag>
        {
            empty_iter& operator++()
            {
                assert(false);
                return *this;
            }
            T& operator*() const
            {
                terminate();
            }
            constexpr bool operator==(empty_iter) const
            {
                return true;
            }
        };

        template <typename T>
        auto empty()
        {
            return subrange<empty_iter<T>>();
        }

        // ********************************** single ******************************************************

        template <typename V>
        auto single(V&& value)
        {
            return repeat(value) | take(1);
        }

        // *************************** zip ****************************************************************
        /// \cond PRIVATE
        namespace details
        {
            template <typename Tuple, typename F, size_t... Indices>
            void for_each_impl(Tuple&& tuple, F&& f, RAH_STD::index_sequence<Indices...>)
            {
                using swallow = int[];
                (void)swallow{
                    1, (f(RAH_STD::get<Indices>(RAH_STD::forward<Tuple>(tuple))), void(), int{})...};
            }

            template <typename Tuple, typename F>
            void for_each(Tuple&& tuple, F&& f)
            {
                constexpr size_t N = RAH_STD::tuple_size<RAH_STD::remove_reference_t<Tuple>>::value;
                for_each_impl(
                    RAH_STD::forward<Tuple>(tuple),
                    RAH_STD::forward<F>(f),
                    RAH_STD::make_index_sequence<N>{});
            }

            template <class F, typename... Args, size_t... Is>
            auto transform_each_impl(
                const RAH_STD::tuple<Args...>& t, F&& f, RAH_STD::index_sequence<Is...>)
            {
                return RAH_STD::make_tuple(f(RAH_STD::get<Is>(t))...);
            }

            template <class F, typename... Args, size_t... Is>
            auto transform_each_impl(RAH_STD::tuple<Args...>& t, F&& f, RAH_STD::index_sequence<Is...>)
            {
                return RAH_STD::make_tuple(f(RAH_STD::get<Is>(t))...);
            }

            template <class F, typename... Args>
            auto transform_each(const RAH_STD::tuple<Args...>& t, F&& f)
            {
                return transform_each_impl(
                    t, RAH_STD::forward<F>(f), RAH_STD::make_index_sequence<sizeof...(Args)>{});
            }

            template <class F, typename... Args>
            auto transform_each(RAH_STD::tuple<Args...>& t, F&& f)
            {
                return transform_each_impl(
                    t, RAH_STD::forward<F>(f), RAH_STD::make_index_sequence<sizeof...(Args)>{});
            }

            template <typename... Args, size_t... Is>
            auto deref_impl(const RAH_STD::tuple<Args...>& t, RAH_STD::index_sequence<Is...>)
            {
                return RAH_STD::tuple<typename RAH_STD::iterator_traits<Args>::reference...>(
                    (*RAH_STD::get<Is>(t))...);
            }

            template <typename... Args>
            auto deref(const RAH_STD::tuple<Args...>& t)
            {
                return deref_impl(t, RAH_STD::make_index_sequence<sizeof...(Args)>{});
            }

            template <size_t Index>
            struct Equal
            {
                template <typename... Args, typename... Args2>
                bool operator()(RAH_STD::tuple<Args...> const& a, RAH_STD::tuple<Args2...> const& b) const
                {
                    return (RAH_STD::get<Index - 1>(a) == RAH_STD::get<Index - 1>(b))
                           || Equal<Index - 1>{}(a, b);
                }
            };

            template <>
            struct Equal<0>
            {
                template <typename... Args, typename... Args2>
                bool operator()(RAH_STD::tuple<Args...> const&, RAH_STD::tuple<Args2...> const&) const
                {
                    return false;
                }
            };

            template <typename... Args, typename... Args2>
            auto equal(RAH_STD::tuple<Args...> const& a, RAH_STD::tuple<Args2...> const& b)
            {
                return Equal<sizeof...(Args)>{}(a, b);
            }

            template <typename... Args, size_t... Is>
            auto get_begin_tuple_impl(const RAH_STD::tuple<Args...>& t, RAH_STD::index_sequence<Is...>)
            {
                return RAH_STD::make_tuple((RAH_NAMESPACE::begin(RAH_STD::get<Is>(t)))...);
            }

            template <typename... Args>
            auto get_begin_tuple(RAH_STD::tuple<Args...> const& a)
            {
                return get_begin_tuple_impl(a, RAH_STD::make_index_sequence<sizeof...(Args)>{});
            }

            template <typename... Args, size_t... Is>
            auto get_end_tuple_impl(const RAH_STD::tuple<Args...>& t, RAH_STD::index_sequence<Is...>)
            {
                return RAH_STD::make_tuple((RAH_NAMESPACE::end(RAH_STD::get<Is>(t)))...);
            }

            template <typename... Args>
            auto get_end_tuple(RAH_STD::tuple<Args...> const& a)
            {
                return get_end_tuple_impl(a, RAH_STD::make_index_sequence<sizeof...(Args)>{});
            }

        } // namespace details
        /// \endcond

        // RAH_STD::make_tuple(RAH_NAMESPACE::begin(details::declval<Ranges>())...)
        template <typename RangeTuple>
        class zip_view : public view_interface<zip_view<RangeTuple>>
        {
            // Can't use a lambda in decltype, but can use a functor
            struct range_begin
            {
                template <typename R>
                auto operator()(R&& r) const
                {
                    return RAH_NAMESPACE::begin(r);
                }
            };
            struct range_end
            {
                template <typename R>
                auto operator()(R&& r) const
                {
                    return RAH_NAMESPACE::end(r);
                }
            };
            RangeTuple bases_;
            using IterTuple = decltype(details::transform_each(bases_, range_begin()));

        public:
            struct iterator
                : iterator_facade<
                      iterator,
                      void,
                      decltype(details::deref(RAH_NAMESPACE::details::declval<IterTuple>())),
                      RAH_STD::bidirectional_iterator_tag>
            {
                IterTuple iters_;
                iterator()
                {
                }
                iterator(IterTuple const& iters)
                    : iters_(iters)
                {
                }
                iterator& operator++()
                {
                    details::for_each(iters_, [](auto& iter) { ++iter; });
                    // details::for_each(iters_, [](auto& iter) { iter.deleteCheck.check(); });
                    return *this;
                }
                iterator& operator+=(intptr_t val)
                {
                    for_each(iters_, [val](auto& iter) { iter += val; });
                    return *this;
                }
                iterator& operator--()
                {
                    details::for_each(iters_, [](auto& iter) { --iter; });
                    return *this;
                }
                auto operator*() const
                {
                    return details::deref(iters_);
                }
                auto operator-(iterator const& other) const
                {
                    return RAH_STD::get<0>(iters_) - RAH_STD::get<0>(other.iters_);
                }
                bool operator==(iterator const& other) const
                {
                    return details::equal(iters_, other.iters_);
                }
            };

            zip_view(RangeTuple rangeTuple)
                : bases_(RAH_STD::move(rangeTuple))
            {
            }

            auto begin()
            {
                view_interface<zip_view<RangeTuple>>::deleteCheck.check();
                return iterator(details::transform_each(bases_, range_begin()));
            }

            auto end()
            {
                view_interface<zip_view<RangeTuple>>::deleteCheck.check();
                return iterator(details::transform_each(bases_, range_end()));
            }
        };

        template <typename... R>
        auto zip(R&&... ranges)
        {
            auto refTuple =
                RAH_STD::make_tuple(RAH_NAMESPACE::views::all(RAH_STD::forward<R>(ranges))...);
            return zip_view<decltype(refTuple)>(RAH_STD::move(refTuple));
        }

        // ************************************ chunk *****************************************************

        template <typename R>
        struct chunk_iterator
            : iterator_facade<chunk_iterator<R>, void, subrange<iterator_t<R>>, RAH_STD::forward_iterator_tag>
        {
            iterator_t<R> iter_;
            iterator_t<R> iter2_;
            sentinel_t<R> end_;
            size_t step_;

            chunk_iterator(
                iterator_t<R> const& iter,
                iterator_t<R> const& iter2,
                sentinel_t<R> const& end,
                size_t step = 0)
                : iter_(iter)
                , iter2_(iter2)
                , end_(end)
                , step_(step)
            {
            }

            chunk_iterator& operator++()
            {
                iter_ = iter2_;
                for (size_t i = 0; i != step_ and iter2_ != end_; ++i)
                    ++iter2_;
                return *this;
            }

            auto operator*() const
            {
                return make_subrange(iter_, iter2_);
            }
            bool operator==(chunk_iterator const& other) const
            {
                return iter_ == other.iter_;
            }
        };

        template <typename R, RAH_STD::enable_if_t<not RAH_STD::is_rvalue_reference_v<R&&>, int> = 0>
        auto chunk(R&& range, size_t step)
        {
            auto iter = RAH_NAMESPACE::begin(range);
            auto endIter = RAH_NAMESPACE::end(range);
            using iterator = chunk_iterator<RAH_STD::remove_reference_t<R>>;
            iterator begin = {iter, iter, endIter, step};
            ++begin;
            return subrange<iterator>{{begin}, {endIter, endIter, endIter, step}};
        }

        inline auto chunk(size_t step)
        {
            return make_pipeable([=](auto&& range)
                                 { return chunk(RAH_STD::forward<decltype(range)>(range), step); });
        }

        // ***************************************** filter ***********************************************

        template <typename R, typename P>
        class filter_view : public view_interface<filter_view<R, P>>
        {
            R range_;
            P pred_;
            using I = RAH_NAMESPACE::iterator_t<R>;

        public:
            struct iterator
                : iterator_facade<iterator, void, typename RAH_STD::iterator_traits<I>::reference, RAH_STD::bidirectional_iterator_tag>
            {
                filter_view* view_;
                I iter_;
                typename RAH_STD::iterator_traits<I>::pointer value_pointer_;

                // Get a pointer to the pointed value,
                //   OR a pointer to a copy of the pointed value (when not a reference iterator)
                template <class It>
                struct get_pointer
                {
                    static auto get(It const& iter)
                    {
                        return iter.operator->();
                    }
                };
                template <class V>
                struct get_pointer<V*>
                {
                    static auto get(V* ptr)
                    {
                        return ptr;
                    }
                };

                iterator() = default;

                iterator(filter_view* v, I iter)
                    : view_(v)
                    , iter_(RAH_STD::move(iter))
                {
                    next_value();
                }

                void next_value()
                {
                    while (iter_ != RAH_NAMESPACE::end(view_->range_)
                           && not(view_->pred_)(*(value_pointer_ = get_pointer<I>::get(iter_))))
                    {
                        assert(iter_ != RAH_NAMESPACE::end(view_->range_));
                        ++iter_;
                    }
                }

                iterator& operator++()
                {
                    ++iter_;
                    next_value();
                    return *this;
                }

                iterator& operator--()
                {
                    do
                    {
                        --iter_;
                    } while (not(view_->pred_)(*iter_)
                             && iter_ != RAH_NAMESPACE::begin(view_->range_));
                    return *this;
                }

                auto operator*() const -> decltype(*iter_)
                {
                    return *value_pointer_;
                }
                bool operator==(iterator const& other) const
                {
                    return iter_ == other.iter_;
                }
            };

            filter_view(R rng, P pred)
                : range_(std::move(rng))
                , pred_(std::move(pred))
            {
            }

            R base() const
            {
                return range_;
            }

            P const& pred() const
            {
                return pred_;
            }

            auto begin()
            {
                return iterator(this, RAH_NAMESPACE::begin(range_));
            }

            auto end()
            {
                return iterator{this, RAH_NAMESPACE::end(range_)};
            }

            auto begin() const
            {
                return iterator(this, RAH_NAMESPACE::begin(range_));
            }

            auto end() const
            {
                return iterator{this, RAH_NAMESPACE::end(range_)};
            }
        };

        template <typename R, typename P>
        auto filter(R&& range, P&& pred)
        {
            auto view_ref = all(RAH_STD::forward<R>(range));
            return filter_view<decltype(view_ref), RAH_NAMESPACE::remove_cvref_t<P>>(
                RAH_STD::move(view_ref), RAH_STD::forward<P>(pred));
        }

        template <typename P>
        auto filter(P&& pred)
        {
            return make_pipeable([=](auto&& range)
                                 { return filter(RAH_STD::forward<decltype(range)>(range), pred); });
        }

        // ***************************************** concat ***********************************************

        template <typename IterPair, typename V>
        struct concat_iterator
            : iterator_facade<concat_iterator<IterPair, V>, void, V, RAH_STD::forward_iterator_tag>
        {
            IterPair iter_;
            IterPair end_;
            size_t range_index_;

            concat_iterator(IterPair const& iter, IterPair const& end, size_t range_index)
                : iter_(iter)
                , end_(end)
                , range_index_(range_index)
            {
                if (range_index == 0)
                {
                    if (RAH_STD::get<0>(iter_) == RAH_STD::get<0>(end_))
                        range_index_ = 1;
                }
            }

            concat_iterator& operator++()
            {
                if (range_index_ == 0)
                {
                    auto& i = RAH_STD::get<0>(iter_);
                    ++i;
                    if (i == RAH_STD::get<0>(end_))
                        range_index_ = 1;
                }
                else
                    ++RAH_STD::get<1>(iter_);
                return *this;
            }

            auto operator*() const -> decltype(*RAH_STD::get<0>(iter_))
            {
                if (range_index_ == 0)
                    return *RAH_STD::get<0>(iter_);
                else
                    return *RAH_STD::get<1>(iter_);
            }

            bool operator==(concat_iterator const& other) const
            {
                if (range_index_ != other.range_index_)
                    return false;
                if (range_index_ == 0)
                    return RAH_STD::get<0>(iter_) == RAH_STD::get<0>(other.iter_);
                else
                    return RAH_STD::get<1>(iter_) == RAH_STD::get<1>(other.iter_);
            }
        };

        /// @brief return the same range
        template <typename R1>
        auto concat(R1&& range1)
        {
            return RAH_STD::forward<R1>(range1);
        }

        template <typename R1, typename R2>
        auto concat(R1&& range1, R2&& range2)
        {
            auto begin_range1 =
                RAH_STD::make_pair(RAH_NAMESPACE::begin(range1), RAH_NAMESPACE::begin(range2));
            auto begin_range2 =
                RAH_STD::make_pair(RAH_NAMESPACE::end(range1), RAH_NAMESPACE::end(range2));
            auto end_range1 =
                RAH_STD::make_pair(RAH_NAMESPACE::end(range1), RAH_NAMESPACE::end(range2));
            auto end_range2 =
                RAH_STD::make_pair(RAH_NAMESPACE::end(range1), RAH_NAMESPACE::end(range2));
            return subrange<
                concat_iterator<RAH_STD::pair<iterator_t<R1>, iterator_t<R2>>, range_reference_t<R1>>>{
                {begin_range1, begin_range2, 0},
                {end_range1, end_range2, 1},
            };
        }

        /// @see rah::views::concat(R1&& range1, R2&& range2)
        template <typename R1, typename R2, typename... Ranges>
        auto concat(R1&& range1, R2&& range2, Ranges&&... ranges)
        {
            return concat(
                concat(RAH_STD::forward<R1>(range1), RAH_STD::forward<R2>(range2)), ranges...);
        }

        // *************************** enumerate **********************************************************

        template <typename R> //, RAH_STD::enable_if_t<not RAH_STD::is_rvalue_reference_v<R&&>, int> = 0>
        auto enumerate(R&& range)
        {
            auto views = all(range);
            size_t const dist =
                RAH_STD::distance(RAH_NAMESPACE::begin(views), RAH_NAMESPACE::end(views));
            return zip(iota(size_t(0), dist), views);
        }

        inline auto enumerate()
        {
            return make_pipeable([](auto&& range)
                                 { return enumerate(RAH_STD::forward<decltype(range)>(range)); });
        }

        // ****************************** map_value ********************************************************

        template <size_t I>
        struct get_tuple_elt
        {
            template <typename T>
            auto operator()(T&& nvp) const
                -> decltype(RAH_STD::get<I>(RAH_STD::forward<decltype(nvp)>(nvp)))
            {
                static_assert(
                    not RAH_STD::is_rvalue_reference<decltype(nvp)>::value,
                    "map_value/map_key only apply only apply on lvalue pairs. "
                    "Pairs from map are ok but for generated pairs, prefer use "
                    "views::tranform");
                return RAH_STD::get<I>(RAH_STD::forward<decltype(nvp)>(nvp));
            }
        };

        template <typename R>
        auto map_value(R&& range)
        {
            return transform(RAH_STD::forward<R>(range), get_tuple_elt<1>{});
        }

        inline auto map_value()
        {
            return make_pipeable([=](auto&& range)
                                 { return map_value(RAH_STD::forward<decltype(range)>(range)); });
        }

        // ****************************** map_key **********************************************************

        template <typename R>
        auto map_key(R&& range)
        {
            return RAH_NAMESPACE::views::transform(RAH_STD::forward<R>(range), get_tuple_elt<0>{});
        }

        inline auto map_key()
        {
            return make_pipeable([=](auto&& range)
                                 { return map_key(RAH_STD::forward<decltype(range)>(range)); });
        }
    } // namespace views

} // namespace RAH_NAMESPACE
