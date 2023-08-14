//
// Copyright (c) 2019 Loïc HAMOT
//
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//
#pragma once

#include "range_bases.hpp"

#include <istream>

#ifdef RAH2_USE_EASTL

#include <EASTL/tuple.h>
#include <EASTL/utility.h>

#else

#include <tuple>
#include <utility>

#endif

#ifdef _MSC_VER
#define RAH2_EXT_WARNING_PUSH __pragma(warning(push, 0))
#define RAH2_EXT_WARNING_POP __pragma(warning(pop))
#elif defined(__GNUC__) || defined(__clang__)
#define RAH2_EXT_WARNING_PUSH _Pragma("GCC diagnostic push")
#define RAH2_EXT_WARNING_POP _Pragma("GCC diagnostic pop")
#else
#define RAH2_EXT_WARNING_PUSH
#define RAH2_EXT_WARNING_POP
#endif

RAH2_EXT_WARNING_PUSH
#include "mpark/variant.hpp"
RAH2_EXT_WARNING_POP

namespace RAH2_NS
{
    namespace ranges
    {
        // **************************** Range conversions *********************************************

        /// @brief Return a container of type C, filled with the content of range
        ///
        /// @snippet test.cpp RAH2_NS::to
        template <
            typename C,
            typename R,
            decltype(RAH2_STD::declval<C>().push_back(RAH2_STD::declval<range_reference_t<R>>()))* = nullptr>
        auto to(R&& range)
        {
            C container;
            auto iter = begin(range);
            auto sent = end(range);
            for (; iter != sent; ++iter)
            {
                auto&& elt = *iter;
                container.push_back(RAH2_STD::forward<decltype(elt)>(elt));
            }
            return container;
        }

        template <
            typename C,
            typename R,
            decltype(RAH2_STD::declval<C>().insert(RAH2_STD::declval<range_reference_t<R>>()))* = nullptr>
        auto to(R&& range)
        {
            C container;
            for (auto&& elt : range)
            {
                container.insert(RAH2_STD::forward<decltype(elt)>(elt));
            }
            return container;
        }

        /// @brief Return a container of type C, filled with the content of range
        /// @remark pipeable syntax
        ///
        /// @snippet test.cpp RAH2_NS::to_container_pipeable
        template <typename C>
        auto to()
        {
            return make_pipeable([](auto&& range)
                                 { return to<C>(RAH2_STD::forward<decltype(range)>(range)); });
        }

        // **************************************** pipeable **********************************************

        template <typename Func>
        struct pipeable
        {
            Func func;

            explicit pipeable(Func f)
                : func(RAH2_STD::move(f))
            {
            }
            pipeable(pipeable const&) = delete;
            pipeable(pipeable&&) noexcept = default;
            pipeable& operator=(pipeable const&) = delete;
            pipeable& operator=(pipeable&&) = delete;
        };

        template <typename MakeRange>
        auto make_pipeable(MakeRange&& make_range)
        {
            return pipeable<MakeRange>{RAH2_STD::forward<MakeRange>(make_range)};
        }

        template <typename R, typename MakeRange>
        auto operator|(R&& range, pipeable<MakeRange> const& adapter)
            -> decltype(adapter.func(RAH2_STD::forward<R>(range)))
        {
            return adapter.func(RAH2_STD::forward<R>(range));
        }

        // ************************************ iterator_facade *******************************************

        namespace details
        {
            template <class Reference>
            struct pointer_type
            {
                using type = RAH2_NS::details::optional<Reference>;
                template <typename Ref>
                static type to_pointer(Ref&& ref)
                {
                    return type(RAH2_STD::forward<Ref>(ref));
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

#undef RAH2_SELF
#undef RAH2_SELF_CONST
#define RAH2_SELF (*static_cast<I*>(this))
#define RAH2_SELF_CONST (*static_cast<I const*>(this))

#define RAH2_POST_INCR(CAT)                                                                        \
    template <                                                                                     \
        typename C = CAT,                                                                          \
        RAH2_STD::enable_if_t<                                                                     \
            RAH2_NS::derived_from<C, RAH2_NS::forward_iterator_tag>                                \
            or RAH2_NS::derived_from<C, RAH2_NS::output_iterator_tag>>* = nullptr>                 \
    auto operator++(int)                                                                           \
    {                                                                                              \
        auto it = *this;                                                                           \
        ++(*this);                                                                                 \
        return it;                                                                                 \
    }                                                                                              \
    template <                                                                                     \
        typename C = CAT,                                                                          \
        RAH2_STD::enable_if_t<                                                                     \
            !RAH2_NS::derived_from<C, RAH2_NS::forward_iterator_tag>                               \
            and !RAH2_NS::derived_from<C, RAH2_NS::output_iterator_tag>>* = nullptr>               \
    void operator++(int)                                                                           \
    {                                                                                              \
        ++(*this);                                                                                 \
    }

#define RAH2_POST_DECR                                                                             \
    auto operator--(int)                                                                           \
    {                                                                                              \
        auto it = *this;                                                                           \
        --(*this);                                                                                 \
        return it;                                                                                 \
    }

        struct sentinel_iterator
        {
        };

        template <typename I, typename S, typename R, typename C>
        struct iterator_facade;

        template <typename I, typename S, typename R>
        struct iterator_facade<I, S, R, RAH2_STD::input_iterator_tag>
        {
            using iterator_category = RAH2_STD::input_iterator_tag;
            using value_type = RAH2_NS::remove_cvref_t<R>;
            using difference_type = intptr_t;
            using pointer = RAH2_STD::remove_reference_t<R>*;
            using reference = R;

            static_assert(
                not RAH2_STD::is_reference<value_type>::value, "value_type can't be a reference");

#if !RAH2_CPP20
            template <
                typename Sent = S,
                RAH2_STD::enable_if_t<!RAH2_NS::is_same_v<Sent, void> and !RAH2_NS::is_same_v<Sent, I>>* = nullptr>
            friend bool operator!=(Sent const& sent, I const& it)
            {
                return !(it == sent);
            }
            template <
                typename Sent = S,
                RAH2_STD::enable_if_t<!RAH2_NS::is_same_v<Sent, void> and !RAH2_NS::is_same_v<Sent, I>>* = nullptr>
            friend bool operator!=(I const& it, Sent const& sent)
            {
                return !(it == sent);
            }
#endif

            //bool operator!=(S const& rho) const
            //{
            //    return !(RAH2_SELF_CONST == rho);
            //}
            template <typename Ref = reference, RAH2_STD::enable_if_t<RAH2_NS::is_reference_v<Ref>>* = nullptr>
            pointer operator->()
            {
                return &(*RAH2_SELF);
            }
        };

        template <typename I, typename S, typename R>
        struct iterator_facade<I, S, R, RAH2_STD::forward_iterator_tag>
            : iterator_facade<I, S, R, RAH2_STD::input_iterator_tag>
        {
            using iterator_category = RAH2_STD::forward_iterator_tag;

            friend bool operator!=(I const& it1, I const& it2)
            {
                return !(it1 == it2);
            }
        };

        template <typename I, typename S, typename R>
        struct iterator_facade<I, S, R, RAH2_STD::output_iterator_tag>
        {
            using iterator_category = RAH2_STD::output_iterator_tag;
            using value_type = RAH2_STD::remove_reference_t<R>;
            using difference_type = intptr_t;
            using pointer = value_type*;
            using reference = R;

            static_assert(not RAH2_NS::is_reference_v<value_type>, "value_type can't be a reference");

            I& operator++()
            {
                return RAH2_SELF;
            }
            I operator++(int)
            {
                auto it = *this;
                ++(*this);
                return it;
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
                RAH2_SELF_CONST.put(RAH2_STD::forward<V>(value));
                return RAH2_SELF_CONST;
            }
        };

        template <typename I, typename S, typename R>
        struct iterator_facade<I, S, R, RAH2_STD::bidirectional_iterator_tag>
            : iterator_facade<I, S, R, RAH2_STD::forward_iterator_tag>
        {
            using iterator_category = RAH2_STD::bidirectional_iterator_tag;
        };

        template <typename I, typename S, typename R>
        struct iterator_facade<I, S, R, RAH2_STD::random_access_iterator_tag>
            : iterator_facade<I, S, R, RAH2_STD::bidirectional_iterator_tag>
        {
            using iterator_category = RAH2_STD::random_access_iterator_tag;

            friend bool operator<=(I const& it1, I const& it2)
            {
                return (it1 < it2) || (it1 == it2);
            }
            friend bool operator>(I const& it1, I const& it2)
            {
                return !((it1 < it2) || (it1 == it2));
            }
            friend bool operator>=(I const& it1, I const& it2)
            {
                return !(it1 < it2);
            }
            I& operator-=(intptr_t amount)
            {
                RAH2_SELF += -amount;
                return *this;
            }
            friend I operator-(I const& it, intptr_t amount)
            {
                auto cpy = it;
                cpy += -amount;
                return cpy;
            }
            friend I operator+(I const& it, intptr_t amount)
            {
                auto cpy = it;
                cpy += amount;
                return cpy;
            }
            friend I operator+(intptr_t amount, I const& it)
            {
                auto cpy = it;
                cpy += amount;
                return cpy;
            }
            R operator[](intptr_t index)
            {
                auto cpy = RAH2_SELF;
                cpy += index;
                return *cpy;
            }
            R operator[](intptr_t index) const
            {
                auto cpy = RAH2_SELF_CONST;
                cpy += index;
                return *cpy;
            }
        };

        template <typename I, typename S, typename R>
        struct iterator_facade<I, S, R, RAH2_NS::contiguous_iterator_tag>
            : iterator_facade<I, S, R, RAH2_STD::random_access_iterator_tag>
        {
            using iterator_category = RAH2_NS::contiguous_iterator_tag;
        };
    } // namespace ranges

    template <typename I>
    class counted_iterator : public ranges::iterator_facade<
                                 counted_iterator<I>,
                                 RAH2_NS::default_sentinel_t,
                                 decltype(*RAH2_STD::declval<I>()),
                                 typename RAH2_STD::iterator_traits<I>::iterator_category>
    {
        I iter_;
        iter_difference_t<I> count_ = {};
        using base_cat = typename RAH2_STD::iterator_traits<I>::iterator_category;

    public:
        counted_iterator() = default;
        counted_iterator(I iter, iter_difference_t<I> count_)
            : iter_(RAH2_STD::move(iter))
            , count_(count_)
        {
        }

        counted_iterator& operator++()
        {
            ++iter_;
            --count_;
            return *this;
        }
        RAH2_POST_INCR(base_cat)
        counted_iterator& operator+=(iter_difference_t<I> off)
        {
            iter_ += off;
            count_ -= off;
            return *this;
        }
        template <typename U = I, RAH2_STD::enable_if_t<RAH2_NS::bidirectional_iterator<U>>* = nullptr>
        counted_iterator& operator--()
        {
            --iter_;
            ++count_;
            return *this;
        }
        template <typename U = I, RAH2_STD::enable_if_t<RAH2_NS::bidirectional_iterator<U>>* = nullptr>
        RAH2_POST_DECR;
        auto operator-(counted_iterator const& r) const
        {
            return r.count_ - count_;
        }
        auto operator*() -> decltype(*iter_)
        {
            return *iter_;
        }
        I base() const
        {
            return iter_;
        }
        iter_difference_t<I> count() const
        {
            return count_;
        }
        friend bool operator==(counted_iterator const& it1, counted_iterator const& it2)
        {
            return it1.count_ == it2.count_;
        }
        friend bool operator==(counted_iterator const& it, default_sentinel_t const&)
        {
            return it.count_ == 0;
        }
        friend bool operator==(default_sentinel_t const&, counted_iterator const& it)
        {
            return it.count_ == 0;
        }
        friend bool operator<(counted_iterator const& it1, counted_iterator const& it2)
        {
            return it1.count_ > it2.count_;
        }
    };

    template <typename I>
    auto make_counted_iterator(I it, iter_difference_t<I> count)
    {
        return counted_iterator<I>{RAH2_STD::move(it), count};
    }

    namespace ranges
    {
        // ********************************** empty ***********************************************
        template <typename T>
        class empty_view : public view_interface<empty_view<T>>
        {
        public:
            // ReSharper disable CppMemberFunctionMayBeStatic
            T* begin() const
            {
                return nullptr;
            }
            T* end() const
            {
                return nullptr;
            }
            T* data() const
            {
                return nullptr;
            }
            size_t size() const
            {
                return 0;
            }
            bool empty() const
            {
                return true;
            }
            // ReSharper restore CppMemberFunctionMayBeStatic
        };
        namespace views
        {
            template <class T>
            constexpr empty_view<T> empty{};
        } // namespace views
        // ********************************** single ******************************************************

        template <typename T>
        class single_view : public view_interface<single_view<T>>
        {
            T value_;

        public:
            explicit single_view(T val)
                : value_(RAH2_STD::move(val))
            {
            }

            T* begin()
            {
                return &value_;
            }
            T const* begin() const
            {
                return &value_;
            }
            T* end()
            {
                return (&value_) + 1;
            }
            T const* end() const
            {
                return (&value_) + 1;
            }
            static size_t size()
            {
                return 1;
            }
            T* data()
            {
                return &value_;
            }
            T const* data() const
            {
                return &value_;
            }
        };
        namespace views
        {
            template <typename V>
            auto single(V&& value)
            {
                return single_view<RAH2_NS::remove_cvref_t<V>>{RAH2_STD::forward<V>(value)};
            }
        } // namespace views
        // ********************************** iota ************************************************

        /// @see RAH2_NS::iota
        template <typename W>
        class iota_iterator
            : public iterator_facade<iota_iterator<W>, default_sentinel_t, W, RAH2_STD::random_access_iterator_tag>
        {
            W val_ = {};

        public:
            iota_iterator() = default;
            explicit iota_iterator(W val)
                : val_(val)
            {
            }

            iota_iterator& operator++()
            {
                ++val_;
                return *this;
            }
            RAH2_POST_INCR(RAH2_STD::random_access_iterator_tag)
            iota_iterator& operator+=(intptr_t value)
            {
                val_ += W(value);
                return *this;
            }
            iota_iterator& operator-=(intptr_t value)
            {
                val_ -= W(value);
                return *this;
            }
            iota_iterator& operator--()
            {
                --val_;
                return *this;
            }
            RAH2_POST_DECR
            auto operator-(iota_iterator const& other) const
            {
                return (val_ - other.val_);
            }
            auto operator*() const
            {
                return val_;
            }
            friend bool operator==(iota_iterator const& it, iota_iterator const& it2)
            {
                return it.val_ == it2.val_;
            }
            friend bool operator==(iota_iterator const&, default_sentinel_t)
            {
                return false;
            }
            friend bool operator==(default_sentinel_t, iota_iterator const&)
            {
                return false;
            }
            friend bool operator<(iota_iterator const& it, iota_iterator const& it2)
            {
                return it.val_ < it2.val_;
            }
        };
        namespace views
        {
            // TODO : Create a iota_view
            template <typename W = size_t>
            constexpr auto iota(W start, W stop)
            {

                return make_subrange(iota_iterator<W>(start), iota_iterator<W>(stop));
            }

            template <typename W = size_t>
            constexpr auto iota(W start = 0)
            {
                return make_subrange(iota_iterator<W>(start), default_sentinel_t{});
            }
        } // namespace views
        // ******************************* istream_view ******************************************************

        template <typename Val, class CharT, class Traits = std::char_traits<CharT>>
        class basic_istream_view : view_interface<basic_istream_view<Val, CharT, Traits>>
        {
            std::istream* stream_ = nullptr;
            Val value_;

        public:
            explicit basic_istream_view(std::istream* stream)
                : stream_(stream)
            {
            }

            std::istream* stream() const
            {
                return stream_;
            }

            struct sentinel
            {
            };

            class iterator
                : public iterator_facade<iterator, sentinel, Val, RAH2_STD::input_iterator_tag>
            {
                basic_istream_view* istream_;

            public:
                explicit iterator(basic_istream_view* istream)
                    : istream_(istream)
                {
                }

                Val operator*() const
                {
                    return istream_->value_;
                }

                iterator const& operator++()
                {
                    *(istream_->stream()) >> istream_->value_;
                    return *this;
                }
                void operator++(int)
                {
                    ++(*this);
                }
                friend bool operator==(iterator const& it, sentinel const&)
                {
                    return !(*it.istream_->stream());
                }
                friend bool operator==(sentinel const&, iterator const& it)
                {
                    return !(*it.istream_->stream());
                }
            };

            auto begin()
            {
                *stream_ >> value_;
                return iterator(this);
            }

            auto end() const
            {
                return sentinel{};
            }
        };

        template <class Val>
        using istream_view = basic_istream_view<Val, char>;

        template <class Val>
        using wistream_view = basic_istream_view<Val, wchar_t>;

        namespace views
        {
            template <class Val, typename S>
            auto istream(S& stream)
            {
                return basic_istream_view<Val, typename S::char_type, typename S::traits_type>(
                    &stream);
            }
        } // namespace views

        // ********************************** repeat ******************************************************

        /// @see RAH2_NS::repeat
        template <typename V>
        class repeat_view : public view_interface<repeat_view<V>>
        {
            V value_;

        public:
            class iterator
                : public iterator_facade<iterator, default_sentinel_t, V, RAH2_STD::random_access_iterator_tag>
            {
                V val_ = V();
                size_t current_ = 0;

            public:
                iterator() = default;
                template <typename U>
                explicit iterator(U val)
                    : val_(RAH2_STD::forward<U>(val))
                {
                }

                iterator& operator++()
                {
                    ++current_;
                    return *this;
                }
                RAH2_POST_INCR(RAH2_STD::random_access_iterator_tag)
                iterator& operator+=(intptr_t value)
                {
                    current_ += value;
                    return *this;
                }
                iterator& operator--()
                {
                    --current_;
                    return *this;
                }
                RAH2_POST_DECR
                iterator& operator-=(intptr_t value)
                {
                    current_ -= value;
                    return *this;
                }
                V operator*() const
                {
                    return val_;
                }
                friend bool operator==(iterator const& it1, iterator const& it2)
                {
                    return it1.current_ == it2.current_;
                }
                friend bool operator==(iterator, default_sentinel_t)
                {
                    return false;
                }
                friend bool operator==(default_sentinel_t, iterator)
                {
                    return false;
                }
                friend bool operator<(iterator const& it1, iterator const& it2)
                {
                    return it1.current_ < it2.current_;
                }
                friend intptr_t operator-(iterator const& it1, iterator const& it2)
                {
                    return it1.current_ - it2.current_;
                }
            };

            explicit repeat_view(V value)
                : value_(RAH2_STD::move(value))
            {
            }

            iterator begin() const
            {
                return iterator{value_};
            }

            default_sentinel_t end() const
            {
                return {};
            }
        };
        namespace views
        {
            template <typename V>
            auto repeat(V&& value)
            {
                return repeat_view<remove_cvref_t<V>>(RAH2_STD::forward<V>(value));
            }
        } // namespace views
        // ********************************* ref_view *********************************************

        template <typename R>
        class ref_view : public view_interface<ref_view<R>>
        {
            R* ref_ = nullptr;
            static constexpr bool is_sized = RAH2_NS::ranges::sized_range<R>;

        public:
            explicit ref_view(R& ref)
                : ref_(&ref)
            {
            }
            auto begin() const
            {
                assert(ref_ != nullptr);
                return RAH2_NS::ranges::begin(*ref_);
            }
            auto end() const
            {
                assert(ref_ != nullptr);
                return RAH2_NS::ranges::end(*ref_);
            }
            bool empty() const
            {
                assert(ref_ != nullptr);
                return RAH2_NS::ranges::empty(*ref_);
            }
            template <bool IsSized = RAH2_NS::ranges::sized_range<R const>, RAH2_STD::enable_if_t<IsSized>* = nullptr>
            auto size() const
            {
                assert(ref_ != nullptr);
                return RAH2_NS::ranges::size(*ref_);
            }
            template <bool IsSized = RAH2_NS::ranges::sized_range<R>, RAH2_STD::enable_if_t<IsSized>* = nullptr>
            auto size()
            {
                assert(ref_ != nullptr);
                return RAH2_NS::ranges::size(*ref_);
            }

            template <typename U = R, RAH2_STD::enable_if_t<RAH2_NS::ranges::contiguous_range<U>>* = nullptr>
            auto data()
            {
                assert(ref_ != nullptr);
                return RAH2_NS::ranges::data(*ref_);
            }
        };
        template <class T>
        constexpr bool enable_borrowed_range<ref_view<T>> = true;

        namespace views
        {
            template <typename R>
            auto ref(R& range)
            {
                static_assert(
                    not RAH2_NS::is_rvalue_reference_v<R>, "range can't be a rvalue reference");
                return ref_view<RAH2_STD::remove_reference_t<R>>(range);
            }

        } // namespace views
        // ********************************* owning_view ******************************************

        template <typename R>
        class owning_view : view_interface<ref_view<R>>
        {
            R range_;

        public:
            explicit owning_view(R r)
                : range_(RAH2_STD::move(r))
            {
            }
            owning_view(owning_view const&) = delete;
            owning_view& operator=(owning_view const&) = delete;
            owning_view(owning_view&&) noexcept = default;
            owning_view& operator=(owning_view&&) noexcept = default;
            ~owning_view() = default;
            R& base()
            {
                return range_;
            }
            R const& base() const
            {
                return range_;
            }
            auto begin()
            {
                return range_.begin();
            }
            auto end()
            {
                return range_.end();
            }
            bool empty() const
            {
                return RAH2_NS::ranges::empty(range_);
            }
            template <bool IsSized = RAH2_NS::ranges::sized_range<R const>, RAH2_STD::enable_if_t<IsSized>* = nullptr>
            auto size() const
            {
                return RAH2_NS::ranges::size(range_);
            }
            template <bool IsSized = RAH2_NS::ranges::sized_range<R>, RAH2_STD::enable_if_t<IsSized>* = nullptr>
            auto size()
            {
                return RAH2_NS::ranges::size(range_);
            }
            template <
                typename C = RAH2_NS::ranges::range_iter_categ_t<R>,
                RAH2_STD::enable_if_t<RAH2_NS::derived_from<C, RAH2_NS::contiguous_iterator_tag>>* = nullptr>
            auto data() const
            {
                return RAH2_NS::ranges::data(range_);
            }
        };

        namespace views
        {
            struct owning_raco : closure_object_facade<owning_raco>
            {
                template <typename R>
                auto operator()(R&& range) const
                {
                    static_assert(
                        not RAH2_NS::is_lvalue_reference_v<R>, "range can't be a lvalue reference");
                    return owning_view<RAH2_STD::remove_reference_t<R>>(RAH2_STD::forward<R>(range));
                }
            };
            constexpr owning_raco owning;
        } // namespace views
        // ********************************** all *************************************************
        namespace views
        {
            struct all_raco : closure_object_facade<all_raco>
            {
                template <typename R, RAH2_STD::enable_if_t<view<RAH2_STD::remove_reference_t<R>>>* = nullptr>
                auto operator()(R&& range) const -> decltype(RAH2_STD::forward<R>(range))
                {
                    return RAH2_STD::forward<R>(range);
                }
                template <
                    typename R,
                    RAH2_STD::enable_if_t<not view<RAH2_STD::remove_reference_t<R>>>* = nullptr,
                    RAH2_STD::enable_if_t<RAH2_NS::is_lvalue_reference_v<R>>* = nullptr>
                auto operator()(R&& range) const
                {
                    return views::ref(RAH2_STD::forward<R>(range));
                }

                template <
                    typename R,
                    RAH2_STD::enable_if_t<not view<RAH2_STD::remove_reference_t<R>>>* = nullptr,
                    RAH2_STD::enable_if_t<not RAH2_NS::is_lvalue_reference_v<R>>* = nullptr>
                auto operator()(R&& range) const
                {
                    return owning_view<RAH2_STD::decay_t<R>>(RAH2_STD::forward<R>(range));
                }

                template <typename V>
                auto operator()(std::initializer_list<V>& range) const
                {
                    return views::ref(range);
                }
            };
            constexpr all_raco all;

            template <typename R, RAH2_STD::enable_if_t<viewable_range<R>>* = nullptr>
            using all_t = decltype(all(RAH2_STD::declval<R>()));
        } // namespace views

        // ***************************************** filter ***************************************

        template <typename R, typename P>
        class filter_view : public view_interface<filter_view<R, P>>
        {
            R base_;
            P pred_;
            using inner_iterator = RAH2_NS::ranges::iterator_t<R>;
            using inner_sentinel = RAH2_NS::ranges::sentinel_t<R>;
            constexpr static bool is_common_range = RAH2_NS::ranges::common_range<R>;
            using base_cat =
                RAH2_NS::ranges::common_iterator_tag<range_iter_categ_t<R>, bidirectional_iterator_tag>;

            // Get a pointer to the pointed value,
            //   OR a pointer to a copy of the pointed value (when not a reference iterator)
            template <class Ref>
            struct get_pointer
            {
                template <typename It>
                static auto get(It&& iter)
                {
                    return RAH2_NS::details::optional<Ref>(*iter);
                }
                using type = RAH2_NS::details::optional<Ref>;
            };

            template <class Ref>
            struct get_pointer<Ref&>
            {
                template <typename It>
                static auto get(It&& iter)
                {
                    return &(*iter);
                }
                using type = Ref*;
            };

        public:
            struct sentinel
            {
                inner_sentinel sent;
            };
            class iterator
                : public iterator_facade<
                      iterator,
                      sentinel,
                      typename RAH2_STD::iterator_traits<inner_iterator>::reference,
                      RAH2_NS::ranges::common_iterator_tag<RAH2_STD::bidirectional_iterator_tag, base_cat>>
            {
                filter_view* view_ = nullptr;
                inner_iterator iter_;

                typename get_pointer<iter_reference_t<inner_iterator>>::type value_pointer_;

                void next_value()
                {
                    while (iter_ != RAH2_NS::ranges::end(view_->base_)
                           && not(view_->pred_)(
                               *(value_pointer_ =
                                     get_pointer<iter_reference_t<inner_iterator>>::get(iter_))))
                    {
                        assert(iter_ != RAH2_NS::ranges::end(view_->base_));
                        ++iter_;
                    }
                }

            public:
                iterator() = default;

                iterator(filter_view* v, inner_iterator iter)
                    : view_(v)
                    , iter_(RAH2_STD::move(iter))
                {
                    next_value();
                }

                iterator& operator++()
                {
                    ++iter_;
                    next_value();
                    return *this;
                }
                RAH2_POST_INCR(base_cat)
                template <typename U = R, RAH2_STD::enable_if_t<bidirectional_range<U>>* = nullptr>
                iterator& operator--()
                {
                    do
                    {
                        --iter_;
                    } while (not(view_->pred_)(*iter_)
                             && iter_ != RAH2_NS::ranges::begin(view_->base_));
                    return *this;
                }
                template <typename U = R, RAH2_STD::enable_if_t<bidirectional_range<U>>* = nullptr>
                RAH2_POST_DECR;
                auto operator*() const -> iter_reference_t<inner_iterator>
                {
                    return *value_pointer_;
                }
                template <typename U = R, RAH2_STD::enable_if_t<equality_comparable<iterator_t<U>>>* = nullptr>
                friend bool operator==(iterator const& it, iterator const& other)
                {
                    return it.iter_ == other.iter_;
                }
                friend bool operator==(iterator const& it, sentinel const& sent)
                {
                    return it.iter_ == sent.sent;
                }
            };

            filter_view(R rng, P pred)
                : base_(RAH2_STD::move(rng))
                , pred_(RAH2_STD::move(pred))
            {
            }

            R base() const
            {
                return base_;
            }

            P const& pred() const
            {
                return pred_;
            }

            iterator begin()
            {
                return iterator(this, RAH2_NS::ranges::begin(base_));
            }

            template <typename U = R, RAH2_STD::enable_if_t<common_range<U>>* = nullptr>
            iterator end()
            {
                return iterator{this, RAH2_NS::ranges::end(base_)};
            }
            template <typename U = R, RAH2_STD::enable_if_t<not common_range<U>>* = nullptr>
            sentinel end()
            {
                return sentinel{RAH2_NS::ranges::end(base_)};
            }
        };
        namespace views
        {
            struct filter_raco
            {
                template <typename R, typename P>
                auto operator()(R&& range, P&& pred) const
                {
                    auto view_ref = all(RAH2_STD::forward<R>(range));
                    return filter_view<decltype(view_ref), RAH2_NS::remove_cvref_t<P>>(
                        RAH2_STD::move(view_ref), RAH2_STD::forward<P>(pred));
                }

                template <typename P>
                auto operator()(P&& pred) const
                {
                    return make_pipeable(
                        [this, pred = RAH2_STD::forward<P>(pred)](auto&& range) {
                            return (*this)(
                                RAH2_STD::forward<decltype(range)>(range), RAH2_STD::move(pred));
                        });
                }
            };
            constexpr filter_raco filter;
        } // namespace views
        // ******************************************* transform ******************************************

        template <typename R, typename F>
        class transform_view : public view_interface<transform_view<R, F>>
        {
            R base_;
            RAH2_NS::details::optional<F> func_;
            constexpr static bool is_common_range = RAH2_NS::ranges::common_range<R>;
            using category =
                cap_iterator_tag<range_iter_categ_t<R>, RAH2_STD::input_iterator_tag, RAH2_STD::random_access_iterator_tag>;
            constexpr static bool is_sized = sized_range<R>;

        public:
            struct sentinel
            {
                sentinel_t<R> sent;
            };

            using reference =
                decltype(RAH2_STD::declval<F>()(RAH2_STD::declval<range_reference_t<R>>()));
            class iterator : public iterator_facade<iterator, sentinel, reference, category>
            {
                iterator_t<R> iter_;
                RAH2_NS::details::optional<F> func_;

            public:
                using difference_type = intptr_t;
                using reference =
                    decltype(RAH2_STD::declval<F>()(RAH2_STD::declval<range_reference_t<R>>()));
                using pointer = typename details::pointer_type<reference>::type;
                using value_type = RAH2_STD::remove_reference_t<reference>;

                iterator() = default;
                iterator(iterator_t<R> iter, F func)
                    : iter_(RAH2_STD::move(iter))
                    , func_(RAH2_STD::move(func))
                {
                }

                iterator& operator++()
                {
                    ++iter_;
                    return *this;
                }
                RAH2_POST_INCR(category)
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
                template <
                    typename C = category,
                    RAH2_STD::enable_if_t<RAH2_NS::derived_from<C, RAH2_STD::bidirectional_iterator_tag>>* = nullptr>
                iterator& operator--()
                {
                    --iter_;
                    return *this;
                }
                template <
                    typename C = category,
                    RAH2_STD::enable_if_t<RAH2_NS::derived_from<C, RAH2_STD::bidirectional_iterator_tag>>* =
                        nullptr>
                RAH2_POST_DECR;
                template <
                    typename C = category,
                    RAH2_STD::enable_if_t<RAH2_NS::derived_from<C, RAH2_STD::random_access_iterator_tag>>* = nullptr>
                auto operator-(iterator const& r) const
                {
                    return iter_ - r.iter_;
                }
                auto operator*() -> decltype((*func_)(*iter_))
                {
                    return (*func_)(*iter_);
                }
                template <
                    typename C = category,
                    RAH2_STD::enable_if_t<RAH2_NS::derived_from<C, RAH2_STD::random_access_iterator_tag>>* = nullptr>
                bool operator<(iterator const it2) const
                {
                    return iter_ < it2.iter_;
                }
                friend bool operator==(iterator const& it, iterator const& it2)
                {
                    return it.iter_ == it2.iter_;
                }
                friend bool operator==(iterator const& it, sentinel const& sent)
                {
                    return it.iter_ == sent.sent;
                }
                friend bool operator==(sentinel const& sent, iterator const& it)
                {
                    return it.iter_ == sent.sent;
                }
            };

            transform_view() = default;
            transform_view(R base, F func)
                : base_(RAH2_STD::move(base))
                , func_(RAH2_STD::move(func))
            {
            }

            template <bool IsSized = RAH2_NS::ranges::sized_range<R const>, RAH2_STD::enable_if_t<IsSized>* = nullptr>
            auto size() const
            {
                return RAH2_NS::ranges::size(base_);
            }
            template <bool IsSized = RAH2_NS::ranges::sized_range<R>, RAH2_STD::enable_if_t<IsSized>* = nullptr>
            auto size()
            {
                return RAH2_NS::ranges::size(base_);
            }

            auto begin()
            {
                return iterator(RAH2_NS::ranges::begin(base_), *func_);
            }

            template <typename U = R, RAH2_STD::enable_if_t<is_common_range, U>* = nullptr>
            auto end()
            {
                return iterator(RAH2_NS::ranges::end(base_), *func_);
            }

            template <typename U = R, RAH2_STD::enable_if_t<not is_common_range, U>* = nullptr>
            auto end()
            {
                return sentinel{RAH2_NS::ranges::end(base_)};
            }
        };
        namespace views
        {
            struct transform_raco
            {
                template <typename R, typename F>
                constexpr auto operator()(R&& range, F&& func) const
                {
                    auto ref = all(RAH2_STD::forward<R>(range));
                    return transform_view<decltype(ref), RAH2_NS::remove_cvref_t<F>>(
                        RAH2_STD::move(ref), RAH2_STD::forward<F>(func));
                }

                template <typename F>
                constexpr auto operator()(F&& func) const
                {
                    return make_pipeable(
                        [this, func = RAH2_STD::forward<F>(func)](auto&& range) {
                            return (*this)(
                                RAH2_STD::forward<decltype(range)>(range), RAH2_STD::move(func));
                        });
                }
            };
            constexpr transform_raco transform;
        } // namespace views

        // ******************************************* take ***********************************************

        template <typename R>
        class take_view : public view_interface<take_view<R>>
        {
            R input_view_;
            using base_diff_type = range_difference_t<R>;
            base_diff_type count_;
            static constexpr bool IsCommon =
                RAH2_NS::ranges::random_access_range<R> && RAH2_NS::ranges::sized_range<R>;
            constexpr static bool is_sized = sized_range<R>;

        public:
            using base_iterator = RAH2_NS::ranges::iterator_t<R>;
            using base_sentinel = RAH2_NS::ranges::sentinel_t<R>;
            using reference = range_reference_t<R>;
            struct sentinel
            {
                base_sentinel end_;

                friend constexpr bool
                operator==(counted_iterator<iterator_t<R>> const& y, sentinel const& x)
                {
                    return y.count() == 0 || y.base() == x.end_;
                }

                friend constexpr bool
                operator==(sentinel const& x, counted_iterator<iterator_t<R>> const& y)
                {
                    return y.count() == 0 || y.base() == x.end_;
                }
            };

            take_view(R input_view, base_diff_type count_)
                : input_view_(RAH2_STD::move(input_view))
                , count_(count_)
            {
            }

            template <bool IsSized = RAH2_NS::ranges::sized_range<R const>, RAH2_STD::enable_if_t<IsSized>* = nullptr>
            auto size() const
            {
                return RAH2_NS::details::min(
                    range_size_t<R>(count_), RAH2_NS::ranges::size(input_view_));
            }
            template <bool IsSized = sized_range<R>, RAH2_STD::enable_if_t<IsSized>* = nullptr>
            auto size()
            {
                return RAH2_NS::details::min(
                    range_size_t<R>(count_), RAH2_NS::ranges::size(input_view_));
            }

            template <typename U = R, RAH2_STD::enable_if_t<RAH2_NS::ranges::contiguous_range<U>>* = nullptr>
            auto data()
            {
                return input_view_.data();
            }

            template <typename U = R, RAH2_STD::enable_if_t<RAH2_NS::ranges::contiguous_range<U>>* = nullptr>
            auto data() const
            {
                return input_view_.data();
            }

            template <bool S = sized_range<R>, RAH2_STD::enable_if_t<!S>* = nullptr>
            auto begin()
            {
                return counted_iterator<iterator_t<R>>(ranges::begin(input_view_), count_);
            }

            template <
                bool S = sized_range<R>,
                bool RA = random_access_range<R>,
                RAH2_STD::enable_if_t<S && RA>* = nullptr>
            auto begin()
            {
                return ranges::begin(input_view_);
            }

            template <
                bool S = sized_range<R>,
                bool RA = random_access_range<R>,
                RAH2_STD::enable_if_t<S && !RA>* = nullptr>
            auto begin()
            {
                return counted_iterator<iterator_t<R>>(
                    ranges::begin(input_view_), ranges::range_difference_t<R>(this->size()));
            }

            template <bool S = sized_range<R>, RAH2_STD::enable_if_t<!S>* = nullptr>
            sentinel end()
            {
                return sentinel{ranges::end(input_view_)};
            }

            template <
                bool S = sized_range<R>,
                bool RA = random_access_range<R>,
                RAH2_STD::enable_if_t<S && RA>* = nullptr>
            auto end()
            {
                return ranges::begin(input_view_) + ranges::range_difference_t<R>(this->size());
            }

            template <
                bool S = sized_range<R>,
                bool RA = random_access_range<R>,
                RAH2_STD::enable_if_t<S && !RA>* = nullptr>
            auto end()
            {
                return RAH2_NS::default_sentinel;
            }
        };
        template <class T>
        constexpr bool enable_borrowed_range<take_view<T>> =
            RAH2_NS::ranges::enable_borrowed_range<T>;

        namespace views
        {
            struct take_raco
            {
                template <typename R>
                auto operator()(R&& range, range_difference_t<R> count) const
                {
                    auto range_view = all(RAH2_STD::forward<R>(range));
                    return take_view<decltype(range_view)>(RAH2_STD::move(range_view), count);
                }

                auto operator()(size_t count) const
                {
                    return make_pipeable(
                        [this, count](auto&& range)
                        {
                            return (*this)(
                                RAH2_STD::forward<decltype(range)>(range),
                                static_cast<range_difference_t<decltype(range)>>(count));
                        });
                }
            };
            constexpr take_raco take;
        } // namespace views

        // ******************************************* drop ***********************************************

        template <typename R>
        class drop_view : view_interface<drop_view<R>>
        {
            R base_;
            using base_diff_t = range_difference_t<R>;
            base_diff_t drop_count_ = 0;
            using category = range_iter_categ_t<R>;
            constexpr static bool is_sized = sized_range<R>;

        public:
            using iterator = RAH2_NS::ranges::iterator_t<R>;

            drop_view() = default;
            drop_view(R v, base_diff_t const drop_count)
                : base_(RAH2_STD::move(v))
                , drop_count_(drop_count)
            {
            }

            template <bool IsSized = RAH2_NS::ranges::sized_range<R const>, RAH2_STD::enable_if_t<IsSized>* = nullptr>
            auto size() const
            {
                using base_size_t = range_size_t<R>;
                auto const subsize = RAH2_NS::ranges::size(base_);
                return RAH2_NS::details::max(base_size_t(0), base_size_t(subsize - drop_count_));
            }
            template <bool IsSized = RAH2_NS::ranges::sized_range<R>, RAH2_STD::enable_if_t<IsSized>* = nullptr>
            auto size()
            {
                using base_size_t = range_size_t<R>;
                auto const subsize = RAH2_NS::ranges::size(base_);
                return RAH2_NS::details::max(base_size_t(0), base_size_t(subsize - drop_count_));
            }

            auto begin()
            {
                auto iter = RAH2_NS::ranges::begin(base_);
                RAH2_NS::ranges::advance(iter, drop_count_, RAH2_NS::ranges::end(base_));
                return iter;
            }

            auto end()
            {
                return RAH2_NS::ranges::end(base_);
            }

            template <
                typename C = category,
                RAH2_STD::enable_if_t<RAH2_NS::derived_from<C, RAH2_NS::contiguous_iterator_tag>>* = nullptr>
            auto data()
            {
                return &(*begin());
            }
        };
        template <class T>
        constexpr bool enable_borrowed_range<drop_view<T>> =
            RAH2_NS::ranges::enable_borrowed_range<T>;

        namespace views
        {
            struct drop_raco
            {
                template <typename R>
                auto operator()(R&& range, range_difference_t<R> count) const
                {
                    auto ref = all(RAH2_STD::forward<R>(range));
                    return drop_view<decltype(ref)>(RAH2_STD::move(ref), count);
                }

                inline auto operator()(size_t count) const
                {
                    return make_pipeable(
                        [this, count](auto&& range)
                        {
                            return (*this)(
                                RAH2_STD::forward<decltype(range)>(range),
                                static_cast<range_difference_t<decltype(range)>>(count));
                        });
                }
            };
            constexpr drop_raco drop;
        } // namespace views

        // ******************************************* drop_while *********************************

        template <typename R, typename F>
        class drop_while_view : view_interface<drop_while_view<R, F>>
        {
            R base_;
            F pred_;
            using category = range_iter_categ_t<R>;

        public:
            using iterator = RAH2_NS::ranges::iterator_t<R>;
            using sentinel = RAH2_NS::ranges::sentinel_t<R>;

            drop_while_view() = default;
            drop_while_view(R v, F func)
                : base_(RAH2_STD::move(v))
                , pred_(RAH2_STD::move(func))
            {
            }

            iterator begin()
            {
                auto iter = RAH2_NS::ranges::begin(base_);
                auto end = RAH2_NS::ranges::end(base_);
                while (pred_(*iter) and iter != end)
                {
                    ++iter;
                }
                return iter;
            }

            sentinel end()
            {
                return RAH2_NS::ranges::end(base_);
            }

            template <
                typename C = category,
                RAH2_STD::enable_if_t<RAH2_NS::derived_from<C, RAH2_NS::contiguous_iterator_tag>>* = nullptr>
            auto data()
            {
                return &(*begin());
            }
        };
        template <class T, class Pred>
        constexpr bool enable_borrowed_range<drop_while_view<T, Pred>> =
            RAH2_NS::ranges::enable_borrowed_range<T>;

        namespace views
        {
            struct drop_while_raco
            {
                /// @brief A view of elements from an underlying sequence, beginning at the first element
                /// for which the predicate returns false.
                ///
                /// @snippet test.cpp RAH2_NS::views::drop_while
                /// @snippet test.cpp RAH2_NS::views::drop_while_pipeable
                template <typename R, typename P>
                auto operator()(R&& range, P&& predicate) const
                {
                    auto ref = all(RAH2_STD::forward<R>(range));
                    return drop_while_view<decltype(ref), RAH2_NS::remove_cvref_t<P>>(
                        RAH2_STD::move(ref), RAH2_STD::forward<P>(predicate));
                }

                template <typename P>
                auto operator()(P&& predicate) const
                {
                    return make_pipeable(
                        [this, pred = RAH2_STD::forward<P>(predicate)](auto&& range) {
                            return (*this)(
                                RAH2_STD::forward<decltype(range)>(range), RAH2_STD::move(pred));
                        });
                }
            };
            constexpr drop_while_raco drop_while;
        } // namespace views

        // ********************************** join ********************************************************

        template <typename R>
        class join_view : public view_interface<join_view<R>>
        {
            R base_;
            using iterator1 = iterator_t<R>;
            using iterator2 = sentinel_t<R>;
            using sub_range_type = RAH2_NS::ranges::range_reference_t<R>;
            using sub_range_ref_traits = RAH2_NS::ranges::details::pointer_type<sub_range_type>;
            using sub_range_type_ptr = typename sub_range_ref_traits::type;
            using sub_iter_begin = iterator_t<sub_range_type>;
            using sub_iter_end = sentinel_t<sub_range_type>;
            iterator1 range_iter_;
            iterator2 range_end_;
            sub_range_type_ptr subrange_;
            sub_iter_begin sub_range_iter_;
            sub_iter_end sub_range_end_;
            bool init_ = false;
            using base_cat = RAH2_STD::input_iterator_tag;

            void next_valid()
            {
                while (sub_range_iter_ == sub_range_end_)
                {
                    ++range_iter_;
                    if (range_iter_ == range_end_)
                        return;
                    else
                    {
                        subrange_ = sub_range_ref_traits::to_pointer(*range_iter_);
                        sub_range_iter_ = RAH2_NS::ranges::begin(*subrange_);
                        sub_range_end_ = RAH2_NS::ranges::end(*subrange_);
                    }
                }
            }

        public:
            class iterator
                : public iterator_facade<iterator, default_sentinel_t, range_reference_t<range_reference_t<R>>, base_cat>
            {
                join_view* view_ = nullptr;

            public:
                iterator() = default;

                explicit iterator(join_view* base)
                    : view_(base)
                {
                    if (view_->range_iter_ == view_->range_end_)
                        return;
                    view_->next_valid();
                }

                iterator& operator++()
                {
                    ++view_->sub_range_iter_;
                    view_->next_valid();
                    return *this;
                }
                RAH2_POST_INCR(base_cat)
                auto operator*() const -> decltype(*view_->sub_range_iter_)
                {
                    return *view_->sub_range_iter_;
                }
                bool operator==(default_sentinel_t) const
                {
                    return view_->range_iter_ == view_->range_end_;
                }
                friend bool operator==(default_sentinel_t sent, iterator const& it)
                {
                    return it == sent;
                }
            };

            join_view() = default;

            explicit join_view(R base)
                : base_(RAH2_STD::move(base))
            {
            }

            bool empty() const
            {
                return RAH2_NS::ranges::empty(base_);
            }

            auto base() const
            {
                return base_;
            }

            auto begin()
            {
                init_ = true;
                range_iter_ = RAH2_NS::ranges::begin(base_);
                range_end_ = RAH2_NS::ranges::end(base_);
                subrange_ = sub_range_ref_traits::to_pointer(*range_iter_);
                sub_range_iter_ = RAH2_NS::ranges::begin(*subrange_);
                sub_range_end_ = RAH2_NS::ranges::end(*subrange_);

                return iterator(this);
            }

            auto end()
            {
                return default_sentinel_t();
            }
        };
        namespace views
        {
            struct join_raco : closure_object_facade<join_raco>
            {
                template <typename R>
                auto operator()(R&& range_of_ranges) const
                {
                    auto rangeRef = all(RAH2_STD::forward<R>(range_of_ranges));
                    return join_view<decltype(rangeRef)>(RAH2_STD::move(rangeRef));
                }
            };
            constexpr join_raco join;

        } // namespace views

        // ************************************ split_view ****************************************

        template <typename R, typename P, RAH2_STD::enable_if_t<forward_range<R>>* = nullptr>
        class split_view : public view_interface<split_view<R, P>>
        {
            R base_;
            P pattern_;
            using inner_iterator = RAH2_NS::ranges::iterator_t<R>;
            using inner_sentinel = RAH2_NS::ranges::sentinel_t<R>;
            RAH2_NS::details::optional<subrange<inner_iterator, inner_iterator>> cached_begin_;
            using base_cat = RAH2_STD::input_iterator_tag;

            template <typename I1, typename S1, typename I2, typename S2>
            static constexpr subrange<I1> search(I1 first1, S1 last1, I2 first2, S2 last2)
            {
                for (;; ++first1)
                {
                    I1 it1 = first1;
                    for (I2 it2 = first2;; ++it1, ++it2)
                    {
                        if (it2 == last2)
                            return {first1, it1};
                        if (it1 == last1)
                            return {it1, it1};
                        if (!(*it1 == *it2))
                            break;
                    }
                }
            }

            auto find_next(inner_iterator const& it)
            {
                auto sub = split_view::search(
                    it,
                    RAH2_NS::ranges::end(base_),
                    RAH2_NS::ranges::begin(pattern_),
                    RAH2_NS::ranges::end(pattern_));
                auto b = sub.begin();
                auto e = sub.end();

                if (b != RAH2_NS::ranges::end(base_) and RAH2_NS::ranges::empty(pattern_))
                {
                    ++b;
                    ++e;
                }

                return RAH2_NS::ranges::make_subrange(b, e);
            }

        public:
            struct sentinel
            {
                inner_sentinel inner;
            };
            class iterator
                : public iterator_facade<iterator, sentinel, subrange<inner_iterator, inner_iterator>, base_cat>
            {
                split_view* parent_;
                inner_iterator cur_;
                subrange<inner_iterator, inner_iterator> next_;
                bool trailing_empty_ = false;

            public:
                iterator(
                    split_view* parent,
                    inner_iterator cur,
                    subrange<inner_iterator, inner_iterator> next)
                    : parent_(parent)
                    , cur_(RAH2_STD::move(cur))
                    , next_(RAH2_STD::move(next))
                {
                }

                iterator& operator++()
                {
                    cur_ = next_.begin();

                    if (cur_ != RAH2_NS::ranges::end(parent_->base_))
                    {
                        cur_ = next_.end();
                        if (cur_ == RAH2_NS::ranges::end(parent_->base_))
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
                RAH2_POST_INCR(base_cat)
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
                    return x.cur_ == y.inner;
                }
            };

            split_view(R base, P pattern)
                : base_(RAH2_STD::move(base))
                , pattern_(RAH2_STD::move(pattern))
            {
            }

            iterator begin()
            {
                if (!cached_begin_.has_value())
                    cached_begin_ = find_next(RAH2_NS::ranges::begin(base_));
                return iterator(this, RAH2_NS::ranges::begin(base_), *cached_begin_);
            }

            sentinel end()
            {
                return sentinel{RAH2_NS::ranges::end(base_)};
            }
        };
        namespace views
        {
            struct split_raco
            {
                template <typename R, typename P, RAH2_STD::enable_if_t<forward_range<R>>* = nullptr>
                auto operator()(R&& range, P&& pattern) const
                {
                    auto ref = all(RAH2_STD::forward<R>(range));
                    return split_view<decltype(ref), RAH2_NS::remove_cvref_t<P>>(
                        RAH2_STD::move(ref), RAH2_STD::forward<P>(pattern));
                }

                template <typename P>
                auto operator()(P&& pattern) const
                {
                    return make_pipeable(
                        [p = RAH2_STD::forward<P>(pattern)](auto&& range) {
                            return split(
                                RAH2_STD::forward<decltype(range)>(range), RAH2_STD::move(p));
                        });
                }
            };
            constexpr split_raco split;
        } // namespace views
        // ******************************************* counted ************************************
        namespace views
        {

            template <typename I, RAH2_STD::enable_if_t<RAH2_NS::random_access_iterator<I>>* = nullptr>
            auto counted(I&& it, iter_difference_t<I> n)
            {
                using iterator = RAH2_STD::remove_reference_t<I>;
                return make_subrange(iterator(it), iterator(it + n));
            }

            template <typename I, RAH2_STD::enable_if_t<!RAH2_NS::random_access_iterator<I>>* = nullptr>
            auto counted(I&& it, iter_difference_t<I> n)
            {
                using iterator = counted_iterator<RAH2_STD::remove_reference_t<I>>;
                return make_subrange(iterator(it, n), default_sentinel_t{});
            }
        } // namespace views

        // ******************************************* common_view ********************************

        template <typename R>
        class common_view : public view_interface<common_view<R>>
        {
            R base_;
            using base_iterator = iterator_t<R>;
            using base_sentinel = sentinel_t<R>;
            using input_iter_cat = range_iter_categ_t<R>;
            using Cat = RAH2_STD::conditional_t<
                RAH2_NS::ranges::common_range<R>, // If common
                input_iter_cat, // do not use common_iterator
                std::conditional_t< // If sized random_access
                    RAH2_NS::ranges::sized_range<R> && RAH2_NS::ranges::random_access_range<R>,
                    input_iter_cat, // do not use common_iterator (move begin to end)
                    RAH2_NS::ranges::cap_iterator_tag<input_iter_cat, RAH2_NS::input_iterator_tag, RAH2_NS::bidirectional_iterator_tag>>>;
            static constexpr bool is_sized = RAH2_NS::ranges::sized_range<R>;

            static_assert(not common_range<R>, "expect not common_range<R>");
            static_assert(RAH2_NS::copyable<iterator_t<R>>, "expect copyable<iterator_t<R>>");

        public:
            explicit common_view(R r)
                : base_(RAH2_STD::move(r))
            {
            }

            // TODO : Move in RAH_NS for std::common_iterator
            class common_iterator
                : public iterator_facade<common_iterator, void, iter_reference_t<base_iterator>, Cat>
            {
                static_assert(
                    RAH2_NS::input_or_output_iterator<base_iterator>,
                    "RAH2_NS::input_or_output_iterator<I>");
                static_assert(!RAH2_NS::same_as<base_iterator, base_sentinel>, "!RAH2_NS::same_as<I, S>");
                static_assert(RAH2_NS::copyable<base_iterator>, "RAH2_NS::copyable<I>");
                mpark::variant<base_iterator, base_sentinel> var_;

                struct lesser
                {
                    auto operator()(base_iterator const& it1, base_iterator const& it2)
                    {
                        return it1 < it2;
                    }
                    auto operator()(base_iterator const&, base_sentinel const&)
                    {
                        return true;
                    }
                    auto operator()(base_sentinel const&, base_iterator const&)
                    {
                        return false;
                    }
                    auto operator()(base_sentinel const&, base_sentinel const&)
                    {
                        return false;
                    }
                };

                struct sub
                {
                    auto operator()(base_iterator const& it1, base_iterator const& it2)
                    {
                        return it1 - it2;
                    }
                    auto operator()(base_iterator const&, base_sentinel const&)
                    {
                        RAH2_ASSERT(false && "Can't substract sentinel to iterator");
                        return 0;
                    }
                    auto operator()(base_sentinel const&, base_iterator const&)
                    {
                        RAH2_ASSERT(false && "Can't substract iterator to sentinel");
                        return 0;
                    }
                    auto operator()(base_sentinel const&, base_sentinel const&)
                    {
                        return 0;
                    }
                };

                struct equal
                {
                    template <typename I = base_iterator, RAH2_STD::enable_if_t<equality_comparable<I>>* = nullptr>
                    auto operator()(base_iterator const& it1, base_iterator const& it2)
                    {
                        return it1 == it2;
                    }
                    template <typename I = base_iterator, RAH2_STD::enable_if_t<!equality_comparable<I>>* = nullptr>
                    auto operator()(base_iterator const&, base_iterator const&)
                    {
                        RAH2_ASSERT(false && "This iterator is not equality comparable");
                        return false;
                    }
                    auto operator()(base_iterator const& it, base_sentinel const& sent)
                    {
                        return it == sent;
                    }
                    auto operator()(base_sentinel const& sent, base_iterator const& it)
                    {
                        return it == sent;
                    }
                    auto operator()(base_sentinel const&, base_sentinel const&)
                    {
                        return true;
                    }
                };

                template <typename Apply>
                static auto
                dispatch(Apply&& apply, common_iterator const& it1, common_iterator const& it2)
                {
                    if (mpark::holds_alternative<base_sentinel>(it2.var_))
                    {
                        if (mpark::holds_alternative<base_sentinel>(it1.var_))
                            return apply(mpark::get<1>(it1.var_), mpark::get<1>(it2.var_));
                        else
                            return apply(mpark::get<0>(it1.var_), mpark::get<1>(it2.var_));
                    }
                    else if (mpark::holds_alternative<base_sentinel>(it1.var_))
                        return apply(mpark::get<1>(it1.var_), mpark::get<0>(it2.var_));
                    else
                        return apply(mpark::get<0>(it1.var_), mpark::get<0>(it2.var_));
                }

            public:
                common_iterator() = default;

                explicit common_iterator(base_iterator it)
                    : var_(RAH2_STD::move(it))
                {
                }
                explicit common_iterator(base_sentinel sent)
                    : var_(RAH2_STD::move(sent))
                {
                }

                iter_reference_t<base_iterator> operator*()
                {
                    return *mpark::get<base_iterator>(var_);
                }

                common_iterator& operator++()
                {
                    ++mpark::get<base_iterator>(var_);
                    return *this;
                }
                RAH2_POST_INCR(Cat)
                template <
                    typename C = Cat,
                    RAH2_STD::enable_if_t<RAH2_NS::derived_from<C, RAH2_STD::bidirectional_iterator_tag>>* = nullptr>
                common_iterator& operator--()
                {
                    --mpark::get<base_iterator>(var_);
                    return *this;
                }
                template <
                    typename C = Cat,
                    RAH2_STD::enable_if_t<RAH2_NS::derived_from<C, RAH2_STD::bidirectional_iterator_tag>>* =
                        nullptr>
                RAH2_POST_DECR;
                template <
                    typename C = Cat,
                    RAH2_STD::enable_if_t<RAH2_NS::derived_from<C, RAH2_STD::random_access_iterator_tag>>* = nullptr>
                bool operator<(common_iterator const& it2) const
                {
                    return mpark::get<base_iterator>(var_) < mpark::get<base_iterator>(it2.var_);
                }
                template <
                    typename C = Cat,
                    RAH2_STD::enable_if_t<RAH2_NS::derived_from<C, RAH2_STD::random_access_iterator_tag>>* = nullptr>
                common_iterator& operator+=(RAH2_NS::ranges::range_difference_t<R> value)
                {
                    mpark::get<base_iterator>(var_) += value;
                    return *this;
                }
                bool operator==(common_iterator const& it) const
                {
                    return dispatch(equal(), *this, it);
                }

                template <
                    typename C = Cat,
                    RAH2_STD::enable_if_t<RAH2_NS::is_same_v<C, RAH2_STD::input_iterator_tag>>* = nullptr>
                bool operator!=(common_iterator const& it) const
                {
                    return !dispatch(equal(), *this, it);
                }

                template <
                    typename I = base_iterator,
                    typename S = base_sentinel,
                    RAH2_STD::enable_if_t<sized_sentinel_for<S, I> && sized_sentinel_for<I, I>>* = nullptr>
                common_iterator operator-(common_iterator const& it)
                {
                    return dispatch(sub(), *this, it);
                }
            };

            template <typename R2 = R, RAH2_STD::enable_if_t<RAH2_NS::ranges::common_range<R2>>* = nullptr>
            auto begin()
            {
                return RAH2_NS::ranges::begin(base_);
            }

            template <
                typename R2 = R,
                RAH2_STD::enable_if_t<
                    !RAH2_NS::ranges::common_range<R2> && RAH2_NS::ranges::random_access_range<R2>
                    && RAH2_NS::ranges::sized_range<R2>>* = nullptr>
            auto begin()
            {
                return RAH2_NS::ranges::begin(base_);
            }

            template <
                typename R2 = R,
                RAH2_STD::enable_if_t<
                    !(!RAH2_NS::ranges::common_range<R2> && RAH2_NS::ranges::random_access_range<R2>
                      && RAH2_NS::ranges::sized_range<R2>)>* = nullptr>
            auto begin()
            {
                return common_iterator(RAH2_NS::ranges::begin(base_));
            }

            template <typename R2 = R, RAH2_STD::enable_if_t<RAH2_NS::ranges::common_range<R2>>* = nullptr>
            auto end()
            {
                return RAH2_NS::ranges::end(base_);
            }

            template <
                typename R2 = R,
                RAH2_STD::enable_if_t<
                    !RAH2_NS::ranges::common_range<R2> && RAH2_NS::ranges::random_access_range<R2>
                    && RAH2_NS::ranges::sized_range<R2>>* = nullptr>
            auto end()
            {
                return RAH2_NS::ranges::begin(base_) + RAH2_NS::ranges::size(base_);
            }

            template <
                typename R2 = R,
                RAH2_STD::enable_if_t<
                    !(!RAH2_NS::ranges::common_range<R2> && RAH2_NS::ranges::random_access_range<R2>
                      && RAH2_NS::ranges::sized_range<R2>)>* = nullptr>

            auto end()
            {
                return common_iterator(RAH2_NS::ranges::end(base_));
            }

            template <
                typename C = Cat,
                RAH2_STD::enable_if_t<derived_from<C, RAH2_NS::contiguous_iterator_tag>>* = nullptr>
            auto data()
            {
                return &(*begin());
            }

            template <bool IsSized = RAH2_NS::ranges::sized_range<R const>, RAH2_STD::enable_if_t<IsSized>* = nullptr>
            auto size() const
            {
                return RAH2_NS::ranges::size(base_);
            }
            template <bool IsSized = RAH2_NS::ranges::sized_range<R>, RAH2_STD::enable_if_t<IsSized>* = nullptr>
            auto size()
            {
                return RAH2_NS::ranges::size(base_);
            }
        };
        template <class T>
        constexpr bool enable_borrowed_range<common_view<T>> =
            RAH2_NS::ranges::enable_borrowed_range<T>;

        namespace views
        {
            struct common_raco : closure_object_facade<common_raco>
            {
                /// @brief Adapts a given view with different types for iterator/sentinel pair into a view
                /// that is also a common_range. A common_view always has the same iterator/sentinel type.
                ///
                /// @snippet test.cpp RAH2_NS::views::common
                template <typename R, RAH2_STD::enable_if_t<not common_range<R>>* = nullptr>
                auto operator()(R&& range) const
                {
                    auto ref = all(RAH2_STD::forward<R>(range));
                    return common_view<decltype(ref)>(RAH2_STD::move(ref));
                }

                template <typename R, RAH2_STD::enable_if_t<common_range<R>>* = nullptr>
                auto operator()(R&& range) const
                {
                    return all(RAH2_STD::forward<R>(range));
                }
            };
            constexpr common_raco common;
        } // namespace views

        // ***************************************** reverse **********************************************

        template <typename R>
        class reverse_view : public view_interface<reverse_view<R>>
        {
            R base_;
            static constexpr bool is_sized = RAH2_NS::ranges::sized_range<R>;
            static constexpr bool is_const_sized = RAH2_NS::ranges::sized_range<R const>;

        public:
            reverse_view() = default;

            explicit reverse_view(R base)
                : base_(RAH2_STD::move(base))
            {
            }

            template <typename X = R, RAH2_STD::enable_if_t<common_range<X>>* = nullptr>
            auto begin()
            {
                return RAH2_STD::make_reverse_iterator(RAH2_NS::ranges::end(base_));
            }

            template <typename X = R, RAH2_STD::enable_if_t<!common_range<X>>* = nullptr>
            auto begin()
            {
                return RAH2_STD::make_reverse_iterator(RAH2_NS::ranges::next(
                    RAH2_NS::ranges::begin(base_), RAH2_NS::ranges::end(base_)));
            }

            auto end()
            {
                return RAH2_STD::make_reverse_iterator(RAH2_NS::ranges::begin(base_));
            }

            template <bool IsSized = RAH2_NS::ranges::sized_range<R const>, RAH2_STD::enable_if_t<IsSized>* = nullptr>
            auto size() const
            {
                return RAH2_NS::ranges::size(base_);
            }
            template <bool IsSized = RAH2_NS::ranges::sized_range<R>, RAH2_STD::enable_if_t<IsSized>* = nullptr>
            auto size()
            {
                return RAH2_NS::ranges::size(base_);
            }
        };
        template <class T>
        constexpr bool enable_borrowed_range<reverse_view<T>> =
            RAH2_NS::ranges::enable_borrowed_range<T>;

        template <class T>
        constexpr bool disable_sized_range<reverse_view<T>> =
            not(RAH2_NS::ranges::sized_range<T> || RAH2_NS::random_access_iterator<iterator_t<T>>);

        namespace views
        {
            struct reverse_raco : closure_object_facade<reverse_raco>
            {
                template <typename R>
                auto operator()(R&& range) const
                {
                    static_assert(
                        RAH2_NS::ranges::bidirectional_range<R>,
                        "reverse expect a bidirectional_range");
                    auto ref = all(RAH2_STD::forward<R>(range));
                    return reverse_view<decltype(ref)>(RAH2_STD::move(ref));
                }
            };

            constexpr reverse_raco reverse;
        } // namespace views

        // **************************** element_view **********************************************

        template <typename R, size_t N>
        class elements_view : public view_interface<elements_view<R, N>>
        {
            R base_;

            using inner_iterator = iterator_t<R>;
            using inner_sentinel = sentinel_t<R>;
            using base_cat =
                cap_iterator_tag<range_iter_categ_t<R>, input_iterator_tag, random_access_iterator_tag>;
            using reference =
                typename RAH2_STD::tuple_element<N, RAH2_STD::remove_reference_t<range_reference_t<R>>>::type;

        public:
            using difference_type = range_difference_t<R>;

            elements_view() = default;

            explicit elements_view(R base)
                : base_(RAH2_STD::move(base))
            {
            }

            struct sentinel
            {
                inner_sentinel sent;
            };

            class iterator : public iterator_facade<iterator, sentinel, reference, base_cat>
            {
                inner_iterator iter_;

            public:
                iterator() = default;
                explicit iterator(inner_iterator it)
                    : iter_(RAH2_STD::move(it))
                {
                }

                reference operator*()
                {
                    return RAH2_STD::get<N>(*iter_);
                }

                iterator& operator++()
                {
                    ++iter_;
                    return *this;
                }
                RAH2_POST_INCR(base_cat)
                template <
                    typename C = base_cat,
                    RAH2_STD::enable_if_t<RAH2_NS::derived_from<C, RAH2_STD::bidirectional_iterator_tag>>* = nullptr>
                iterator& operator--()
                {
                    --iter_;
                    return *this;
                }
                template <
                    typename C = base_cat,
                    RAH2_STD::enable_if_t<RAH2_NS::derived_from<C, RAH2_STD::bidirectional_iterator_tag>>* =
                        nullptr>
                RAH2_POST_DECR;
                template <
                    typename C = base_cat,
                    RAH2_STD::enable_if_t<RAH2_NS::derived_from<C, RAH2_STD::forward_iterator_tag>>* = nullptr>
                bool operator==(iterator const& it) const
                {
                    return iter_ == it.iter_;
                }
                friend bool operator==(iterator const& it, sentinel const& sent)
                {
                    return it.iter_ == sent.sent;
                }
                template <
                    typename C = base_cat,
                    RAH2_STD::enable_if_t<RAH2_NS::derived_from<C, RAH2_STD::random_access_iterator_tag>>* = nullptr>
                bool operator<(iterator const& it2) const
                {
                    return iter_ < it2.iter_;
                }
                template <
                    typename C = base_cat,
                    RAH2_STD::enable_if_t<RAH2_NS::derived_from<C, RAH2_STD::random_access_iterator_tag>>* = nullptr>
                auto operator-(iterator const& it2) const
                {
                    return iter_ - it2.iter_;
                }
                template <
                    typename C = base_cat,
                    RAH2_STD::enable_if_t<RAH2_NS::derived_from<C, RAH2_STD::random_access_iterator_tag>>* = nullptr>
                iterator& operator+=(difference_type value)
                {
                    iter_ += value;
                    return *this;
                }
            };

            auto begin()
            {
                return iterator{RAH2_NS::ranges::begin(base_)};
            }

            template <typename U = R, RAH2_STD::enable_if_t<RAH2_NS::ranges::common_range<U>>* = nullptr>
            auto end()
            {
                return iterator(RAH2_NS::ranges::end(base_));
            }

            template <typename U = R, RAH2_STD::enable_if_t<!RAH2_NS::ranges::common_range<U>>* = nullptr>
            auto end()
            {
                return sentinel{RAH2_NS::ranges::end(base_)};
            }
        };
        namespace views
        {
            template <size_t N>
            struct elements_raco : closure_object_facade<elements_raco<N>>
            {
                template <typename T>
                auto operator()(T&& range) const
                {
                    auto ref = all(RAH2_STD::forward<T>(range));
                    return elements_view<decltype(ref), N>(RAH2_STD::move(ref));
                }
            };
            template <size_t N>
            constexpr elements_raco<N> elements;

            template <typename T>
            using keys_view = elements_view<T, 0>;

            struct keys_raco : closure_object_facade<keys_raco>
            {
                template <typename T>
                auto operator()(T&& range) const
                {
                    auto ref = all(RAH2_STD::forward<T>(range));
                    return keys_view<decltype(ref)>(RAH2_STD::move(ref));
                }
            };
            constexpr keys_raco keys;

            template <typename T>
            using values_view = elements_view<T, 1>;

            struct values_raco : closure_object_facade<values_raco>
            {
                template <typename T>
                auto operator()(T&& range) const
                {
                    auto ref = all(RAH2_STD::forward<T>(range));
                    return values_view<decltype(ref)>(RAH2_STD::move(ref));
                }
            };
            constexpr values_raco values;
        } // namespace views
        // *************************** enumerate **********************************************************

        template <typename R>
        class enumerate_view : public view_interface<enumerate_view<R>>
        {
            R base_;
            using inner_iterator = iterator_t<R>;
            using inner_sentinel = sentinel_t<R>;
            using base_cat = common_iterator_tag<random_access_iterator_tag, range_iter_categ_t<R>>;
            static constexpr bool is_sized = RAH2_NS::ranges::sized_range<R>;
            using base_diff_type = range_difference_t<R>;

        public:
            explicit enumerate_view(R base)
                : base_(RAH2_STD::move(base))
            {
            }

            using value_type = RAH2_STD::pair<intptr_t, range_reference_t<R>>;

            struct sentinel
            {
                inner_sentinel sent;
            };

            class iterator : public iterator_facade<iterator, sentinel, value_type, base_cat>
            {
                inner_iterator current_;
                base_diff_type pos_{};

            public:
                iterator() = default;
                iterator(inner_iterator current, base_diff_type pos)
                    : current_(RAH2_STD::move(current))
                    , pos_(pos)
                {
                }

                iterator& operator++()
                {
                    ++current_;
                    ++pos_;
                    return *this;
                }
                RAH2_POST_INCR(base_cat)
                template <
                    typename C = base_cat,
                    RAH2_STD::enable_if_t<derived_from<C, RAH2_STD::bidirectional_iterator_tag>>* = nullptr>
                iterator& operator--()
                {
                    --current_;
                    --pos_;
                    return *this;
                }
                template <
                    typename C = base_cat,
                    RAH2_STD::enable_if_t<derived_from<C, RAH2_STD::bidirectional_iterator_tag>>* = nullptr>
                RAH2_POST_DECR value_type operator*()
                {
                    return {pos_, *current_};
                }
                template <
                    typename C = base_cat,
                    RAH2_STD::enable_if_t<RAH2_NS::derived_from<C, RAH2_STD::forward_iterator_tag>>* = nullptr>
                bool operator==(iterator const& iter) const
                {
                    return iter.pos_ == pos_;
                }
                template <typename I = iterator, RAH2_STD::enable_if_t<!RAH2_NS::is_same_v<I, sentinel>>* = nullptr>
                friend bool operator==(iterator const& iter, sentinel const& sent)
                {
                    return iter.current_ == sent.sent;
                }
                template <
                    typename C = base_cat,
                    RAH2_STD::enable_if_t<RAH2_NS::derived_from<C, RAH2_STD::random_access_iterator_tag>>* = nullptr>
                bool operator<(iterator const& iter) const
                {
                    return pos_ < iter.pos_;
                }
                template <
                    typename C = base_cat,
                    RAH2_STD::enable_if_t<RAH2_NS::derived_from<C, RAH2_STD::random_access_iterator_tag>>* = nullptr>
                auto operator-(iterator const& it2) const
                {
                    return pos_ - it2.pos_;
                }
                template <
                    typename C = base_cat,
                    RAH2_STD::enable_if_t<RAH2_NS::derived_from<C, RAH2_STD::random_access_iterator_tag>>* = nullptr>
                iterator& operator+=(intptr_t value)
                {
                    current_ += value;
                    pos_ += value;
                    return *this;
                }
            };

            auto begin()
            {
                return iterator(RAH2_NS::ranges::begin(base_), 0);
            }

            template <
                typename U = R,
                RAH2_STD::enable_if_t<
                    RAH2_NS::ranges::sized_range<U> and RAH2_NS::ranges::common_range<U>>* = nullptr>
            auto end()
            {
                return iterator(
                    RAH2_NS::ranges::end(base_),
                    static_cast<range_difference_t<U>>(RAH2_NS::ranges::ssize(base_)));
            }

            template <
                typename U = R,
                RAH2_STD::enable_if_t<not(
                    RAH2_NS::ranges::sized_range<U> and RAH2_NS::ranges::common_range<U>)>* = nullptr>
            auto end()
            {
                return sentinel{RAH2_NS::ranges::end(base_)};
            }

            template <bool IsSized = RAH2_NS::ranges::sized_range<R const>, RAH2_STD::enable_if_t<IsSized>* = nullptr>
            auto size() const
            {
                return RAH2_NS::ranges::size(base_);
            }
            template <bool IsSized = RAH2_NS::ranges::sized_range<R>, RAH2_STD::enable_if_t<IsSized>* = nullptr>
            auto size()
            {
                return RAH2_NS::ranges::size(base_);
            }
        };
        template <class T>
        constexpr bool enable_borrowed_range<enumerate_view<T>> =
            RAH2_NS::ranges::enable_borrowed_range<T>;
        namespace views
        {
            struct enumerate_raco : closure_object_facade<enumerate_raco>
            {
                template <typename R>
                auto operator()(R&& range) const
                {
                    auto ref = all(RAH2_STD::forward<R>(range));
                    return enumerate_view<decltype(ref)>(RAH2_STD::move(ref));
                }
            };
            constexpr enumerate_raco enumerate;
        } // namespace views

        // *************************** zip ****************************************************************
        /// \cond PRIVATE
        namespace details
        {
            template <typename Tuple, typename F, size_t... Indices>
            void for_each_impl(Tuple&& tuple, F&& f, RAH2_STD::index_sequence<Indices...>)
            {
                using swallow = int[];
                (void)swallow{
                    1,
                    (f(RAH2_STD::get<Indices>(RAH2_STD::forward<Tuple>(tuple))), void(), int{})...};
            }

            template <typename Tuple, typename F>
            void for_each(Tuple&& tuple, F&& f)
            {
                constexpr size_t N = RAH2_STD::tuple_size<RAH2_STD::remove_reference_t<Tuple>>::value;
                for_each_impl(
                    RAH2_STD::forward<Tuple>(tuple),
                    RAH2_STD::forward<F>(f),
                    RAH2_STD::make_index_sequence<N>{});
            }

            template <typename Tuple, size_t Index, typename Check>
            struct all_type_impl
            {
                static constexpr bool value =
                    Check::template value<RAH2_STD::tuple_element_t<Index, Tuple>>
                    && all_type_impl<Tuple, Index - 1, Check>::value;
            };

            template <typename Tuple, typename Check>
            struct all_type_impl<Tuple, 0, Check>
            {
                static constexpr bool value =
                    Check::template value<RAH2_STD::tuple_element_t<0, Tuple>>;
            };

            template <typename Tuple, typename Check>
            constexpr bool all_type()
            {
                return all_type_impl<Tuple, RAH2_STD::tuple_size<Tuple>::value - 1, Check>::value;
            }

            template <class F, typename... Args, size_t... Is>
            auto transform_each_impl(
                RAH2_STD::tuple<Args...> const& t, F&& f, RAH2_STD::index_sequence<Is...>)
            {
                return RAH2_STD::make_tuple(f(RAH2_STD::get<Is>(t))...);
            }

            template <class F, typename... Args, size_t... Is>
            auto transform_each_impl(RAH2_STD::tuple<Args...>& t, F&& f, RAH2_STD::index_sequence<Is...>)
            {
                return RAH2_STD::make_tuple(f(RAH2_STD::get<Is>(t))...);
            }

            template <class F, typename... Args>
            auto transform_each(RAH2_STD::tuple<Args...> const& t, F&& f)
            {
                return transform_each_impl(
                    t, RAH2_STD::forward<F>(f), RAH2_STD::make_index_sequence<sizeof...(Args)>{});
            }

            template <class F, typename... Args>
            auto transform_each(RAH2_STD::tuple<Args...>& t, F&& f)
            {
                return transform_each_impl(
                    t, RAH2_STD::forward<F>(f), RAH2_STD::make_index_sequence<sizeof...(Args)>{});
            }

            template <typename... Args, size_t... Is>
            auto zip_range_reference_impl(
                RAH2_STD::tuple<Args...> const& t, RAH2_STD::index_sequence<Is...>)
            {
                return RAH2_STD::tuple<typename RAH2_STD::iterator_traits<Args>::reference...>(
                    (*RAH2_STD::get<Is>(t))...);
            }

            template <typename... Args>
            auto zip_range_reference(RAH2_STD::tuple<Args...> const& t)
            {
                return zip_range_reference_impl(t, RAH2_STD::make_index_sequence<sizeof...(Args)>{});
            }

            template <typename... Args, size_t... Is>
            auto zip_range_reference_impl(RAH2_STD::tuple<Args...>& t, RAH2_STD::index_sequence<Is...>)
            {
                return RAH2_STD::tuple<typename RAH2_STD::iterator_traits<Args>::reference...>(
                    (*RAH2_STD::get<Is>(t))...);
            }

            template <typename... Args>
            auto zip_range_reference(RAH2_STD::tuple<Args...>& t)
            {
                return zip_range_reference_impl(t, RAH2_STD::make_index_sequence<sizeof...(Args)>{});
            }

            template <typename... Args, size_t... Is>
            auto zip_range_value_impl(RAH2_STD::tuple<Args...> const& t, RAH2_STD::index_sequence<Is...>)
            {
                return RAH2_STD::tuple<typename RAH2_STD::iterator_traits<Args>::value_type...>(
                    (*RAH2_STD::get<Is>(t))...);
            }

            template <typename... Args>
            auto zip_range_value(RAH2_STD::tuple<Args...> const& t)
            {
                return zip_range_value_impl(t, RAH2_STD::make_index_sequence<sizeof...(Args)>{});
            }

            template <typename... Args, size_t... Is>
            auto zip_range_value_impl(RAH2_STD::tuple<Args...>& t, RAH2_STD::index_sequence<Is...>)
            {
                return RAH2_STD::tuple<typename RAH2_STD::iterator_traits<Args>::value_type...>(
                    (*RAH2_STD::get<Is>(t))...);
            }

            template <typename... Args>
            auto zip_range_value(RAH2_STD::tuple<Args...>& t)
            {
                return zip_range_value_impl(t, RAH2_STD::make_index_sequence<sizeof...(Args)>{});
            }

            template <typename F, typename... Args, size_t... Is>
            auto deref_call_impl(
                F const& func, RAH2_STD::tuple<Args...>& t, RAH2_STD::index_sequence<Is...>)
            {
                return func((*RAH2_STD::get<Is>(t))...);
            }

            template <typename F, typename... Args>
            auto deref_call(F const& func, RAH2_STD::tuple<Args...>& t)
            {
                return deref_call_impl(func, t, RAH2_STD::make_index_sequence<sizeof...(Args)>{});
            }

            template <size_t Index>
            struct equal_tuple
            {
                template <typename... Args, typename... Args2>
                bool operator()(RAH2_STD::tuple<Args...> const& a, RAH2_STD::tuple<Args2...> const& b) const
                {
                    // Used to find the end of the sequence, so if one iterator is end, it is the end
                    // This is why OR is used.
                    return (RAH2_STD::get<Index - 1>(a) == RAH2_STD::get<Index - 1>(b))
                           || equal_tuple<Index - 1>{}(a, b);
                }
            };

            template <>
            struct equal_tuple<0>
            {
                template <typename... Args, typename... Args2>
                bool operator()(RAH2_STD::tuple<Args...> const&, RAH2_STD::tuple<Args2...> const&) const
                {
                    return false;
                }
            };

            template <typename... Args, typename... Args2>
            auto equal(RAH2_STD::tuple<Args...> const& a, RAH2_STD::tuple<Args2...> const& b)
            {
                return equal_tuple<sizeof...(Args)>{}(a, b);
            }

            template <size_t Index>
            struct lesser_tuple
            {
                template <typename... Args, typename... Args2>
                bool operator()(RAH2_STD::tuple<Args...> const& a, RAH2_STD::tuple<Args2...> const& b) const
                {
                    return (RAH2_STD::get<Index - 1>(a) < RAH2_STD::get<Index - 1>(b))
                           && lesser_tuple<Index - 1>{}(a, b);
                }
            };

            template <>
            struct lesser_tuple<0>
            {
                template <typename... Args, typename... Args2>
                bool operator()(RAH2_STD::tuple<Args...> const&, RAH2_STD::tuple<Args2...> const&) const
                {
                    return true;
                }
            };

            template <typename... Args, typename... Args2>
            auto lesser(RAH2_STD::tuple<Args...> const& a, RAH2_STD::tuple<Args2...> const& b)
            {
                return lesser_tuple<sizeof...(Args)>{}(a, b);
            }

            template <typename Arg1>
            auto min(Arg1 val)
            {
                return val;
            }

            template <typename Arg1, typename... Args>
            auto min(Arg1 val, Args... others)
            {
                return RAH2_NS::details::min<Arg1>(val, min(Arg1(others)...));
            }

            struct compute_min_size
            {
                template <typename... Args>
                auto operator()(Args&&... args) const
                {
                    return details::min(RAH2_STD::forward<Args>(args)...);
                }
            };

            template <typename... Args, size_t... Is>
            auto get_begin_tuple_impl(RAH2_STD::tuple<Args...> const& t, RAH2_STD::index_sequence<Is...>)
            {
                return RAH2_STD::make_tuple((RAH2_NS::ranges::begin(RAH2_STD::get<Is>(t)))...);
            }

            template <typename... Args>
            auto get_begin_tuple(RAH2_STD::tuple<Args...> const& a)
            {
                return get_begin_tuple_impl(a, RAH2_STD::make_index_sequence<sizeof...(Args)>{});
            }

            template <typename... Args, size_t... Is>
            auto get_end_tuple_impl(RAH2_STD::tuple<Args...> const& t, RAH2_STD::index_sequence<Is...>)
            {
                return RAH2_STD::make_tuple((RAH2_NS::ranges::end(RAH2_STD::get<Is>(t)))...);
            }

            template <typename... Args>
            auto get_end_tuple(RAH2_STD::tuple<Args...> const& a)
            {
                return get_end_tuple_impl(a, RAH2_STD::make_index_sequence<sizeof...(Args)>{});
            }

            template <typename Function, typename Tuple, size_t... I>
            auto apply(Function&& f, Tuple&& t, RAH2_STD::index_sequence<I...>)
            {
                return f(RAH2_STD::get<I>(t)...);
            }

            template <typename Function, typename Tuple>
            auto apply(Function&& f, Tuple&& t)
            {
                static constexpr auto tup_size =
                    RAH2_STD::tuple_size<RAH2_STD::remove_reference_t<Tuple>>::value;
                return apply(f, t, RAH2_STD::make_index_sequence<tup_size>{});
            }
            struct range_begin
            {
                template <typename R>
                auto operator()(R&& r) const
                {
                    return RAH2_NS::ranges::begin(r);
                }
            };
            struct range_end
            {
                template <typename R>
                auto operator()(R&& r) const
                {
                    return RAH2_NS::ranges::end(r);
                }
            };
            template <typename Cat>
            struct range_has_cat
            {
                template <typename Range>
                static constexpr bool value =
                    RAH2_NS::derived_from<RAH2_NS::ranges::range_iter_categ_t<Range>, Cat>;
            };
            template <typename RangeTuple>
            using tuple_base_cat = RAH2_STD::conditional_t<
                details::all_type<RangeTuple, details::range_has_cat<RAH2_NS::contiguous_iterator_tag>>(),
                RAH2_NS::contiguous_iterator_tag,
                RAH2_STD::conditional_t<
                    details::all_type<RangeTuple, details::range_has_cat<RAH2_NS::random_access_iterator_tag>>(),
                    RAH2_NS::random_access_iterator_tag,
                    RAH2_STD::conditional_t<
                        details::all_type<RangeTuple, details::range_has_cat<RAH2_NS::bidirectional_iterator_tag>>(),
                        RAH2_NS::bidirectional_iterator_tag,
                        RAH2_STD::conditional_t<
                            details::all_type<RangeTuple, details::range_has_cat<RAH2_NS::forward_iterator_tag>>(),
                            RAH2_NS::forward_iterator_tag,
                            RAH2_STD::conditional_t<
                                details::all_type<RangeTuple, details::range_has_cat<RAH2_NS::input_iterator_tag>>(),
                                RAH2_NS::input_iterator_tag,
                                bool>>>>>;

        } // namespace details
        /// \endcond

        template <typename RangeTuple>
        class zip_view : public view_interface<zip_view<RangeTuple>>
        {
            RangeTuple bases_;
            using IterTuple = decltype(details::transform_each(bases_, details::range_begin()));
            using SentinelTuple = decltype(details::transform_each(bases_, details::range_end()));

            using base_cat =
                cap_iterator_tag<details::tuple_base_cat<RangeTuple>, input_iterator_tag, random_access_iterator_tag>;
            struct is_sized_range
            {
                template <typename Range>
                static constexpr bool value = RAH2_NS::ranges::sized_range<Range>;
            };
            struct is_const_sized_range
            {
                template <typename Range>
                static constexpr bool value = RAH2_NS::ranges::sized_range<Range const>;
            };
            struct is_borrowed_range_impl
            {
                template <typename Range>
                static constexpr bool value = RAH2_NS::ranges::borrowed_range<Range const>;
            };
            static constexpr bool common_one_range =
                RAH2_STD::tuple_size<RangeTuple>::value == 1
                && RAH2_NS::ranges::common_range<RAH2_STD::tuple_element_t<0, RangeTuple>>;
            static constexpr bool all_sized = details::all_type<RangeTuple, is_sized_range>();
            static constexpr bool all_const_sized =
                details::all_type<RangeTuple, is_const_sized_range>();
            static constexpr bool common_all_sized_random_access =
                details::all_type<RangeTuple, details::range_has_cat<RAH2_NS::random_access_iterator_tag>>()
                && all_sized;

        public:
            static constexpr bool is_borrowed_range =
                details::all_type<RangeTuple, is_borrowed_range_impl>();
            struct sentinel
            {
                SentinelTuple sentinels;
            };

            class iterator
                : public iterator_facade<
                      iterator,
                      sentinel,
                      decltype(details::zip_range_reference(RAH2_STD::declval<IterTuple&>())),
                      base_cat>
            {
                IterTuple iters_;

            public:
                iterator() = default;
                explicit iterator(IterTuple iters)
                    : iters_(RAH2_STD::move(iters))
                {
                }
                iterator& operator++()
                {
                    details::for_each(iters_, [](auto& iter) { ++iter; });
                    return *this;
                }
                RAH2_POST_INCR(base_cat)
                iterator& operator+=(intptr_t val)
                {
                    details::for_each(iters_, [val](auto& iter) { iter += val; });
                    return *this;
                }
                template <
                    typename C = base_cat,
                    RAH2_STD::enable_if_t<RAH2_NS::derived_from<C, RAH2_STD::bidirectional_iterator_tag>>* = nullptr>
                iterator& operator--()
                {
                    details::for_each(iters_, [](auto& iter) { --iter; });
                    return *this;
                }
                template <
                    typename C = base_cat,
                    RAH2_STD::enable_if_t<RAH2_NS::derived_from<C, RAH2_STD::bidirectional_iterator_tag>>* =
                        nullptr>
                RAH2_POST_DECR;
                auto operator*()
                {
                    return details::zip_range_reference(iters_);
                }
                template <
                    typename C = base_cat,
                    RAH2_STD::enable_if_t<RAH2_NS::derived_from<C, RAH2_STD::random_access_iterator_tag>>* = nullptr>
                auto operator-(iterator const& other) const
                {
                    return RAH2_STD::get<0>(iters_) - RAH2_STD::get<0>(other.iters_);
                }
                template <
                    typename C = base_cat,
                    RAH2_STD::enable_if_t<RAH2_NS::derived_from<C, RAH2_STD::random_access_iterator_tag>>* = nullptr>
                auto operator<(iterator const& other) const
                {
                    return details::lesser(iters_, other.iters_);
                }
                template <
                    typename C = base_cat,
                    RAH2_STD::enable_if_t<RAH2_NS::derived_from<C, RAH2_STD::forward_iterator_tag>>* = nullptr>
                bool operator==(iterator const& iter) const
                {
                    return details::equal(iters_, iter.iters_);
                }
                friend bool operator==(iterator const& iter, sentinel const& sent)
                {
                    return details::equal(iter.iters_, sent.sentinels);
                }
                friend bool operator==(sentinel const& sent, iterator const& it)
                {
                    return details::equal(it.iters_, sent.sentinels);
                }
            };

            explicit zip_view(RangeTuple rangeTuple)
                : bases_(RAH2_STD::move(rangeTuple))
            {
            }

            iterator begin()
            {
                return iterator(details::transform_each(bases_, details::range_begin()));
            }

            template <bool C = common_one_range, RAH2_STD::enable_if_t<C>* = nullptr>
            iterator end()
            {
                return iterator(details::transform_each(bases_, details::range_end()));
            }

            template <
                bool A = common_one_range,
                bool B = common_all_sized_random_access,
                RAH2_STD::enable_if_t<!A && B>* = nullptr>
            iterator end()
            {
                auto sizes = details::transform_each(
                    bases_, [](auto&& r) { return RAH2_NS::ranges::size(r); });
                auto const min_size = details::apply(details::compute_min_size(), sizes);
                return iterator(details::transform_each(
                    bases_,
                    [min_size](auto&& r) {
                        return RAH2_NS::ranges::begin(r)
                               + static_cast<range_difference_t<decltype(r)>>(min_size);
                    }));
            }

            template <
                bool A = common_one_range,
                bool B = common_all_sized_random_access,
                RAH2_STD::enable_if_t<!A && !B>* = nullptr>
            sentinel end()
            {
                return sentinel{details::transform_each(bases_, details::range_end())};
            }

            template <bool AllSized = all_const_sized, RAH2_STD::enable_if_t<AllSized>* = nullptr>
            size_t size() const
            {
                auto sizes = details::transform_each(
                    bases_, [](auto&& r) { return RAH2_NS::ranges::size(r); });
                return details::apply(details::compute_min_size(), sizes);
            }
            template <bool IsSized = all_sized, RAH2_STD::enable_if_t<IsSized>* = nullptr>
            size_t size()
            {
                auto sizes = details::transform_each(
                    bases_, [](auto&& r) { return RAH2_NS::ranges::size(r); });
                return details::apply(details::compute_min_size(), sizes);
            }
        };
        template <class... Views>
        constexpr bool enable_borrowed_range<zip_view<Views...>> =
            zip_view<Views...>::is_borrowed_range;
        namespace views
        {
            template <typename... R>
            auto zip(R&&... ranges)
            {
                auto refTuple = RAH2_STD::make_tuple(all(RAH2_STD::forward<R>(ranges))...);
                return zip_view<decltype(refTuple)>(RAH2_STD::move(refTuple));
            }
        } // namespace views

        // ************************************ zip_transform *************************************

        template <typename Func, typename RangeTuple>
        class zip_transform_view : public view_interface<zip_view<RangeTuple>>
        {
            Func func_;
            RangeTuple bases_;
            using IterTuple = decltype(details::transform_each(bases_, details::range_begin()));
            using SentinelTuple = decltype(details::transform_each(bases_, details::range_end()));

            using base_cat =
                common_iterator_tag<details::tuple_base_cat<RangeTuple>, RAH2_NS::random_access_iterator_tag>;
            static constexpr size_t tuple_size = RAH2_STD::tuple_size<RangeTuple>::value;
            struct is_sized_range
            {
                template <typename Range>
                static constexpr bool value = RAH2_NS::ranges::sized_range<Range>;
            };
            struct is_const_sized_range
            {
                template <typename Range>
                static constexpr bool value = RAH2_NS::ranges::sized_range<Range const>;
            };
            struct is_borrowed_range_impl
            {
                template <typename Range>
                static constexpr bool value = RAH2_NS::ranges::borrowed_range<Range const>;
            };
            static constexpr bool common_one_range =
                tuple_size == 1
                && RAH2_NS::ranges::common_range<RAH2_STD::tuple_element_t<0, RangeTuple>>;
            static constexpr bool all_sized = details::all_type<RangeTuple, is_sized_range>();
            static constexpr bool all_const_sized =
                details::all_type<RangeTuple, is_const_sized_range>();
            static constexpr bool common_all_sized_random_access =
                details::all_type<RangeTuple, details::range_has_cat<RAH2_NS::random_access_iterator_tag>>()
                && all_sized;
            using reference_t = decltype(details::deref_call_impl(
                func_, RAH2_STD::declval<IterTuple&>(), RAH2_STD::make_index_sequence<tuple_size>{}));

        public:
            static constexpr bool is_borrowed_range =
                details::all_type<RangeTuple, is_borrowed_range_impl>();
            struct sentinel
            {
                SentinelTuple sentinels;
            };

            class iterator : public iterator_facade<iterator, sentinel, reference_t, base_cat>
            {
                IterTuple iters_;
                zip_transform_view* parent_ = nullptr;

            public:
                iterator() = default;
                iterator(zip_transform_view* parent, IterTuple iterators)
                    : iters_(RAH2_STD::move(iterators))
                    , parent_(parent)
                {
                }
                iterator& operator++()
                {
                    details::for_each(iters_, [](auto& iter) { ++iter; });
                    return *this;
                }
                RAH2_POST_INCR(base_cat)
                iterator& operator+=(intptr_t val)
                {
                    details::for_each(iters_, [val](auto& iter) { iter += val; });
                    return *this;
                }
                template <
                    typename C = base_cat,
                    RAH2_STD::enable_if_t<RAH2_NS::derived_from<C, RAH2_STD::bidirectional_iterator_tag>>* = nullptr>
                iterator& operator--()
                {
                    details::for_each(iters_, [](auto& iter) { --iter; });
                    return *this;
                }
                template <
                    typename C = base_cat,
                    RAH2_STD::enable_if_t<RAH2_NS::derived_from<C, RAH2_STD::bidirectional_iterator_tag>>* =
                        nullptr>
                RAH2_POST_DECR;
                reference_t operator*()
                {
                    return details::deref_call_impl(
                        parent_->func_, iters_, RAH2_STD::make_index_sequence<tuple_size>{});
                }
                template <
                    typename C = base_cat,
                    RAH2_STD::enable_if_t<RAH2_NS::derived_from<C, RAH2_STD::random_access_iterator_tag>>* = nullptr>
                auto operator-(iterator const& other) const
                {
                    return RAH2_STD::get<0>(iters_) - RAH2_STD::get<0>(other.iters_);
                }
                template <
                    typename C = base_cat,
                    RAH2_STD::enable_if_t<RAH2_NS::derived_from<C, RAH2_STD::random_access_iterator_tag>>* = nullptr>
                auto operator<(iterator const& other) const
                {
                    return details::lesser(iters_, other.iters_);
                }
                template <
                    typename C = base_cat,
                    RAH2_STD::enable_if_t<RAH2_NS::derived_from<C, RAH2_STD::forward_iterator_tag>>* = nullptr>
                bool operator==(iterator const& iter) const
                {
                    return details::equal(iters_, iter.iters_);
                }
                friend bool operator==(iterator const& iter, sentinel const& sent)
                {
                    return details::equal(iter.iters_, sent.sentinels);
                }
                friend bool operator==(sentinel const& sent, iterator const& it)
                {
                    return details::equal(it.iters_, sent.sentinels);
                }
            };

            explicit zip_transform_view(Func f, RangeTuple rangeTuple)
                : func_(RAH2_STD::move(f))
                , bases_(RAH2_STD::move(rangeTuple))
            {
            }

            iterator begin()
            {
                return iterator(this, details::transform_each(bases_, details::range_begin()));
            }

            template <bool C = common_one_range, RAH2_STD::enable_if_t<C>* = nullptr>
            iterator end()
            {
                return iterator(this, details::transform_each(bases_, details::range_end()));
            }

            template <
                bool A = common_one_range,
                bool B = common_all_sized_random_access,
                RAH2_STD::enable_if_t<!A && B>* = nullptr>
            iterator end()
            {
                auto sizes = details::transform_each(
                    bases_, [](auto&& r) { return RAH2_NS::ranges::size(r); });
                auto const min_size = details::apply(details::compute_min_size(), sizes);
                return iterator(
                    this,
                    details::transform_each(
                        bases_,
                        [min_size](auto&& r) {
                            return RAH2_NS::ranges::begin(r)
                                   + static_cast<range_difference_t<decltype(r)>>(min_size);
                        }));
            }

            template <
                bool A = common_one_range,
                bool B = common_all_sized_random_access,
                RAH2_STD::enable_if_t<!A && !B>* = nullptr>
            sentinel end()
            {
                return sentinel{details::transform_each(bases_, details::range_end())};
            }

            template <bool AllSized = all_const_sized, RAH2_STD::enable_if_t<AllSized>* = nullptr>
            size_t size() const
            {
                auto sizes = details::transform_each(
                    bases_, [](auto&& r) { return RAH2_NS::ranges::size(r); });
                return details::apply(details::compute_min_size(), sizes);
            }
            template <bool IsSized = all_sized, RAH2_STD::enable_if_t<IsSized>* = nullptr>
            size_t size()
            {
                auto sizes = details::transform_each(
                    bases_, [](auto&& r) { return RAH2_NS::ranges::size(r); });
                return details::apply(details::compute_min_size(), sizes);
            }
        };
        template <class... Views>
        constexpr bool enable_borrowed_range<zip_transform_view<Views...>> =
            zip_transform_view<Views...>::is_borrowed_range;
        namespace views
        {
            template <typename F, typename... R>
            auto zip_transform(F&& func, R&&... ranges)
            {
                auto ref_tuple = RAH2_STD::make_tuple(all(RAH2_STD::forward<R>(ranges))...);
                return zip_transform_view<RAH2_STD::remove_reference_t<F>, decltype(ref_tuple)>(
                    RAH2_STD::forward<F>(func), RAH2_STD::move(ref_tuple));
            }
        } // namespace views

        // ******************************************* adjacent ***********************************

        template <typename R, size_t N>
        class adjacent_view : public view_interface<adjacent_view<R, N>>
        {
            R input_view_;

            static_assert(forward_range<R>, "adjacent_view expect at least a forward_range");
            static_assert(N != 0, "In adjacent_view N should be greater than zero");

            using base_iterator = iterator_t<R>;
            using base_sentinel = sentinel_t<R>;
            using base_value = range_value_t<R>;
            using base_reference = range_reference_t<R>;
            using iterator_category =
                cap_iterator_tag<range_iter_categ_t<R>, forward_iterator_tag, random_access_iterator_tag>;
            static constexpr bool is_sized = sized_range<R>;
            static constexpr bool is_common = common_range<R> && bidirectional_range<R>;

            template <size_t I>
            struct GetType
            {
                using type = base_value;
            };

            template <size_t... Is>
            static auto make_tuple(RAH2_STD::index_sequence<Is...>)
            {
                return RAH2_STD::tuple<typename GetType<Is>::type...>();
            }

        public:
            using value = decltype(make_tuple(RAH2_STD::make_integer_sequence<size_t, N>{}));
            using reference = value;

            struct sentinel
            {
                base_sentinel sent;
            };

            struct iterator : iterator_facade<iterator, sentinel, reference, iterator_category>
            {
                base_iterator subRangeBegin_;
                base_value sliding_result[N];
                size_t result_output_index = 0;

                iterator() = default;
                iterator(base_iterator subRangeBegin, base_sentinel sentinel)
                    : subRangeBegin_(RAH2_STD::move(subRangeBegin))
                {
                    for (; result_output_index < (N - 1) && subRangeBegin_ != sentinel;
                         ++result_output_index, ++subRangeBegin_)
                    {
                        sliding_result[result_output_index] = *subRangeBegin_;
                    }
                }

                iterator& operator++()
                {
                    ++subRangeBegin_;
                    result_output_index = (result_output_index + 1) % N;
                    return *this;
                }
                RAH2_POST_INCR(iterator_category)
                template <
                    typename C = iterator_category,
                    RAH2_STD::enable_if_t<derived_from<C, RAH2_STD::bidirectional_iterator_tag>>* = nullptr>
                iterator& operator--()
                {
                    --subRangeBegin_;
                    result_output_index = (result_output_index - 1) % N;
                    return *this;
                }
                template <
                    typename C = iterator_category,
                    RAH2_STD::enable_if_t<derived_from<C, RAH2_STD::bidirectional_iterator_tag>>* = nullptr>
                RAH2_POST_DECR;
                template <size_t... Is>
                auto make_result(RAH2_STD::index_sequence<Is...>)
                {
                    sliding_result[result_output_index] = *subRangeBegin_;
                    return RAH2_STD::make_tuple(
                        (sliding_result[(result_output_index + 1 + Is) % N])...);
                }

                auto operator*()
                {
                    return make_result(RAH2_STD::make_integer_sequence<size_t, N>{});
                }
                template <
                    typename C = iterator_category,
                    RAH2_STD::enable_if_t<derived_from<C, RAH2_STD::forward_iterator_tag>>* = nullptr>
                bool operator==(iterator const& i2) const
                {
                    return subRangeBegin_ == i2.subRangeBegin_;
                }
                friend bool operator==(iterator const& i, sentinel const& s)
                {
                    return i.subRangeBegin_ == s.sent;
                }
                template <
                    typename C = iterator_category,
                    RAH2_STD::enable_if_t<derived_from<C, RAH2_STD::random_access_iterator_tag>>* = nullptr>
                bool operator<(iterator iter) const
                {
                    return subRangeBegin_ < iter.subRangeBegin_;
                }
                template <
                    typename C = iterator_category,
                    RAH2_STD::enable_if_t<derived_from<C, RAH2_STD::random_access_iterator_tag>>* = nullptr>
                intptr_t operator-(iterator iter) const
                {
                    return subRangeBegin_ - iter.subRangeBegin_;
                }
                template <
                    typename C = iterator_category,
                    RAH2_STD::enable_if_t<derived_from<C, RAH2_STD::random_access_iterator_tag>>* = nullptr>
                iterator& operator+=(intptr_t value)
                {
                    subRangeBegin_ += value;
                    return *this;
                }
            };

            explicit adjacent_view(R inputView)
                : input_view_(RAH2_STD::move(inputView))
            {
            }

            auto begin()
            {
                auto const range_end = RAH2_NS::ranges::end(input_view_);
                auto const sub_range_begin = RAH2_NS::ranges::begin(input_view_);
                return iterator(RAH2_STD::move(sub_range_begin), RAH2_STD::move(range_end));
            }

            template <bool IsCommon = is_common, RAH2_STD::enable_if_t<IsCommon>* = nullptr>
            auto end()
            {
                auto sub_range_begin = RAH2_NS::ranges::end(input_view_);
                RAH2_NS::ranges::advance(
                    sub_range_begin,
                    -RAH2_NS::ranges::range_difference_t<R>(N - 1),
                    RAH2_NS::ranges::begin(input_view_));
                return iterator{RAH2_STD::move(sub_range_begin), RAH2_NS::ranges::end(input_view_)};
            }
            template <bool IsCommon = is_common, RAH2_STD::enable_if_t<not IsCommon>* = nullptr>
            auto end()
            {
                return sentinel{RAH2_NS::ranges::end(input_view_)};
            }

            template <bool IsSized = RAH2_NS::ranges::sized_range<R const>, RAH2_STD::enable_if_t<IsSized>* = nullptr>
            auto size() const
            {
                auto base_size = RAH2_NS::ranges::size(input_view_);
                auto missing = N - 1;
                return range_size_t<R>(base_size - missing);
            }
            template <bool IsSized = RAH2_NS::ranges::sized_range<R>, RAH2_STD::enable_if_t<IsSized>* = nullptr>
            auto size()
            {
                auto base_size = RAH2_NS::ranges::size(input_view_);
                auto missing = N - 1;
                return range_size_t<R>(base_size - missing);
            }
        };
        template <class V, size_t N>
        constexpr bool enable_borrowed_range<adjacent_view<V, N>> = enable_borrowed_range<V>;

        namespace views
        {
            template <size_t N>
            struct adjacent_raco : closure_object_facade<adjacent_raco<N>>
            {
                template <typename R, size_t M = N, RAH2_STD::enable_if_t<M != 0>* = nullptr>
                auto operator()(R&& range) const
                {
                    auto range_view = all(range);
                    return adjacent_view<decltype(range_view), N>(RAH2_STD::move(range_view));
                }

                template <typename R, size_t M = N, RAH2_STD::enable_if_t<M == 0>* = nullptr>
                auto operator()(R&&) const
                {
                    return views::empty<RAH2_STD::tuple<>>;
                }
            };
            template <size_t N>
            constexpr adjacent_raco<N> adjacent;

            struct pairwise_raco : closure_object_facade<pairwise_raco>
            {
                template <typename R>
                auto pairwise(R&& range)
                {
                    auto range_view = all(range);
                    return adjacent_view<decltype(range_view), 2>(RAH2_STD::move(range_view));
                }
            };
            constexpr pairwise_raco pairwise;
        } // namespace views

        // ********************************* adjacent_transform ***********************************

        template <typename R, typename F, size_t N>
        class adjacent_transform_view : public view_interface<adjacent_transform_view<R, F, N>>
        {
            F func_;

            using inner_reference = range_reference_t<R>;

            using inner_adjacent_view = adjacent_view<R, N>;
            inner_adjacent_view inner_;

            using adjacent_iterator = iterator_t<inner_adjacent_view>;
            using adjacent_reference = range_reference_t<inner_adjacent_view>;
            using adjacent_sentinel = sentinel_t<inner_adjacent_view>;
            using base_cat = cap_iterator_tag<
                RAH2_NS::ranges::range_iter_categ_t<R>,
                RAH2_STD::forward_iterator_tag,
                RAH2_STD::random_access_iterator_tag>;

        public:
            using reference =
                decltype(details::apply(func_, RAH2_STD::declval<adjacent_reference>()));
            using value = reference;

            struct sentinel
            {
                adjacent_sentinel sent;
            };

            class iterator : public iterator_facade<iterator, sentinel, reference, base_cat>
            {
                adjacent_transform_view* view_ = nullptr;
                adjacent_iterator iter_;

            public:
                iterator() = default;
                iterator(adjacent_transform_view* parent, adjacent_iterator iter)
                    : view_(parent)
                    , iter_(RAH2_STD::move(iter))
                {
                }

                iterator& operator++()
                {
                    ++iter_;
                    return *this;
                }
                RAH2_POST_INCR(base_cat)

                template <
                    typename C = base_cat,
                    RAH2_STD::enable_if_t<derived_from<C, bidirectional_iterator_tag>>* = nullptr>
                iterator& operator--()
                {
                    --iter_;
                    return *this;
                }

                template <
                    typename C = base_cat,
                    RAH2_STD::enable_if_t<derived_from<C, bidirectional_iterator_tag>>* = nullptr>
                RAH2_POST_DECR;

                auto operator*()
                {
                    return details::apply(view_->func_, *iter_, RAH2_STD::make_index_sequence<N>{});
                }

                template <
                    typename C = base_cat,
                    RAH2_STD::enable_if_t<derived_from<C, random_access_iterator_tag>>* = nullptr>
                auto operator<(iterator const& i) const
                {
                    return iter_ < i.iter_;
                }

                template <
                    typename C = base_cat,
                    RAH2_STD::enable_if_t<derived_from<C, random_access_iterator_tag>>* = nullptr>
                iterator& operator+=(range_difference_t<R> diff)
                {
                    iter_ += diff;
                    return *this;
                }

                template <
                    typename C = base_cat,
                    RAH2_STD::enable_if_t<derived_from<C, random_access_iterator_tag>>* = nullptr>
                auto operator-(iterator const& i) const
                {
                    return iter_ - i.iter_;
                }

                friend bool operator==(iterator const& i, iterator const& i2)
                {
                    return i.iter_ == i2.iter_;
                }
                friend bool operator==(iterator const& i, sentinel const& s)
                {
                    return i.iter_ == s.sent;
                }
                friend bool operator==(sentinel const& s, iterator const& i)
                {
                    return i.iter_ == s.sent;
                }
            };

            adjacent_transform_view(R inputView, F func)
                : func_(RAH2_STD::move(func))
                , inner_(RAH2_STD::move(inputView))
            {
            }

            auto begin()
            {
                return iterator(this, RAH2_NS::ranges::begin(inner_));
            }

            template <
                bool IsCommon = RAH2_NS::ranges::common_range<inner_adjacent_view>,
                RAH2_STD::enable_if_t<IsCommon>* = nullptr>
            auto end()
            {
                return iterator(this, RAH2_NS::ranges::end(inner_));
            }

            template <
                bool IsCommon = RAH2_NS::ranges::common_range<inner_adjacent_view>,
                RAH2_STD::enable_if_t<!IsCommon>* = nullptr>
            auto end()
            {
                return sentinel{RAH2_NS::ranges::end(inner_)};
            }

            template <
                bool IsSized = RAH2_NS::ranges::sized_range<inner_adjacent_view const>,
                RAH2_STD::enable_if_t<IsSized>* = nullptr>
            auto size() const
            {
                return range_size_t<R>(RAH2_NS::ranges::size(inner_));
            }
            template <
                bool IsSized = RAH2_NS::ranges::sized_range<inner_adjacent_view>,
                RAH2_STD::enable_if_t<IsSized>* = nullptr>
            auto size()
            {
                return range_size_t<R>(RAH2_NS::ranges::size(inner_));
            }
        };
        namespace views
        {
            template <size_t N>
            struct adjacent_transform_raco
            {
                template <typename R, typename F, size_t M = N, RAH2_STD::enable_if_t<M != 0>* = nullptr>
                auto operator()(R&& range, F&& func) const
                {
                    auto range_view = all(range);
                    return adjacent_transform_view<decltype(range_view), F, N>(
                        RAH2_STD::move(range_view), RAH2_STD::forward<F>(func));
                }

                template <typename R, typename F, size_t M = N, RAH2_STD::enable_if_t<M == 0>* = nullptr>
                auto operator()(R&&, F&&) const
                {
                    return views::empty<RAH2_STD::tuple<>>;
                }

                template <typename F>
                auto operator()(F&& func) const
                {
                    return make_pipeable(
                        [this, func = RAH2_STD::forward<F>(func)](auto&& range) {
                            return (*this)(
                                RAH2_STD::forward<decltype(range)>(range), RAH2_STD::move(func));
                        });
                }
            };
            template <size_t N>
            constexpr adjacent_transform_raco<N> adjacent_transform;
        } // namespace views
        // ******************************************* sliding ********************************************

        template <typename R>
        class slide_view : public view_interface<slide_view<R>>
        {
            R input_view;
            intptr_t count_;

            static_assert(forward_range<R>, "slide_view expect a forward_range");

            using base_iterator = iterator_t<R>;
            using base_sentinel = sentinel_t<R>;
            using base_cat =
                cap_iterator_tag<RAH2_NS::ranges::range_iter_categ_t<R>, forward_iterator_tag, random_access_iterator_tag>;
            constexpr static bool is_common = common_range<R> && bidirectional_range<R>;

        public:
            struct sentinel
            {
                base_sentinel sent;
            };

            struct iterator
                : iterator_facade<iterator, sentinel, subrange<base_iterator, base_iterator>, base_cat>
            {
                // Actually store a closed range [begin, last]
                //   to avoid to exceed the end iterator of the underlying range
                // The last valid iterator will have subRangeLast_ equal to the end iterator of base
                // So the "past-the-end" range will overcome the end iterator of base
                base_iterator subRangeBegin_;
                base_iterator subRangeLast_;

                iterator() = default;
                iterator(base_iterator subRangeBegin, base_iterator subRangeLast)
                    : subRangeBegin_(RAH2_STD::move(subRangeBegin))
                    , subRangeLast_(RAH2_STD::move(subRangeLast))
                {
                }

                iterator& operator++()
                {
                    ++subRangeBegin_;
                    ++subRangeLast_;
                    return *this;
                }
                RAH2_POST_INCR(base_cat)
                template <
                    typename C = base_cat,
                    RAH2_STD::enable_if_t<derived_from<C, random_access_iterator_tag>>* = nullptr>
                iterator& operator+=(intptr_t off)
                {
                    subRangeBegin_ += off;
                    subRangeLast_ += off;
                    return *this;
                }
                template <
                    typename C = base_cat,
                    RAH2_STD::enable_if_t<derived_from<C, bidirectional_iterator_tag>>* = nullptr>
                iterator& operator--()
                {
                    --subRangeBegin_;
                    --subRangeLast_;
                    return *this;
                }
                template <
                    typename C = base_cat,
                    RAH2_STD::enable_if_t<derived_from<C, bidirectional_iterator_tag>>* = nullptr>
                RAH2_POST_DECR;

                template <
                    typename C = base_cat,
                    RAH2_STD::enable_if_t<derived_from<C, random_access_iterator_tag>>* = nullptr>
                auto operator-(iterator const& r) const
                {
                    return subRangeBegin_ - r.subRangeBegin_;
                }
                template <
                    typename C = base_cat,
                    RAH2_STD::enable_if_t<derived_from<C, random_access_iterator_tag>>* = nullptr>
                auto operator<(iterator const& r) const
                {
                    return subRangeBegin_ < r.subRangeBegin_;
                }
                auto operator*() const
                {
                    base_iterator endIter = subRangeLast_;
                    ++endIter;
                    return make_subrange(subRangeBegin_, endIter);
                }
                friend bool operator==(iterator const& i, iterator const& i2)
                {
                    return i.subRangeBegin_ == i2.subRangeBegin_;
                }
                friend bool operator==(iterator const& i, sentinel const& s)
                {
                    return i.subRangeLast_ == s.sent;
                }
                friend bool operator==(sentinel const& s, iterator const& i)
                {
                    return i.subRangeLast_ == s.sent;
                }
            };

            slide_view(R input_view, intptr_t const count_)
                : input_view(RAH2_STD::move(input_view))
                , count_(count_)
            {
            }

            auto begin()
            {
                auto const range_end = RAH2_NS::ranges::end(input_view);
                auto const sub_range_begin = RAH2_NS::ranges::begin(input_view);
                auto sub_range_last = sub_range_begin;
                if (count_ == 0)
                {
                    return iterator(sub_range_last, sub_range_last);
                }
                auto const left = RAH2_NS::ranges::advance(sub_range_last, count_ - 1, range_end);
                if (left != 0)
                {
                    return iterator(sub_range_last, sub_range_last);
                }
                return iterator(sub_range_begin, sub_range_last);
            }

            template <bool IsCommon = is_common, RAH2_STD::enable_if_t<IsCommon>* = nullptr>
            auto end()
            {
                auto const range_end = RAH2_NS::ranges::end(input_view);
                if (count_ == 0)
                {
                    return iterator(range_end, range_end);
                }
                auto const sub_range_begin = RAH2_NS::ranges::begin(input_view);
                auto sub_range_first = range_end;
                auto const left = RAH2_NS::ranges::advance(sub_range_first, -count_, sub_range_begin);
                if (left != 0)
                {
                    return iterator(range_end, range_end);
                }
                ++sub_range_first;
                return iterator(sub_range_first, range_end);
            }

            template <bool IsCommon = is_common, RAH2_STD::enable_if_t<not IsCommon>* = nullptr>
            auto end()
            {
                return sentinel{RAH2_NS::ranges::end(input_view)};
            }

            template <bool Sized = RAH2_NS::ranges::sized_range<R>, RAH2_STD::enable_if_t<Sized>* = nullptr>
            auto size()
            {
                auto const sz = RAH2_NS::ranges::size(input_view) - count_ + 1;
                return RAH2_NS::ranges::range_size_t<R>(sz < 0 ? 0 : sz);
            }
            template <bool Sized = RAH2_NS::ranges::sized_range<R const>, RAH2_STD::enable_if_t<Sized>* = nullptr>
            auto size() const
            {
                auto const sz = RAH2_NS::ranges::size(input_view) - count_ + 1;
                return RAH2_NS::ranges::range_size_t<R const>(sz < 0 ? 0 : sz);
            }
        };
        namespace views
        {
            struct slide_raco
            {
                template <typename R>
                auto operator()(R&& range, range_difference_t<R> n) const
                {
                    auto range_view = all(range);
                    return slide_view<decltype(range_view)>(RAH2_STD::move(range_view), n);
                }

                inline auto operator()(size_t n) const
                {
                    return make_pipeable(
                        [this, n](auto&& range)
                        {
                            return (*this)(
                                RAH2_STD::forward<decltype(range)>(range),
                                static_cast<range_difference_t<decltype(range)>>(n));
                        });
                }
            };
            constexpr slide_raco slide;
        } // namespace views
        // ************************************ chunk *********************************************

        template <typename R>
        class chunk_view : public view_interface<chunk_view<R>>
        {
            R base_;
            using base_diff_type = range_difference_t<R>;
            base_diff_type step_;
            using inner_iterator = iterator_t<R>;
            using inner_sentinel = sentinel_t<R>;
            using base_cat =
                common_iterator_tag<range_iter_categ_t<R>, RAH2_STD::random_access_iterator_tag>;
            using reference_t = decltype(views::take(
                RAH2_NS::ranges::make_subrange(
                    RAH2_STD::declval<inner_iterator&>(), RAH2_STD::declval<inner_sentinel&>()),
                base_diff_type()));

        public:
            struct sentinel
            {
                sentinel_t<R> sent;
            };

            class iterator : public iterator_facade<iterator, sentinel, reference_t, base_cat>
            {
                iterator_t<R> current_{};
                sentinel_t<R> end_{};
                base_diff_type n_{};
                base_diff_type missing_{};

            public:
                iterator() = default;
                iterator(iterator_t<R> const& iter, sentinel_t<R> const& end, base_diff_type step = 0)
                    : current_(iter)
                    , end_(end)
                    , n_(step)
                {
                }

                iterator& operator++()
                {
                    missing_ = ranges::advance(current_, n_, end_);
                    return *this;
                }
                RAH2_POST_INCR(base_cat)
                template <
                    typename C = base_cat,
                    RAH2_STD::enable_if_t<derived_from<C, bidirectional_iterator_tag>>* = nullptr>
                iterator& operator--()
                {
                    ranges::advance(current_, missing_ - n_);
                    missing_ = 0;
                    return *this;
                }
                template <
                    typename C = base_cat,
                    RAH2_STD::enable_if_t<derived_from<C, bidirectional_iterator_tag>>* = nullptr>
                RAH2_POST_DECR;

                reference_t operator*() const
                {
                    return views::take(RAH2_NS::ranges::make_subrange(current_, end_), n_);
                }

                template <
                    typename C = base_cat,
                    RAH2_STD::enable_if_t<derived_from<C, random_access_iterator_tag>>* = nullptr>
                iterator& operator+=(range_difference_t<R> x)
                {
                    if (x > 0)
                    {
                        ranges::advance(current_, n_ * (x - 1));
                        missing_ = ranges::advance(current_, n_, end_);
                    }
                    else if (x < 0)
                    {
                        ranges::advance(current_, n_ * x + missing_);
                        missing_ = 0;
                    }
                    return *this;
                }

                template <
                    typename C = base_cat,
                    RAH2_STD::enable_if_t<derived_from<C, random_access_iterator_tag>>* = nullptr>
                auto operator-(iterator const& it) const
                {
                    return (current_ - it.current_ + missing_ - it.missing_) / n_;
                }

                template <
                    typename C = base_cat,
                    RAH2_STD::enable_if_t<derived_from<C, random_access_iterator_tag>>* = nullptr>
                auto operator<(iterator const& it) const
                {
                    return current_ < it.current_;
                }

                friend bool operator==(iterator const& it, iterator const& it2)
                {
                    return it.current_ == it2.current_;
                }
                friend bool operator==(iterator const& it, sentinel const& sent)
                {
                    return it.current_ == sent.sent;
                }
                friend bool operator==(sentinel const& sent, iterator const& it)
                {
                    return it.current_ == sent.sent;
                }
            };

            chunk_view(R base, base_diff_type const step)
                : base_(RAH2_STD::move(base))
                , step_(step)
            {
            }

            iterator begin()
            {
                auto iter = RAH2_NS::ranges::begin(base_);
                auto end_iter = RAH2_NS::ranges::end(base_);
                return iterator{iter, end_iter, step_};
            }

            template <typename U = R, RAH2_STD::enable_if_t<RAH2_NS::ranges::common_range<U>>* = nullptr>
            iterator end()
            {
                auto sent = RAH2_NS::ranges::end(base_);
                return iterator{sent, sent, step_};
            }

            template <
                typename U = R,
                RAH2_STD::enable_if_t<
                    !RAH2_NS::ranges::common_range<U>
                    and !RAH2_NS::sized_sentinel_for<inner_sentinel, inner_iterator>>* = nullptr>
            sentinel end()
            {
                return sentinel{RAH2_NS::ranges::end(base_)};
            }

            template <
                typename U = R,
                RAH2_STD::enable_if_t<
                    !RAH2_NS::ranges::common_range<U>
                    and RAH2_NS::sized_sentinel_for<inner_sentinel, inner_iterator>>* = nullptr>
            iterator end()
            {
                auto iter = RAH2_NS::ranges::begin(base_);
                auto sent = RAH2_NS::ranges::end(base_);
                iter = sent;
                return iterator{iter, iter, sent, step_};
            }

            template <bool Sized = sized_range<R>, RAH2_STD::enable_if_t<Sized>* = nullptr>
            auto size()
            {
                // TODO : To test
                return (RAH2_NS::ranges::size(base_) + (step_ - 1)) / step_;
            }

            template <bool Sized = sized_range<R const>, RAH2_STD::enable_if_t<Sized>* = nullptr>
            auto size() const
            {
                return (RAH2_NS::ranges::size(base_) + (step_ - 1)) / step_;
            }
        };
        namespace views
        {
            struct chunk_raco
            {
                template <typename R>
                auto operator()(R&& range, range_difference_t<R> step) const
                {
                    auto ref = all(RAH2_STD::forward<R>(range));
                    return chunk_view<decltype(ref)>(RAH2_STD::move(ref), step);
                }

                inline auto operator()(size_t step) const
                {
                    return make_pipeable(
                        [this, step](auto&& range)
                        {
                            return (*this)(
                                RAH2_STD::forward<decltype(range)>(range),
                                static_cast<range_difference_t<decltype(range)>>(step));
                        });
                }
            };
            constexpr chunk_raco chunk;
        } // namespace views
        // ***************************************** stride ***********************************************
        template <typename R>
        class stride_view : view_interface<stride_view<R>>
        {
            using base_iterator = iterator_t<R>;
            using base_sentinel = sentinel_t<R>;
            using iter_cat =
                cap_iterator_tag<range_iter_categ_t<R>, input_iterator_tag, random_access_iterator_tag>;
            using base_diff_type = range_difference_t<R>;
            R base_;
            base_diff_type stride_;

        public:
            // TODO : Make a version non-bidirectional without "missing_" for perf
            class iterator
                : public iterator_facade<iterator, default_sentinel_t, range_reference_t<R>, iter_cat>
            {
                base_iterator current_;
                base_sentinel end_;
                base_diff_type stride_ = 0;
                base_diff_type missing_ = 0;

            public:
                iterator() = default;
                iterator(base_iterator iter, base_sentinel end, base_diff_type stride, base_diff_type missing)
                    : current_(RAH2_STD::move(iter))
                    , end_(RAH2_STD::move(end))
                    , stride_(stride)
                    , missing_(missing)
                {
                }

                iterator& operator++()
                {
                    assert(missing_ == 0);
                    missing_ = RAH2_NS::ranges::advance(current_, stride_, end_);
                    return *this;
                }
                RAH2_POST_INCR(iter_cat)
                template <
                    typename C = iter_cat,
                    RAH2_STD::enable_if_t<derived_from<C, bidirectional_iterator_tag>>* = nullptr>
                iterator& operator--()
                {
                    RAH2_NS::ranges::advance(current_, missing_ - stride_);
                    missing_ = 0;
                    return *this;
                }
                template <
                    typename C = iter_cat,
                    RAH2_STD::enable_if_t<derived_from<C, bidirectional_iterator_tag>>* = nullptr>
                RAH2_POST_DECR;
                template <
                    typename C = iter_cat,
                    RAH2_STD::enable_if_t<derived_from<C, random_access_iterator_tag>>* = nullptr>
                iterator& operator+=(intptr_t value)
                {
                    current_ += stride_ * value;
                    return *this;
                }
                auto operator*() const -> decltype(*current_)
                {
                    return *current_;
                }
                auto operator*() -> decltype(*current_)
                {
                    return *current_;
                }
                bool operator==(iterator const& other) const
                {
                    return current_ == other.current_;
                }
                bool operator==(default_sentinel_t const&) const
                {
                    return current_ == end_;
                }

                template <
                    typename C = iter_cat,
                    RAH2_STD::enable_if_t<derived_from<C, random_access_iterator_tag>>* = nullptr>
                auto operator-(iterator const& other) const
                {
                    return (current_ - other.current_) / stride_;
                }
                template <
                    typename C = iter_cat,
                    RAH2_STD::enable_if_t<derived_from<C, random_access_iterator_tag>>* = nullptr>
                auto operator<(iterator const& other) const
                {
                    return current_ < other.current_;
                }
            };

            stride_view(R base, base_diff_type step)
                : base_(RAH2_STD::move(base))
                , stride_(step)
            {
            }

            auto begin()
            {
                return iterator(
                    RAH2_NS::ranges::begin(base_), RAH2_NS::ranges::end(base_), stride_, 0);
            }

            template <
                typename Base = R,
                RAH2_STD::enable_if_t<sized_range<Base> && common_range<Base> && forward_range<Base>>* = nullptr>
            auto end()
            {
                auto missing = (stride_ - RAH2_NS::ranges::distance(base_) % stride_) % stride_;
                return iterator(
                    RAH2_NS::ranges::end(base_), RAH2_NS::ranges::end(base_), stride_, missing);
            }
            template <
                typename Base = R,
                RAH2_STD::enable_if_t<
                    !sized_range<Base> && common_range<Base> && !bidirectional_range<Base>>* = nullptr>
            auto end()
            {
                return iterator(RAH2_NS::ranges::end(base_), RAH2_NS::ranges::end(base_), stride_, 0);
            }
            template <
                typename Base = R,
                RAH2_STD::enable_if_t<
                    !common_range<Base> || (!sized_range<Base> && bidirectional_range<Base>)>* = nullptr>
            auto end()
            {
                return default_sentinel_t();
            }
            template <typename Base = R, RAH2_STD::enable_if_t<sized_range<Base>>* = nullptr>
            auto size()
            {
                return range_size_t<R>((RAH2_NS::ranges::size(base_) + (stride_ - 1)) / stride_);
            }
        };
        template <class V>
        constexpr bool enable_borrowed_range<stride_view<V>> = enable_borrowed_range<V>;
        namespace views
        {
            struct stride_raco
            {
                template <typename R>
                auto operator()(R&& range, range_difference_t<R> step) const
                {
                    auto views = all(RAH2_STD::forward<R>(range));
                    return stride_view<decltype(views)>(RAH2_STD::move(views), step);
                }

                inline auto operator()(size_t step) const
                {
                    return make_pipeable(
                        [this, step](auto&& range)
                        {
                            return (*this)(
                                RAH2_STD::forward<decltype(range)>(range),
                                static_cast<range_difference_t<decltype(range)>>(step));
                        });
                }
            };
            constexpr stride_raco stride;
        } // namespace views

        // ******************************************* views extra ********************************

        // ******************************************* unbounded **********************************

        template <typename I>
        class unbounded_view : public view_interface<unbounded_view<I>>
        {
            I iter_;
            using base_cat = typename RAH2_STD::iterator_traits<I>::iterator_category;

        public:
            explicit unbounded_view(I iter)
                : iter_(RAH2_STD::move(iter))
            {
            }
            class iterator
                : public iterator_facade<iterator, default_sentinel_t, iter_reference_t<I>, base_cat>
            {
                I iter_;

            public:
                iterator() = default;
                explicit iterator(I iter)
                    : iter_(RAH2_STD::move(iter))
                {
                }

                iterator& operator++()
                {
                    ++iter_;
                    return *this;
                }
                RAH2_POST_INCR(base_cat)
                iterator& operator+=(intptr_t off)
                {
                    iter_ += off;
                    return *this;
                }
                template <
                    typename C = base_cat,
                    RAH2_STD::enable_if_t<RAH2_NS::derived_from<C, bidirectional_iterator_tag>>* = nullptr>
                iterator& operator--()
                {
                    --iter_;
                    return *this;
                }
                template <
                    typename C = base_cat,
                    RAH2_STD::enable_if_t<RAH2_NS::derived_from<C, bidirectional_iterator_tag>>* = nullptr>
                RAH2_POST_DECR;
                friend intptr_t operator-(iterator const& it1, iterator const& it2)
                {
                    return it1.iter_ - it2.iter_;
                }

                iter_reference_t<I> operator*()
                {
                    return *iter_;
                }
                template <
                    typename C = base_cat,
                    RAH2_STD::enable_if_t<RAH2_NS::derived_from<C, RAH2_STD::random_access_iterator_tag>>* = nullptr>
                bool operator<(iterator const it2) const
                {
                    return iter_ < it2.iter_;
                }

                friend bool operator==(iterator const& it1, iterator const& it2)
                {
                    return it1.iter_ == it2.iter_;
                }
                friend bool operator==(iterator const&, default_sentinel_t const&)
                {
                    return false;
                }
                friend bool operator==(default_sentinel_t const&, iterator const&)
                {
                    return false;
                }
            };

            auto begin()
            {
                return iterator{iter_};
            }
            auto end()
            {
                return default_sentinel_t();
            }
            static bool empty()
            {
                return false;
            }

            template <typename I2 = I, RAH2_STD::enable_if_t<RAH2_NS::contiguous_iterator<I2>>* = nullptr>
            auto data()
            {
                return &(*iter_);
            }
        };

        template <class I>
        constexpr bool enable_borrowed_range<unbounded_view<I>> = true;

        namespace views
        {
            template <typename I>
            auto unbounded(I&& it)
            {
                return unbounded_view<I>(RAH2_STD::forward<I>(it));
            }
        } // namespace views
        // ********************************** irange **********************************************
        /// @see RAH2_NS::irange
        template <typename T, bool Infinite>
        class irange_view : public view_interface<irange_view<T, Infinite>>
        {
            T start_;
            T stop_;
            T step_;
            using base_cat = RAH2_STD::random_access_iterator_tag;

        public:
            class iterator : public iterator_facade<iterator, sentinel_iterator, T, base_cat>
            {
                T val_ = T();
                T step_ = T(1);

            public:
                iterator() = default;
                iterator(T val, T step)
                    : val_(val)
                    , step_(step)
                {
                }

                iterator& operator++()
                {
                    val_ += step_;
                    return *this;
                }
                RAH2_POST_INCR(base_cat)
                iterator& operator+=(intptr_t value)
                {
                    val_ += T(step_ * value);
                    return *this;
                }
                iterator& operator--()
                {
                    val_ -= step_;
                    return *this;
                }
                RAH2_POST_DECR
                auto operator-(iterator const& other) const
                {
                    return (val_ - other.val_) / step_;
                }
                auto operator*() const
                {
                    return val_;
                }
                friend constexpr bool operator==(iterator const& it1, iterator const& it2)
                {
                    return it1.val_ == it2.val_;
                }
                friend constexpr bool operator<(iterator const& it1, iterator const& it2)
                {
                    return it1.val_ < it2.val_;
                }
                friend constexpr bool operator==(default_sentinel_t const&, iterator const&)
                {
                    return false;
                }
                friend constexpr bool operator==(iterator const&, default_sentinel_t const&)
                {
                    return false;
                }
            };

            irange_view(T start, T stop, T step)
                : start_(start)
                , stop_(stop)
                , step_(step)
            {
            }

            auto begin()
            {
                return iterator(start_, step_);
            }
            template <bool I = Infinite, RAH2_STD::enable_if_t<I>* = nullptr>
            auto end()
            {
                return default_sentinel_t{};
            }
            template <bool Inf = Infinite, RAH2_STD::enable_if_t<not Inf>* = nullptr>
            auto end()
            {
                auto const last_index = (stop_ - start_);
                auto const rounded_last = ((last_index + (step_ - 1)) / step_) * step_;
                return iterator(start_ + rounded_last, step_);
            }
        };
        namespace views
        {
            template <typename T = size_t>
            auto irange(T start, T stop, T step)
            {
                return irange_view<T, false>(start, stop, step);
            }

            template <typename T = size_t>
            auto irange(T start, T step)
            {
                return irange_view<T, true>(start, start, step);
            }
        } // namespace views

        // ********************************** cycle ***********************************************

        template <typename R>
        class cycle_view : public view_interface<cycle_view<R>>
        {
            R base_;
            using base_iterator = iterator_t<R>;
            using base_sentinel = sentinel_t<R>;
            using max_tag = RAH2_STD::conditional_t<
                assignable_from<RAH2_STD::add_lvalue_reference_t<iterator_t<R>>, sentinel_t<R>>,
                RAH2_NS::bidirectional_iterator_tag,
                RAH2_NS::forward_iterator_tag>;
            using base_cat =
                cap_iterator_tag<range_iter_categ_t<R>, RAH2_NS::input_iterator_tag, max_tag>;

            static_assert(
                RAH2_NS::ranges::forward_range<R>, "cycle_view require at least a forward_range");

        public:
            class iterator : public iterator_facade<
                                 iterator,
                                 default_sentinel_t,
                                 RAH2_NS::ranges::range_reference_t<R>,
                                 common_iterator_tag<RAH2_STD::bidirectional_iterator_tag, base_cat>>
            {
                cycle_view* view_;
                base_iterator begin_iter_;
                base_sentinel end_iter_;
                base_iterator iter_;

            public:
                iterator() = default;
                explicit iterator(cycle_view* v)
                    : view_(v)
                    , begin_iter_(RAH2_NS::ranges::begin(v->base_))
                    , end_iter_(RAH2_NS::ranges::end(v->base_))
                    , iter_(begin_iter_)
                {
                }

                iterator& operator++()
                {
                    assert(view_ != nullptr);
                    ++iter_;
                    while (iter_ == end_iter_)
                    {
                        iter_ = RAH2_NS::ranges::begin(view_->base_);
                    }
                    return *this;
                }
                RAH2_POST_INCR(base_cat)
                template <
                    typename U = R,
                    RAH2_STD::enable_if_t<
                        bidirectional_range<U>
                        && assignable_from<RAH2_STD::add_lvalue_reference_t<iterator_t<U>>, sentinel_t<U>>>* = nullptr>
                iterator& operator--()
                {
                    assert(view_ != nullptr);
                    while (iter_ == begin_iter_)
                    {
                        iter_ = RAH2_NS::ranges::end(view_->base_);
                    }
                    --iter_;
                    return *this;
                }
                template <
                    typename U = R,
                    RAH2_STD::enable_if_t<
                        bidirectional_range<U>
                        && assignable_from<RAH2_STD::add_lvalue_reference_t<iterator_t<U>>, sentinel_t<U>>>* =
                        nullptr>
                RAH2_POST_DECR;
                RAH2_NS::ranges::range_reference_t<R> operator*()
                {
                    assert(view_ != nullptr);
                    return *iter_;
                }
                friend bool operator==(iterator const& it, iterator const& it2)
                {
                    return it.iter_ == it2.iter_;
                }
                friend bool operator==(iterator const&, default_sentinel_t const&)
                {
                    return false;
                }
                friend bool operator==(default_sentinel_t const&, iterator const&)
                {
                    return false;
                }
            };

            cycle_view() = default;
            explicit cycle_view(R base)
                : base_(RAH2_STD::move(base))
            {
            }

            auto base() const
            {
                return base_;
            }

            iterator begin()
            {
                return iterator(this);
            }

            default_sentinel_t end()
            {
                return {};
            }

            bool empty()
            {
                return RAH2_NS::ranges::empty(base_);
            }
        };
        namespace views
        {
            struct cycle_raco : closure_object_facade<cycle_raco>
            {
                template <typename R>
                auto operator()(R&& range) const
                {
                    auto ref = all(RAH2_STD::forward<R>(range));
                    return cycle_view<decltype(ref)>(RAH2_STD::move(ref));
                }
            };
            constexpr cycle_raco cycle;

        } // namespace views

        // ********************************** generate ****************************************************
        template <typename F>
        class generate_view : public view_interface<generate_view<F>>
        {
            using value = RAH2_NS::remove_cvref_t<decltype(RAH2_STD::declval<F>()())>;
            F func_;
            using base_cat = RAH2_STD::input_iterator_tag;

        public:
            class iterator : public iterator_facade<iterator, default_sentinel_t, value, base_cat>
            {
                generate_view* parent_ = nullptr;
                RAH2_NS::details::optional<value> value_;

            public:
                iterator() = default;
                explicit iterator(generate_view* parent)
                    : parent_(parent)
                    , value_(parent_->func_())
                {
                }
                iterator& operator++()
                {
                    value_ = parent_->func_();
                    return *this;
                }
                RAH2_POST_INCR(base_cat)
                value operator*() const
                {
                    return *value_;
                }
                friend bool operator==(iterator const&, default_sentinel_t const&)
                {
                    return false;
                }
                friend bool operator==(default_sentinel_t const&, iterator const&)
                {
                    return false;
                }
            };

            explicit generate_view(F func)
                : func_(RAH2_STD::move(func))
            {
            }

            auto begin()
            {
                return iterator(this);
            }
            default_sentinel_t end()
            {
                return {};
            }
        };
        namespace views
        {
            template <typename F>
            auto generate(F&& func)
            {
                using functor = RAH2_STD::remove_cv_t<RAH2_STD::remove_reference_t<F>>;
                return generate_view<functor>(RAH2_STD::forward<F>(func));
            }

            template <typename F>
            auto generate_n(size_t const count, F&& func)
            {
                return generate(RAH2_STD::forward<F>(func)) | take(count);
            }
        } // namespace views

        // ******************************************* set_difference *************************************

        template <typename InputRng1, typename InputRng2>
        class set_difference_view : public view_interface<set_difference_view<InputRng1, InputRng2>>
        {
            using reference = range_reference_t<InputRng1>;
            using inner_iterator1 = iterator_t<InputRng1>;
            using inner_sentinel1 = sentinel_t<InputRng1>;
            using inner_iterator2 = iterator_t<InputRng2>;
            using inner_sentinel2 = sentinel_t<InputRng2>;
            InputRng1 base1_;
            InputRng2 base2_;
            using base_cat = common_iterator_tag<
                common_iterator_tag<range_iter_categ_t<InputRng1>, range_iter_categ_t<InputRng2>>,
                RAH2_STD::forward_iterator_tag>;

        public:
            class iterator : public iterator_facade<iterator, default_sentinel_t, reference, base_cat>
            {
                inner_iterator1 first1_{};
                inner_sentinel1 last1_{};
                inner_iterator2 first2_{};
                inner_sentinel2 last2_{};

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

            public:
                iterator() = default;
                iterator(
                    inner_iterator1 first1,
                    inner_sentinel1 last1,
                    inner_iterator2 first2,
                    inner_sentinel2 last2)
                    : first1_(RAH2_STD::move(first1))
                    , last1_(RAH2_STD::move(last1))
                    , first2_(RAH2_STD::move(first2))
                    , last2_(RAH2_STD::move(last2))
                {
                    next_value();
                }

                iterator& operator++()
                {
                    ++first1_;
                    next_value();
                    return *this;
                }
                RAH2_POST_INCR(base_cat)
                auto operator*() const -> decltype(*first1_)
                {
                    return *first1_;
                }
                template <
                    typename C = base_cat,
                    RAH2_STD::enable_if_t<derived_from<C, forward_iterator_tag>>* = nullptr>
                bool operator==(iterator const& it2) const
                {
                    return first1_ == it2.first1_;
                }
                friend bool operator==(iterator const& it, default_sentinel_t)
                {
                    return it.first1_ == it.last1_;
                }
            };
            set_difference_view(InputRng1 base1, InputRng2 base2)
                : base1_(RAH2_STD::move(base1))
                , base2_(RAH2_STD::move(base2))
            {
            }
            auto begin()
            {
                return iterator(
                    RAH2_NS::ranges::begin(base1_),
                    RAH2_NS::ranges::end(base1_),
                    RAH2_NS::ranges::begin(base2_),
                    RAH2_NS::ranges::end(base2_));
            }
            default_sentinel_t end()
            {
                return {};
            }
        };
        namespace views
        {
            struct set_difference_raco : closure_object_facade<set_difference_raco>
            {
                template <typename R1, typename R2>
                auto operator()(R1&& range1, R2&& range2) const
                {
                    auto ref1 = all(RAH2_STD::forward<R1>(range1));
                    auto ref2 = all(RAH2_STD::forward<R2>(range2));
                    return set_difference_view<decltype(ref1), decltype(ref2)>(
                        RAH2_STD::move(ref1), RAH2_STD::move(ref2));
                }

                template <typename R2>
                auto operator()(R2&& range2) const
                {
                    return make_pipeable(
                        [this, r2 = all(range2)](auto&& range) {
                            return (*this)(
                                RAH2_STD::forward<decltype(range)>(range), RAH2_STD::move(r2));
                        });
                }
            };
            constexpr set_difference_raco set_difference;
        } // namespace views

        // ********************************** for_each ****************************************************
        namespace views
        {
            struct for_each_raco
            {
                template <typename R, typename F>
                auto operator()(R&& range, F&& func) const
                {
                    return range | transform(func) | join;
                }

                template <typename F>
                auto operator()(F&& func) const
                {
                    return make_pipeable(
                        [this, func = RAH2_STD::forward<F>(func)](auto&& range) {
                            return (*this)(
                                RAH2_STD::forward<decltype(range)>(range), RAH2_STD::move(func));
                        });
                }
            };
            constexpr for_each_raco for_each;
        } // namespace views

        // ***************************************** slice ************************************************

        template <typename R>
        class slice_view : public view_interface<slice_view<R>>
        {
            R base_;
            intptr_t begin_idx_ = 0;
            intptr_t end_idx_ = 0;
            using iterator = RAH2_NS::ranges::iterator_t<R>;
            static constexpr bool base_is_sized_random_access =
                RAH2_NS::ranges::random_access_range<R> && RAH2_NS::ranges::sized_range<R>;
            static constexpr bool is_sized = RAH2_NS::ranges::sized_range<R>;

        public:
            slice_view() = default;
            slice_view(R v, intptr_t const begin_idx, intptr_t const end_idx)
                : base_(RAH2_STD::move(v))
                , begin_idx_(begin_idx)
                , end_idx_(end_idx)
            {
            }

            template <bool IsSized = base_is_sized_random_access, RAH2_STD::enable_if_t<IsSized>* = nullptr>
            auto begin()
            {
                auto iter = RAH2_NS::ranges::begin(base_);
                iter +=
                    RAH2_NS::details::min(begin_idx_, std::ptrdiff_t(RAH2_NS::ranges::size(base_)));
                return iter;
            }

            template <bool IsSized = base_is_sized_random_access, RAH2_STD::enable_if_t<!IsSized>* = nullptr>
            auto begin()
            {
                auto iter = RAH2_NS::ranges::begin(base_);
                RAH2_NS::ranges::advance(iter, begin_idx_, RAH2_NS::ranges::end(base_));
                return counted_iterator<iterator_t<R>>(iter, end_idx_ - begin_idx_);
            }

            template <bool IsSized = base_is_sized_random_access, RAH2_STD::enable_if_t<IsSized>* = nullptr>
            auto end()
            {
                auto iter = RAH2_NS::ranges::begin(base_);
                iter += RAH2_NS::details::min(end_idx_, RAH2_NS::ranges::ssize(base_));
                return iter;
            }

            template <bool IsSized = base_is_sized_random_access, RAH2_STD::enable_if_t<!IsSized>* = nullptr>
            default_sentinel_t end()
            {
                return default_sentinel_t{};
            }

            template <bool IsSized = RAH2_NS::ranges::sized_range<R const>, RAH2_STD::enable_if_t<IsSized>* = nullptr>
            auto size() const
            {
                auto const base_size = RAH2_NS::ranges::ssize(base_);
                return range_size_t<R>(
                    RAH2_NS::details::min(end_idx_, base_size)
                    - RAH2_NS::details::min(begin_idx_, base_size));
            }
            template <bool IsSized = RAH2_NS::ranges::sized_range<R>, RAH2_STD::enable_if_t<IsSized>* = nullptr>
            auto size()
            {
                auto const base_size = RAH2_NS::ranges::ssize(base_);
                return range_size_t<R>(
                    RAH2_NS::details::min(end_idx_, base_size)
                    - RAH2_NS::details::min(begin_idx_, base_size));
            }
            template <bool C = RAH2_NS::ranges::contiguous_range<R>, RAH2_STD::enable_if_t<C>* = nullptr>
            auto data()
            {
                return &(*begin());
            }
            template <bool C = RAH2_NS::ranges::contiguous_range<R const>, RAH2_STD::enable_if_t<C>* = nullptr>
            auto data() const
            {
                return &(*begin());
            }
        };
        namespace views
        {
            struct slice_raco
            {
                template <typename R>
                auto operator()(
                    R&& range, range_difference_t<R> begin_idx, range_difference_t<R> end_idx) const
                {
                    auto ref = all(RAH2_STD::forward<R>(range));
                    return slice_view<decltype(ref)>{RAH2_STD::move(ref), begin_idx, end_idx};
                }

                inline auto operator()(size_t beg, size_t sent) const
                {
                    return make_pipeable(
                        [this, beg, sent](auto&& range)
                        {
                            return (*this)(
                                RAH2_STD::forward<decltype(range)>(range),
                                static_cast<range_difference_t<decltype(range)>>(beg),
                                static_cast<range_difference_t<decltype(range)>>(sent));
                        });
                }
            };
            constexpr slice_raco slice;
        } // namespace views

        // ***************************************** concat ***********************************************

        template <typename R1, typename R2>
        class concat_view : public view_interface<concat_view<R1, R2>>
        {
            R1 base1_;
            R2 base2_;
            using r1_iterator = iterator_t<R1>;
            using r2_iterator = iterator_t<R2>;
            using r1_sentinel = sentinel_t<R1>;
            using r2_sentinel = sentinel_t<R2>;
            using base_cat = common_iterator_tag<
                common_iterator_tag<range_iter_categ_t<R1>, range_iter_categ_t<R2>>,
                RAH2_STD::forward_iterator_tag>;

        public:
            static_assert(
                RAH2_NS::is_same_v<range_reference_t<R1>, range_reference_t<R2>>,
                "R1 and R2 doesn't have the same reference type");
            using reference = range_reference_t<R1>;

            class iterator : public iterator_facade<iterator, default_sentinel_t, reference, base_cat>
            {
                r1_iterator iter1_{};
                r1_sentinel sent1_{};
                r2_iterator iter2_{};
                r2_sentinel sent2_{};
                size_t range_index_{};

            public:
                iterator() = default;
                iterator(r1_iterator iter1, r1_sentinel sent1, r2_iterator iter2, r2_sentinel sent2)
                    : iter1_(RAH2_STD::move(iter1))
                    , sent1_(RAH2_STD::move(sent1))
                    , iter2_(RAH2_STD::move(iter2))
                    , sent2_(RAH2_STD::move(sent2))
                    , range_index_(0)
                {
                    if (iter1 == sent1)
                        range_index_ = 1;
                }

                iterator& operator++()
                {
                    if (range_index_ == 0)
                    {
                        auto& i = iter1_;
                        ++i;
                        if (i == sent1_)
                            range_index_ = 1;
                    }
                    else
                        ++iter2_;
                    return *this;
                }
                RAH2_POST_INCR(base_cat)
                auto operator*() const -> decltype(*iter1_)
                {
                    if (range_index_ == 0)
                        return *iter1_;
                    else
                        return *iter2_;
                }

                friend bool operator==(iterator const& it1, iterator const& it2)
                {
                    if (it1.range_index_ != it2.range_index_)
                        return false;
                    if (it1.range_index_ == 0)
                        return it1.iter1_ == it2.iter1_;
                    else
                        return it1.iter2_ == it2.iter2_;
                }
                friend bool operator==(iterator const& it, default_sentinel_t const&)
                {
                    if (it.range_index_ == 0)
                        return it.iter1_ == it.sent1_;
                    else
                        return it.iter2_ == it.sent2_;
                }
                friend bool operator==(default_sentinel_t const&, iterator const& it)
                {
                    if (it.range_index_ == 0)
                        return it.iter1_ == it.sent1_;
                    else
                        return it.iter2_ == it.sent2_;
                }
            };

            concat_view(R1 base1, R2 base2)
                : base1_(RAH2_STD::move(base1))
                , base2_(RAH2_STD::move(base2))
            {
            }

            auto begin()
            {
                return iterator(
                    RAH2_NS::ranges::begin(base1_),
                    RAH2_NS::ranges::end(base1_),
                    RAH2_NS::ranges::begin(base2_),
                    RAH2_NS::ranges::end(base2_));
            }

            default_sentinel_t end()
            {
                return {};
            }

            template <bool all_sized = sized_range<R1>&& sized_range<R2>, RAH2_STD::enable_if_t<all_sized>* = nullptr>
            auto size()
            {
                return RAH2_NS::ranges::size(base1_) + RAH2_NS::ranges::size(base2_);
            }
            template <
                bool all_sized = sized_range<R1 const>&& sized_range<R2 const>,
                RAH2_STD::enable_if_t<all_sized>* = nullptr>
            auto size() const
            {
                return RAH2_NS::ranges::size(base1_) + RAH2_NS::ranges::size(base2_);
            }
        };
        namespace views
        {
            /// @brief return the same range
            template <typename R1>
            auto concat(R1&& range1)
            {
                return RAH2_STD::forward<R1>(range1);
            }

            template <typename R1, typename R2>
            auto concat(R1&& range1, R2&& range2)
            {
                auto r1 = all(RAH2_STD::forward<R1>(range1));
                auto r2 = all(RAH2_STD::forward<R2>(range2));
                return concat_view<decltype(r1), decltype(r2)>(
                    RAH2_STD::move(r1), RAH2_STD::move(r2));
            }

            /// @see RAH2_NS::views::concat(R1&& range1, R2&& range2)
            template <typename R1, typename R2, typename... Ranges>
            auto concat(R1&& range1, R2&& range2, Ranges&&... ranges)
            {
                return concat(
                    concat(RAH2_STD::forward<R1>(range1), RAH2_STD::forward<R2>(range2)), ranges...);
            }
        } // namespace views
    } // namespace ranges

    namespace views = ranges::views;
} // namespace RAH2_NS
