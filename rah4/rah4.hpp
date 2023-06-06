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
#include "mpark/variant.hpp"

namespace RAH_NAMESPACE
{

    // **************************** Range conversions *********************************************

    /// @brief Return a container of type C, filled with the content of range
    ///
    /// @snippet test.cpp rah::to
    template <
        typename C,
        typename R,
        decltype(std::declval<C>().push_back(std::declval<RAH_NAMESPACE::range_reference_t<R>>()))* = nullptr>
    auto to(R&& range)
    {
        C container;
        for (auto&& elt : range)
        {
            container.push_back(std::forward<decltype(elt)>(elt));
        }
        return container;
    }

    template <
        typename C,
        typename R,
        decltype(std::declval<C>().insert(std::declval<RAH_NAMESPACE::range_reference_t<R>>()))* = nullptr>
    auto to(R&& range)
    {
        C container;
        for (auto&& elt : range)
        {
            container.insert(std::forward<decltype(elt)>(elt));
        }
        return container;
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

#if !RAH_CPP20
        template <
            typename Sent = S,
            std::enable_if_t<!std::is_same_v<Sent, void> and !std::is_same_v<Sent, I>>* = nullptr>
        friend bool operator!=(Sent const& sent, I const& it)
        {
            return !(it == sent);
        }
        template <
            typename Sent = S,
            std::enable_if_t<!std::is_same_v<Sent, void> and !std::is_same_v<Sent, I>>* = nullptr>
        friend bool operator!=(I const& it, Sent const& sent)
        {
            return !(it == sent);
        }
        friend bool operator!=(I const& it1, I const& it2)
        {
            return !(it1 == it2);
        }
#endif

        //bool operator!=(S const& rho) const
        //{
        //    return !(RAH_SELF_CONST == rho);
        //}
        auto operator->()
        {
            return RAH_NAMESPACE::details::pointer_type<reference>::to_pointer(*RAH_SELF);
        }

        auto operator++(int)
        {
            auto it = RAH_SELF;
            ++(*this);
            return it;
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

    template <typename I>
    struct counted_iterator : iterator_facade<
                                  counted_iterator<I>,
                                  default_sentinel,
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
            --count_;
            return *this;
        }
        counted_iterator& operator+=(intptr_t off)
        {
            iter_ += off;
            count_ -= off;
            return this;
        }
        counted_iterator& operator--()
        {
            --iter_;
            ++count_;
            return *this;
        }
        auto operator-(counted_iterator const& r) const
        {
            return r.count_ - count_;
        }
        auto operator*() -> decltype(*iter_)
        {
            return *iter_;
        }
        friend bool operator==(counted_iterator const& it1, counted_iterator const& it2)
        {
            return it1.count_ == it2.count_;
        }
        friend bool operator==(counted_iterator const& it, default_sentinel const&)
        {
            return it.count_ == 0;
        }
        friend bool operator==(default_sentinel const&, counted_iterator const& it)
        {
            return it.count_ == 0;
        }
    };

    namespace views
    {
        // ********************************** empty ***********************************************
        template <typename T>
        class empty_view : public view_interface<empty_view<T>>
        {
        public:
            T* begin()
            {
                return nullptr;
            }
            T* end()
            {
                return nullptr;
            }
            T* data()
            {
                return nullptr;
            }
            static size_t size()
            {
                return 0;
            }
            bool empty()
            {
                return true;
            }
        };

        template <typename T>
        auto empty()
        {
            return empty_view<T>{};
        }

        // ********************************** single ******************************************************

        template <typename T>
        class single_view : public view_interface<single_view<T>>
        {
            T value_;

        public:
            single_view(T val)
                : value_(std::move(val))
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
                return 0;
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

        template <typename V>
        auto single(V&& value)
        {
            return single_view<RAH_NAMESPACE::remove_cvref_t<V>>{std::forward<V>(value)};
        }

        // ********************************** iota ************************************************

        /// @see rah::iota
        template <typename W>
        class iota_iterator
            : public iterator_facade<iota_iterator<W>, default_sentinel, W, RAH_STD::random_access_iterator_tag>
        {
            W val_ = {};

        public:
            iota_iterator()
            {
            }
            iota_iterator(W val)
                : val_(val)
            {
            }

            iota_iterator& operator++()
            {
                ++val_;
                return *this;
            }
            iota_iterator& operator+=(intptr_t value)
            {
                val_ += W(value);
                return *this;
            }
            iota_iterator operator+(intptr_t value)
            {
                iota_iterator copy = *this;
                return copy += value;
            }
            iota_iterator& operator-=(intptr_t value)
            {
                val_ -= W(value);
                return *this;
            }
            iota_iterator operator-(intptr_t value)
            {
                iota_iterator copy = *this;
                return copy -= value;
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
            friend bool operator==(iota_iterator const& it, iota_iterator const& it2)
            {
                return it.val_ == it2.val_;
            }
            friend bool operator==(iota_iterator const&, default_sentinel)
            {
                return false;
            }
            friend bool operator==(default_sentinel, iota_iterator const&)
            {
                return false;
            }
        };

        template <typename W = size_t>
        constexpr auto iota(W start, W stop)
        {

            return subrange<iota_iterator<W>>{start, stop};
        }

        template <typename W = size_t>
        constexpr auto iota(W start = 0)
        {
            return subrange<iota_iterator<W>, default_sentinel>{start, {}};
        }

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
            basic_istream_view(basic_istream_view const&) = default;

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

                iterator(const iterator&) = default;
                iterator(iterator&&) = default;
                iterator& operator=(const iterator&) = default;
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
                friend bool operator==(iterator const& it, sentinel const&)
                {
                    return !(*it.istream_->stream_);
                }
                friend bool operator==(sentinel const&, iterator const& it)
                {
                    return !(*it.istream_->stream_);
                }
            };

            auto begin()
            {
                *stream_ >> value_;
                return iterator(this);
            }

            auto end()
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

        // ********************************** repeat ******************************************************

        // TODO : Make the range to be a RAH_NAMESPACE::sentinel_range_base

        /// @see rah::repeat
        template <typename V>
        struct repeat_iterator
            : iterator_facade<repeat_iterator<V>, default_sentinel, V const&, RAH_STD::forward_iterator_tag>
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
            friend bool operator==(repeat_iterator, default_sentinel)
            {
                return false;
            }
            friend bool operator==(default_sentinel, repeat_iterator)
            {
                return false;
            }
        };

        template <typename V>
        auto repeat(V&& value)
        {
            return subrange<repeat_iterator<RAH_STD::remove_const_t<RAH_STD::remove_reference_t<V>>>, default_sentinel>{
                {value}, {}};
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

        // ***************************************** filter ***********************************************

        template <typename R, typename P>
        class filter_view : public view_interface<filter_view<R, P>>
        {
            R range_;
            P pred_;
            using inner_iterator = RAH_NAMESPACE::iterator_t<R>;
            using inner_sentinel = RAH_NAMESPACE::sentinel_t<R>;
            constexpr static bool is_common_range = RAH_NAMESPACE::common_range<R>;

        public:
            struct sentinel
            {
                inner_sentinel sent;
            };
            struct iterator : iterator_facade<
                                  iterator,
                                  sentinel,
                                  typename RAH_STD::iterator_traits<inner_iterator>::reference,
                                  RAH_STD::bidirectional_iterator_tag>
            {
                filter_view* view_;
                inner_iterator iter_;
                typename RAH_STD::iterator_traits<inner_iterator>::pointer value_pointer_;

                // Get a pointer to the pointed value,
                //   OR a pointer to a copy of the pointed value (when not a reference iterator)
                template <class It>
                struct get_pointer
                {
                    static auto get(It& iter)
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

                iterator(filter_view* v, inner_iterator iter)
                    : view_(v)
                    , iter_(RAH_STD::move(iter))
                {
                    next_value();
                }

                void next_value()
                {
                    while (iter_ != RAH_NAMESPACE::end(view_->range_)
                           && not(view_->pred_)(
                               *(value_pointer_ = get_pointer<inner_iterator>::get(iter_))))
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

                auto operator*() const -> decltype(*value_pointer_)
                {
                    return *value_pointer_;
                }
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

            template <typename U = R, std::enable_if_t<is_common_range, U>* = nullptr>
            auto end()
            {
                return iterator{this, RAH_NAMESPACE::end(range_)};
            }
            template <typename U = R, std::enable_if_t<not is_common_range, U>* = nullptr>
            auto end()
            {
                return sentinel{RAH_NAMESPACE::end(range_)};
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

        // ******************************************* transform ******************************************

        template <typename R, typename F>
        class transform_view : public view_interface<transform_view<R, F>>
        {
            R base_;
            RAH_NAMESPACE::details::optional<F> func_;
            constexpr static bool is_common_range = RAH_NAMESPACE::common_range<R>;

        public:
            struct sentinel
            {
                sentinel_t<R> sent;
            };

            using reference =
                decltype(details::declval<F>()(details::declval<range_reference_t<R>>()));
            struct iterator : iterator_facade<iterator, sentinel, reference, range_iter_categ_t<R>>
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
                auto operator*() -> decltype((*func_)(*iter_))
                {
                    return (*func_)(*iter_);
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
                : base_(std::move(base))
                , func_(std::move(func))
            {
            }

            auto begin()
            {
                return iterator(RAH_NAMESPACE::begin(base_), *func_);
            }

            template <typename U = R, std::enable_if_t<is_common_range, U>* = nullptr>
            auto end()
            {
                return iterator(RAH_NAMESPACE::end(base_), *func_);
            }

            template <typename U = R, std::enable_if_t<not is_common_range, U>* = nullptr>
            auto end()
            {
                return sentinel{RAH_NAMESPACE::end(base_)};
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

        // ******************************************* take ***********************************************

        template <typename R>
        class take_view : public view_interface<take_view<R>>
        {
            R inputView_;
            size_t count_;

        public:
            using base_iterator = RAH_NAMESPACE::iterator_t<R>;
            using base_sentinel = RAH_NAMESPACE::sentinel_t<R>;
            using reference = range_reference_t<R>;

            take_view(R inputView, size_t count)
                : inputView_(inputView)
                , count_(count)
            {
            }

            template <typename U = R, std::enable_if_t<RAH_NAMESPACE::random_access_range<U>>* = nullptr>
            auto begin()
            {
                return RAH_NAMESPACE::begin(inputView_);
            }

            template <typename U = R, std::enable_if_t<not RAH_NAMESPACE::random_access_range<U>>* = nullptr>
            auto begin()
            {
                return counted_iterator<iterator_t<U>>(RAH_NAMESPACE::begin(inputView_), count_);
            }

            template <typename U = R, std::enable_if_t<RAH_NAMESPACE::random_access_range<U>>* = nullptr>
            auto end()
            {
                auto iter = RAH_NAMESPACE::begin(inputView_);
                RAH_NAMESPACE::advance(iter, count_, RAH_NAMESPACE::end(inputView_));
                return iter;
            }

            template <typename U = R, std::enable_if_t<not RAH_NAMESPACE::random_access_range<U>>* = nullptr>
            default_sentinel end()
            {
                return default_sentinel{};
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

        // ************************************ split_view ****************************************

        template <typename R, typename P>
        class split_view : public view_interface<split_view<R, P>>
        {
            R base_;
            P pattern_;
            using inner_iterator = RAH_NAMESPACE::iterator_t<R>;
            using inner_sentinel = RAH_NAMESPACE::sentinel_t<R>;
            details::optional<subrange<inner_iterator, inner_sentinel>> cached_begin_;

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
                : iterator_facade<iterator, sentinel, subrange<inner_iterator, inner_iterator>, std::input_iterator_tag>
            {
                split_view* parent_;
                inner_iterator cur_;
                subrange<inner_iterator, inner_iterator> next_;
                bool trailing_empty_ = false;

                iterator(
                    split_view* parent,
                    inner_iterator cur,
                    subrange<inner_iterator, inner_iterator> next)
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

        template <typename I, std::enable_if_t<RAH_NAMESPACE::random_access_iterator<I>>* = nullptr>
        auto counted(I&& it, size_t n)
        {
            using iterator = RAH_STD::remove_reference_t<I>;
            return make_subrange(iterator(it), iterator(it + n));
        }

        template <typename I, std::enable_if_t<!RAH_NAMESPACE::random_access_iterator<I>>* = nullptr>
        auto counted(I&& it, size_t n)
        {
            using iterator = counted_iterator<RAH_STD::remove_reference_t<I>>;
            return make_subrange(iterator(it, n), default_sentinel{});
        }

        // ******************************************* common_view ********************************

        template <typename I, typename S>
        class common_iterator
            : public iterator_facade<common_iterator<I, S>, void, iter_reference_t<I>, std::forward_iterator_tag>
        {
            static_assert(
                RAH_NAMESPACE::input_or_output_iterator<I>,
                "RAH_NAMESPACE::input_or_output_iterator<I>");
            // static_assert(RAH_NAMESPACE::sentinel_for<S, I>, "RAH_NAMESPACE::sentinel_for<S, I>");
            static_assert(!RAH_NAMESPACE::same_as<I, S>, "!RAH_NAMESPACE::same_as<I, S>");
            static_assert(RAH_NAMESPACE::copyable<I>, "RAH_NAMESPACE::copyable<I>");
            mpark::variant<I, S> var;

        public:
            common_iterator(I it)
                : var(std::move(it))
            {
            }
            common_iterator(S sent)
                : var(std::move(sent))
            {
            }

            iter_reference_t<I> operator*()
            {
                return *mpark::get<I>(var);
            }

            common_iterator& operator++()
            {
                ++mpark::get<I>(var);
                return *this;
            }

            template <typename It = I, std::enable_if_t<RAH_NAMESPACE::equality_comparable<It>>* = nullptr>
            bool operator==(common_iterator const& it) const
            {
                if (mpark::holds_alternative<S>(it.var))
                {
                    if (mpark::holds_alternative<S>(var))
                        return true;
                    else
                        return mpark::get<I>(var) == mpark::get<S>(it.var);
                }
                else if (mpark::holds_alternative<S>(var))
                    return mpark::get<I>(it.var) == mpark::get<S>(var);
                else
                    return mpark::get<I>(var) == mpark::get<I>(it.var);
            }

            template <typename It = I, std::enable_if_t<not RAH_NAMESPACE::equality_comparable<It>>* = nullptr>
            bool operator==(common_iterator const& it) const
            {
                if (mpark::holds_alternative<S>(it.var))
                {
                    if (mpark::holds_alternative<S>(var))
                        return true;
                    else
                        return mpark::get<I>(var) == mpark::get<S>(it.var);
                }
                else if (mpark::holds_alternative<S>(var))
                    return mpark::get<S>(var) == mpark::get<I>(it.var);
                else
                {
                    assert(false && "Can't compare this type of iterator");
                    return false;
                }
            }

            auto operator-(common_iterator const& it)
            {
                if (mpark::holds_alternative<S>(it.var))
                {
                    if (mpark::holds_alternative<S>(var))
                        return 0;
                    else
                        return mpark::get<0>(var) - mpark::get<1>(it.var);
                }
                else if (mpark::holds_alternative<S>(var))
                    return mpark::get<1>(var) - mpark::get<0>(it.var);
                else
                    return mpark::get<0>(var) - mpark::get<0>(it.var);
            }
        };

        template <typename R>
        class common_view : public view_interface<common_view<R>>
        {
            R base_;
            using iterator = iterator_t<R>;
            using sentinel = sentinel_t<R>;

            static_assert(not common_range<R>, "expect not common_range<R>");
            static_assert(RAH_NAMESPACE::copyable<iterator_t<R>>, "expect copyable<iterator_t<R>>");

        public:
            common_view(R r)
                : base_(r)
            {
            }
            auto begin()
            {
                return common_iterator<iterator, sentinel>(RAH_NAMESPACE::begin(base_));
            }

            auto end()
            {
                return common_iterator<iterator, sentinel>(RAH_NAMESPACE::end(base_));
            }
        };

        /// @brief Adapts a given view with different types for iterator/sentinel pair into a view
        /// that is also a common_range. A common_view always has the same iterator/sentinel type.
        ///
        /// @snippet test.cpp rah::views::common
        template <typename R, std::enable_if_t<not common_range<R>>* = nullptr>
        auto common(R&& range)
        {
            auto ref = RAH_NAMESPACE::views::all(range);
            return common_view<decltype(ref)>(std::move(ref));
        }

        template <typename R, std::enable_if_t<common_range<R>>* = nullptr>
        auto common(R&& range)
        {
            return RAH_NAMESPACE::views::all(range);
        }

        /// @snippet test.cpp rah::views::common_pipeable
        inline auto common()
        {
            return make_pipeable(
                [](auto&& range)
                { return RAH_NAMESPACE::views::common(RAH_STD::forward<decltype(range)>(range)); });
        }

        // ***************************************** reverse **********************************************

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

            template <typename X = R, std::enable_if_t<common_range<X>>* = nullptr>
            auto begin()
            {
                return std::make_reverse_iterator(RAH_NAMESPACE::end(base_));
            }

            template <typename X = R, std::enable_if_t<!common_range<X>>* = nullptr>
            auto begin()
            {
                return std::make_reverse_iterator(
                    RAH_NAMESPACE::next(RAH_NAMESPACE::begin(base_), RAH_NAMESPACE::end(base_)));
            }

            auto end()
            {
                return std::make_reverse_iterator(RAH_NAMESPACE::begin(base_));
            }
        };

        template <typename R>
        auto reverse(R&& range)
        {
            static_assert(
                RAH_NAMESPACE::bidirectional_range<R>, "reverse expect a bidirectional_range");
            auto ref = RAH_NAMESPACE::views::all(std::forward<R>(range));
            return reverse_view<decltype(ref)>(std::move(ref));
        }

        inline auto reverse()
        {
            return make_pipeable([=](auto&& range)
                                 { return reverse(RAH_STD::forward<decltype(range)>(range)); });
        }

        // **************************** element_view **********************************************

        template <typename R, size_t N>
        class elements_view : public view_interface<elements_view<R, N>>
        {
            R base_;

            using inner_iterator = iterator_t<R>;
            using inner_sentinel = sentinel_t<R>;

        public:
            elements_view() = default;
            elements_view(R base)
                : base_(std::move(base))
            {
            }

            using reference = decltype(std::get<N>(std::declval<range_reference_t<R>>()));

            struct sentinel
            {
                inner_sentinel sent;
            };

            struct iterator : iterator_facade<iterator, sentinel, reference, range_iter_categ_t<R>>
            {
                inner_iterator iter_;

                iterator(inner_iterator it)
                    : iter_(std::move(it))
                {
                }

                reference operator*()
                {
                    return std::get<N>(*iter_);
                }

                iterator& operator++()
                {
                    ++iter_;
                    return *this;
                }

                iterator& operator--()
                {
                    --iter_;
                    return *this;
                }

                friend bool operator==(iterator it, iterator it2)
                {
                    return it.iter_ == it2.iter_;
                }
                friend bool operator==(iterator it, sentinel sent)
                {
                    return it.iter_ == sent.sent;
                }
            };

            auto begin()
            {
                return iterator{RAH_NAMESPACE::begin(base_)};
            }

            template <typename U = R, std::enable_if_t<RAH_NAMESPACE::common_range<U>>* = nullptr>
            auto end()
            {
                return iterator(RAH_NAMESPACE::end(base_));
            }

            template <typename U = R, std::enable_if_t<!RAH_NAMESPACE::common_range<U>>* = nullptr>
            auto end()
            {
                return sentinel(RAH_NAMESPACE::end(base_));
            }
        };

        template <size_t N, typename T>
        auto elements(T&& range)
        {
            auto ref = RAH_NAMESPACE::views::all(std::forward<T>(range));
            return elements_view<decltype(ref), N>(std::move(ref));
        }

        template <size_t N>
        auto elements()
        {
            return make_pipeable([=](auto&& range)
                                 { return elements<N>(RAH_STD::forward<decltype(range)>(range)); });
        }

        template <typename T>
        auto keys(T&& range)
        {
            auto ref = RAH_NAMESPACE::views::all(std::forward<T>(range));
            return elements_view<decltype(ref), 0>(std::move(ref));
        }

        inline auto keys()
        {
            return make_pipeable([=](auto&& range)
                                 { return keys(RAH_STD::forward<decltype(range)>(range)); });
        }

        template <typename T>
        auto values(T&& range)
        {
            auto ref = RAH_NAMESPACE::views::all(std::forward<T>(range));
            return elements_view<decltype(ref), 1>(std::move(ref));
        }

        inline auto values()
        {
            return make_pipeable([=](auto&& range)
                                 { return values(RAH_STD::forward<decltype(range)>(range)); });
        }

        // *************************** enumerate **********************************************************

        template <typename R>
        class enumerate_view : public view_interface<enumerate_view<R>>
        {
            R base_;
            using inner_iterator = iterator_t<R>;
            using inner_sentinel = iterator_t<R>;

        public:
            enumerate_view(R base)
                : base_(std::move(base))
            {
            }

            using value_type = std::pair<int64_t, range_reference_t<R>>;

            struct sentinel
            {
                inner_sentinel sent;
            };

            struct iterator : iterator_facade<iterator, sentinel, value_type, range_iter_categ_t<R>>
            {
                inner_iterator current_;
                int64_t pos_;

                iterator(inner_iterator current, int64_t pos)
                    : current_(current)
                    , pos_(pos)
                {
                }

                iterator& operator++()
                {
                    ++current_;
                    ++pos_;
                    return *this;
                }

                iterator& operator--()
                {
                    ++current_;
                    ++pos_;
                    return *this;
                }

                value_type operator*()
                {
                    return {pos_, *current_};
                }

                friend bool operator==(iterator const& iter, sentinel const& sent)
                {
                    return iter.current_ == sent.sent;
                }
                friend bool operator==(iterator const& iter, iterator const& iter2)
                {
                    return iter.pos_ == iter2.pos_;
                }
            };

            auto begin()
            {
                return iterator(RAH_NAMESPACE::begin(base_), 0);
            }

            template <
                typename U = R,
                std::enable_if_t<
                    RAH_NAMESPACE::random_access_range<U> and RAH_NAMESPACE::common_range<U>>* = nullptr>
            auto end()
            {
                return iterator(RAH_NAMESPACE::end(base_), RAH_NAMESPACE::size(base_));
            }

            template <
                typename U = R,
                std::enable_if_t<not(
                    RAH_NAMESPACE::random_access_range<U> and RAH_NAMESPACE::common_range<U>)>* = nullptr>
            auto end()
            {
                return sentinel(RAH_NAMESPACE::end(base_));
            }
        };

        template <typename R>
        auto enumerate(R&& range)
        {
            auto ref = RAH_NAMESPACE::views::all(std::forward<R>(range));
            return enumerate_view<decltype(ref)>(std::move(ref));
        }

        inline auto enumerate()
        {
            return make_pipeable([](auto&& range)
                                 { return enumerate(RAH_STD::forward<decltype(range)>(range)); });
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

            template <typename... Args, size_t... Is>
            auto deref_impl(RAH_STD::tuple<Args...>& t, RAH_STD::index_sequence<Is...>)
            {
                return RAH_STD::tuple<typename RAH_STD::iterator_traits<Args>::reference...>(
                    (*RAH_STD::get<Is>(t))...);
            }

            template <typename... Args>
            auto deref(RAH_STD::tuple<Args...>& t)
            {
                return deref_impl(t, RAH_STD::make_index_sequence<sizeof...(Args)>{});
            }

            template <typename F, typename... Args, size_t... Is>
            auto
            derefCall_impl(F const& func, RAH_STD::tuple<Args...>& t, RAH_STD::index_sequence<Is...>)
            {
                return func((*RAH_STD::get<Is>(t))...);
            }

            template <typename F, typename... Args>
            auto derefCall(F const& func, RAH_STD::tuple<Args...>& t)
            {
                return derefCall_impl(func, t, RAH_STD::make_index_sequence<sizeof...(Args)>{});
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

            template <typename Function, typename Tuple, size_t... I>
            auto apply(Function&& f, Tuple&& t, std::index_sequence<I...>)
            {
                return f(std::get<I>(t)...);
            }

            template <typename Function, typename Tuple>
            auto apply(Function&& f, Tuple&& t)
            {
                static constexpr auto size = std::tuple_size<Tuple>::value;
                return apply(f, t, std::make_index_sequence<size>{});
            }

        } // namespace details
        /// \endcond

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
            using SentinelTuple = decltype(details::transform_each(bases_, range_end()));

        public:
            struct sentinel
            {
                SentinelTuple sentinels;
            };

            struct iterator
                : iterator_facade<
                      iterator,
                      sentinel,
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
                auto operator*()
                {
                    return details::deref(iters_);
                }
                auto operator-(iterator const& other) const
                {
                    return RAH_STD::get<0>(iters_) - RAH_STD::get<0>(other.iters_);
                }
                friend bool operator==(iterator const& iter, iterator const& iter2)
                {
                    return details::equal(iter.iters_, iter2.iters_);
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

            zip_view(RangeTuple rangeTuple)
                : bases_(RAH_STD::move(rangeTuple))
            {
            }

            iterator begin()
            {
                view_interface<zip_view<RangeTuple>>::deleteCheck.check();
                return iterator(details::transform_each(bases_, range_begin()));
            }

            sentinel end()
            {
                view_interface<zip_view<RangeTuple>>::deleteCheck.check();
                return sentinel{details::transform_each(bases_, range_end())};
            }
        };

        template <typename... R>
        auto zip(R&&... ranges)
        {
            auto refTuple =
                RAH_STD::make_tuple(RAH_NAMESPACE::views::all(RAH_STD::forward<R>(ranges))...);
            return zip_view<decltype(refTuple)>(RAH_STD::move(refTuple));
        }

        // ************************************ zip_transform *************************************

        template <typename Func, typename RangeTuple>
        class zip_transform_view : public view_interface<zip_transform_view<Func, RangeTuple>>
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
            Func func_;
            RangeTuple bases_;
            using IterTuple = decltype(details::transform_each(bases_, range_begin()));
            using SentinelTuple = decltype(details::transform_each(bases_, range_end()));
            static constexpr size_t tuple_size = std::tuple_size_v<RangeTuple>;

        public:
            struct sentinel
            {
                SentinelTuple sentinels;
            };

            struct iterator
                : iterator_facade<
                      iterator,
                      sentinel,
                      decltype(details::deref(RAH_NAMESPACE::details::declval<IterTuple>())),
                      RAH_STD::bidirectional_iterator_tag>
            {
                zip_transform_view* parent_;
                IterTuple iters_;

                iterator()
                {
                }
                iterator(zip_transform_view* parent, IterTuple iterators)
                    : parent_(parent)
                    , iters_(iterators)
                {
                }
                iterator& operator++()
                {
                    details::for_each(iters_, [](auto& iter) { ++iter; });
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
                auto operator*()
                {
                    return details::derefCall_impl(
                        parent_->func_, iters_, std::make_index_sequence<tuple_size>{});
                }
                auto operator-(iterator const& other) const
                {
                    return RAH_STD::get<0>(iters_) - RAH_STD::get<0>(parent_->other.iters_);
                }
                friend bool operator==(iterator const& iter, iterator const& iter2)
                {
                    return details::equal(iter.iters_, iter2.iters_);
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

            zip_transform_view(Func f, RangeTuple rangeTuple)
                : func_(std::move(f))
                , bases_(RAH_STD::move(rangeTuple))
            {
            }

            iterator begin()
            {
                return iterator(this, details::transform_each(bases_, range_begin()));
            }

            sentinel end()
            {
                return sentinel{details::transform_each(bases_, range_end())};
            }
        };

        /// @brief that takes an invocable object and one or more views, and produces a view
        /// whose ith element is the result of applying the invocable object
        /// to the ith elements of all views.
        ///
        /// @snippet rah4_unittests.cpp zip_transform
        template <typename F, typename... R>
        auto zip_transform(F&& func, R&&... ranges)
        {
            auto refTuple =
                RAH_STD::make_tuple(RAH_NAMESPACE::views::all(RAH_STD::forward<R>(ranges))...);
            return zip_transform_view<std::remove_reference_t<F>, decltype(refTuple)>(
                std::forward<F>(func), RAH_STD::move(refTuple));
        }

        // ******************************************* adjacent ***********************************

        template <typename R, size_t N>
        struct adjacent_view : view_interface<adjacent_view<R, N>>
        {
            R inputView_;

            using base_iterator = iterator_t<R>;
            using base_sentinel = sentinel_t<R>;
            using base_value = range_value_t<R>;
            using base_reference = range_reference_t<R>;

            template <size_t I>
            struct GetType
            {
                using type = base_value;
            };

            template <std::size_t... Is>
            static auto make_tuple(std::index_sequence<Is...>)
            {
                return std::tuple<typename GetType<Is>::type...>();
            }

            using value = decltype(make_tuple(std::make_integer_sequence<size_t, N>{}));
            using reference = value;

            struct sentinel
            {
                base_sentinel sent;
            };

            struct iterator
                : iterator_facade<iterator, sentinel, reference, std::forward_iterator_tag>
            {
                base_iterator subRangeBegin_;
                base_value sliding_result[N];
                size_t result_output_index = 0;

                iterator() = default;
                iterator(base_iterator subRangeBegin, base_sentinel sentinel)
                    : subRangeBegin_(std::move(subRangeBegin))
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

                template <std::size_t... Is>
                auto make_result(std::index_sequence<Is...>)
                {
                    sliding_result[result_output_index] = *subRangeBegin_;
                    return std::make_tuple((sliding_result[(result_output_index + 1 + Is) % N])...);
                }

                auto operator*()
                {
                    return make_result(std::make_integer_sequence<size_t, N>{});
                }
                friend bool operator==(iterator const& i, iterator const& i2)
                {
                    return i.subRangeBegin_ == i2.subRangeBegin_;
                }
                friend bool operator==(iterator const& i, sentinel const& s)
                {
                    return i.subRangeBegin_ == s.sent;
                }
                friend bool operator==(sentinel const& s, iterator const& i)
                {
                    return i.subRangeBegin_ == s.sent;
                }
            };

            adjacent_view(R inputView)
                : inputView_(std::move(inputView))
            {
            }

            auto begin()
            {
                auto const rangeEnd = RAH_NAMESPACE::end(inputView_);
                auto const subRangeBegin = RAH_NAMESPACE::begin(inputView_);
                return iterator(subRangeBegin, rangeEnd);
            }

            auto end()
            {
                return sentinel{RAH_NAMESPACE::end(inputView_)};
            }
        };

        template <size_t N, typename R, std::enable_if_t<N != 0>* = nullptr>
        auto adjacent(R&& range)
        {
            auto rangeView = all(range);
            return adjacent_view<decltype(rangeView), N>(std::move(rangeView));
        }

        template <size_t N, typename R, std::enable_if_t<N == 0>* = nullptr>
        auto adjacent(R&& range)
        {
            auto rangeView = all(range);
            return views::empty<std::tuple<>>();
        }

        template <size_t N>
        auto adjacent()
        {
            return make_pipeable([=](auto&& range)
                                 { return adjacent<N>(RAH_STD::forward<decltype(range)>(range)); });
        }

        template <typename R>
        auto pairwise(R&& range)
        {
            auto rangeView = all(range);
            return adjacent_view<decltype(rangeView), 2>(std::move(rangeView));
        }

        inline auto pairwise()
        {
            return make_pipeable([=](auto&& range)
                                 { return adjacent<2>(RAH_STD::forward<decltype(range)>(range)); });
        }

        // ********************************* adjacent_transform ***********************************

        template <typename R, typename F, size_t N>
        struct adjacent_transform_view : view_interface<adjacent_transform_view<R, F, N>>
        {
            F func_;

            using inner_reference = range_reference_t<R>;

            using inner_adjacent_view = adjacent_view<R, N>;
            inner_adjacent_view inner_;

            using adjacent_iterator = iterator_t<inner_adjacent_view>;
            using adjacent_reference = range_reference_t<inner_adjacent_view>;
            using adjacent_sentinel = sentinel_t<inner_adjacent_view>;

            using reference = decltype(details::apply(func_, std::declval<adjacent_reference>()));
            using value = reference;

            struct sentinel
            {
                adjacent_sentinel sent;
            };

            struct iterator
                : iterator_facade<iterator, sentinel, reference, std::forward_iterator_tag>
            {
                adjacent_transform_view* view_ = nullptr;
                adjacent_iterator iter_;

                iterator() = default;
                iterator(adjacent_transform_view* parent, adjacent_iterator iter)
                    : view_(parent)
                    , iter_(std::move(iter))
                {
                }

                iterator& operator++()
                {
                    ++iter_;
                    return *this;
                }

                auto operator*()
                {
                    return details::apply(view_->func_, *iter_, std::make_index_sequence<N>{});
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
                : func_(std::move(func))
                , inner_(std::move(inputView))
            {
            }

            auto begin()
            {
                return iterator(this, RAH_NAMESPACE::begin(inner_));
            }

            auto end()
            {
                return sentinel{RAH_NAMESPACE::end(inner_)};
            }
        };

        template <size_t N, typename R, typename F, std::enable_if_t<N != 0>* = nullptr>
        auto adjacent_transform(R&& range, F&& func)
        {
            auto rangeView = all(range);
            return adjacent_transform_view<decltype(rangeView), F, N>(
                std::move(rangeView), std::forward<F>(func));
        }

        template <size_t N, typename R, typename F, std::enable_if_t<N == 0>* = nullptr>
        auto adjacent_transform(R&&, F&&)
        {
            return views::empty<std::tuple<>>();
        }

        template <size_t N, typename F>
        auto adjacent_transform(F&& func)
        {
            return make_pipeable(
                [=](auto&& range)
                { return adjacent_transform<N>(RAH_STD::forward<decltype(range)>(range), func); });
        }

        // ******************************************* sliding ********************************************

        template <typename R>
        struct slide_view : view_interface<slide_view<R>>
        {
            R inputView_;
            intptr_t count_;

            using base_iterator = iterator_t<R>;
            using base_sentinel = sentinel_t<R>;

            struct sentinel
            {
                base_sentinel sent;
            };

            struct iterator : iterator_facade<
                                  iterator,
                                  sentinel,
                                  subrange<base_iterator, base_iterator>,
                                  RAH_NAMESPACE::range_iter_categ_t<R>>
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
                if (count_ == 0)
                {
                    return iterator(subRangeLast, subRangeLast);
                }
                auto const left = RAH_NAMESPACE::advance(subRangeLast, count_ - 1, rangeEnd);
                if (left != 0)
                {
                    return iterator(subRangeLast, subRangeLast);
                }
                return iterator(subRangeBegin, subRangeLast);
            }

            template <typename U = R, std::enable_if_t<RAH_NAMESPACE::common_range<U>>* = nullptr>
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

            template <typename U = R, std::enable_if_t<not RAH_NAMESPACE::common_range<U>>* = nullptr>
            auto end()
            {
                return sentinel{RAH_NAMESPACE::end(inputView_)};
            }
        };

        template <typename R>
        auto slide(R&& range, size_t n)
        {
            auto rangeView = all(range);
            return slide_view<decltype(rangeView)>(std::move(rangeView), n);
        }

        inline auto slide(size_t n)
        {
            return make_pipeable([=](auto&& range)
                                 { return slide(RAH_STD::forward<decltype(range)>(range), n); });
        }

        // ************************************ chunk *********************************************

        template <typename R>
        struct chunk_view : view_interface<chunk_view<R>>
        {
            R base_;
            size_t step_;
            using inner_iterator = iterator_t<R>;
            using inner_sentinel = sentinel_t<R>;

            struct sentinel
            {
                sentinel_t<R> sent;
            };

            struct iterator
                : iterator_facade<iterator, sentinel, subrange<iterator_t<R>, iterator_t<R>>, RAH_STD::forward_iterator_tag>
            {
                iterator_t<R> iter_;
                iterator_t<R> iter2_;
                sentinel_t<R> end_;
                size_t step_;

                iterator(
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

                iterator& operator++()
                {
                    iter_ = iter2_;
                    RAH_NAMESPACE::advance(iter2_, step_, end_);
                    return *this;
                }

                auto operator*() const
                {
                    return make_subrange(iter_, iter2_);
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

            chunk_view(R base, size_t step)
                : base_(std::move(base))
                , step_(step)
            {
            }

            auto begin()
            {
                auto iter = RAH_NAMESPACE::begin(base_);
                auto endIter = RAH_NAMESPACE::end(base_);
                iterator begin{iter, iter, endIter, step_};
                ++begin;
                return begin;
            }

            template <typename U = R, std::enable_if_t<RAH_NAMESPACE::common_range<U>>* = nullptr>
            auto end()
            {
                auto endIter = RAH_NAMESPACE::end(base_);
                return iterator{endIter, endIter, endIter, step_};
            }

            template <
                typename U = R,
                std::enable_if_t<
                    !RAH_NAMESPACE::common_range<U>
                    and !RAH_NAMESPACE::sized_sentinel_for<inner_sentinel, inner_iterator>>* = nullptr>
            auto end()
            {
                return sentinel{RAH_NAMESPACE::end(base_)};
            }

            template <
                typename U = R,
                std::enable_if_t<
                    !RAH_NAMESPACE::common_range<U>
                    and RAH_NAMESPACE::sized_sentinel_for<inner_sentinel, inner_iterator>>* = nullptr>
            auto end()
            {
                auto iter = RAH_NAMESPACE::begin(base_);
                auto sent = RAH_NAMESPACE::end(base_);
                iter = sent;
                return iterator{iter, iter, sent, step_};
            }
        };

        template <typename R, RAH_STD::enable_if_t<not RAH_STD::is_rvalue_reference_v<R&&>, int> = 0>
        auto chunk(R&& range, size_t step)
        {
            auto ref = RAH_NAMESPACE::views::all(range);
            return chunk_view<decltype(ref)>(std::move(ref), step);
        }

        inline auto chunk(size_t step)
        {
            return make_pipeable([=](auto&& range)
                                 { return chunk(RAH_STD::forward<decltype(range)>(range), step); });
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

        // ******************************************* unbounded **********************************

        template <typename I>
        class unbounded_view : public view_interface<unbounded_view<I>>
        {
            I iter_;

        public:
            explicit unbounded_view(I iter)
                : iter_(std::move(iter))
            {
            }
            class iterator : public iterator_facade<
                                 iterator,
                                 default_sentinel,
                                 iter_reference_t<I>,
                                 typename RAH_STD::iterator_traits<I>::iterator_category>
            {
                I iter_;

            public:
                iterator() = default;
                explicit iterator(I iter)
                    : iter_(iter)
                {
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
                iterator& operator--()
                {
                    --iter_;
                    return *this;
                }
                friend int64_t operator-(iterator const& it1, iterator const& it2)
                {
                    return it1.iter_ - it2.iter_;
                }

                iter_reference_t<I> operator*()
                {
                    return *iter_;
                }
                friend bool operator==(iterator const& it1, iterator const& it2)
                {
                    return it1.iter_ == it2.iter_;
                }
                friend bool operator==(iterator const&, default_sentinel const&)
                {
                    return false;
                }
                friend bool operator==(default_sentinel const&, iterator const&)
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
                return default_sentinel();
            }
            static bool empty()
            {
                return false;
            }
        };

        template <typename I>
        auto unbounded(I&& it)
        {
            return unbounded_view<I>(std::forward<I>(it));
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
            friend constexpr bool operator==(irange_iterator const& it1, irange_iterator const& it2)
            {
                return it1.val_ == it2.val_;
            }
            friend constexpr bool operator==(default_sentinel const&, irange_iterator const&)
            {
                return false;
            }
            friend constexpr bool operator==(irange_iterator const&, default_sentinel const&)
            {
                return false;
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
            return subrange<irange_iterator<T>, default_sentinel>{{start, step}, default_sentinel{}};
        }

        // ********************************** cycle ********************************************************

        template <typename R>
        class cycle_view : public view_interface<cycle_view<R>>
        {
            R base_;
            using base_iterator = iterator_t<R>;
            using base_sentinel = iterator_t<R>;

        public:
            class iterator
                : public iterator_facade<
                      iterator,
                      default_sentinel,
                      RAH_NAMESPACE::range_reference_t<R>,
                      common_iterator_tag<RAH_STD::bidirectional_iterator_tag, range_iter_categ_t<R>>>
            {
                cycle_view* view_;
                base_iterator beginIter_;
                base_sentinel endIter_;
                base_iterator iter_;

            public:
                iterator() = default;
                explicit iterator(cycle_view* v)
                    : view_(v)
                    , beginIter_(RAH_NAMESPACE::begin(v->base_))
                    , endIter_(RAH_NAMESPACE::end(v->base_))
                    , iter_(beginIter_)
                {
                }

                iterator& operator++()
                {
                    assert(view_ != nullptr);
                    ++iter_;
                    while (iter_ == endIter_)
                    {
                        iter_ = RAH_NAMESPACE::begin(view_->base_);
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
                RAH_NAMESPACE::range_reference_t<R> operator*()
                {
                    assert(view_ != nullptr);
                    return *iter_;
                }
                friend bool operator==(iterator const& it, iterator const& it2)
                {
                    return it.iter_ == it2.iter_;
                }
                friend bool operator==(iterator const&, default_sentinel const&)
                {
                    return false;
                }
                friend bool operator==(default_sentinel const&, iterator const&)
                {
                    return false;
                }
            };

            cycle_view() = default;
            explicit cycle_view(R base)
                : base_(std::move(base))
            {
            }

            iterator begin()
            {
                return iterator(this);
            }

            default_sentinel end()
            {
                return {};
            }

            bool empty()
            {
                return RAH_NAMESPACE::empty(base_);
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
        template <typename F>
        class generate_view : view_interface<generate_view<F>>
        {
            using value =
                RAH_NAMESPACE::remove_cvref_t<decltype(RAH_NAMESPACE::details::declval<F>()())>;
            F func_;

        public:
            class iterator
                : public iterator_facade<iterator, default_sentinel, value, RAH_STD::forward_iterator_tag>
            {
                generate_view* parent_ = nullptr;
                RAH_NAMESPACE::details::optional<value> value_;

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
                value operator*()
                {
                    return *value_;
                }
                friend bool operator==(iterator const&, default_sentinel const&)
                {
                    return false;
                }
                friend bool operator==(default_sentinel const&, iterator const&)
                {
                    return false;
                }
            };

            explicit generate_view(F func)
                : func_(std::move(func))
            {
            }

            auto begin()
            {
                return iterator(this);
            }
            default_sentinel end()
            {
                return {};
            }
        };

        template <typename F>
        auto generate(F&& func)
        {
            using Functor = RAH_STD::remove_cv_t<RAH_STD::remove_reference_t<F>>;
            return generate_view<Functor>(RAH_STD::forward<F>(func));
        }

        template <typename F>
        auto generate_n(size_t count, F&& func)
        {
            return generate(RAH_STD::forward<F>(func)) | take(count);
        }

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

        public:
            class iterator
                : public iterator_facade<iterator, default_sentinel, reference, RAH_STD::forward_iterator_tag>
            {
                inner_iterator1 first1_;
                inner_sentinel1 last1_;
                inner_iterator2 first2_;
                inner_sentinel2 last2_;

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
                iterator(
                    inner_iterator1 first1,
                    inner_sentinel1 last1,
                    inner_iterator2 first2,
                    inner_sentinel2 last2)
                    : first1_(std::move(first1))
                    , last1_(std::move(last1))
                    , first2_(std::move(first2))
                    , last2_(std::move(last2))
                {
                    next_value();
                }

                iterator& operator++()
                {
                    ++first1_;
                    next_value();
                    return *this;
                }
                auto operator*() const -> decltype(*first1_)
                {
                    return *first1_;
                }
                friend bool operator==(iterator const& it, default_sentinel)
                {
                    return it.first1_ == it.last1_;
                }
            };
            set_difference_view(InputRng1 base1, InputRng2 base2)
                : base1_(std::move(base1))
                , base2_(std::move(base2))
            {
            }
            auto begin()
            {
                return iterator(
                    RAH_NAMESPACE::begin(base1_),
                    RAH_NAMESPACE::end(base1_),
                    RAH_NAMESPACE::begin(base2_),
                    RAH_NAMESPACE::end(base2_));
            }
            default_sentinel end()
            {
                return {};
            }
        };

        template <typename R1, typename R2>
        auto set_difference(R1&& range1, R2&& range2)
        {
            auto ref1 = RAH_NAMESPACE::views::all(range1);
            auto ref2 = RAH_NAMESPACE::views::all(range2);
            return set_difference_view<decltype(ref1), decltype(ref2)>(
                std::move(ref1), std::move(ref2));
        }

        template <typename R2>
        auto set_difference(R2&& range2)
        {
            return make_pipeable(
                [r2 = all(range2)](auto&& range)
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
        class slice_view : public view_interface<slice_view<R>>
        {
            R base_;
            intptr_t begin_idx_;
            intptr_t end_idx_;
            using iterator = RAH_NAMESPACE::iterator_t<R>;

        public:
            slice_view() = default;
            slice_view(R v, intptr_t begin_idx, intptr_t end_idx)
                : base_(std::move(v))
                , begin_idx_(begin_idx)
                , end_idx_(end_idx)
            {
            }

            template <typename U = R, std::enable_if_t<RAH_NAMESPACE::random_access_range<U>>* = nullptr>
            auto begin()
            {
                auto iter = RAH_NAMESPACE::begin(base_);
                RAH_NAMESPACE::advance(iter, begin_idx_, RAH_NAMESPACE::end(base_));
                return iter;
            }

            template <typename U = R, std::enable_if_t<not RAH_NAMESPACE::random_access_range<U>>* = nullptr>
            auto begin()
            {
                auto iter = RAH_NAMESPACE::begin(base_);
                RAH_NAMESPACE::advance(iter, begin_idx_, RAH_NAMESPACE::end(base_));
                return counted_iterator<iterator_t<U>>(iter, end_idx_ - begin_idx_);
            }

            template <typename U = R, std::enable_if_t<RAH_NAMESPACE::random_access_range<U>>* = nullptr>
            auto end()
            {
                auto iter = RAH_NAMESPACE::begin(base_);
                RAH_NAMESPACE::advance(iter, end_idx_, RAH_NAMESPACE::end(base_));
                return iter;
            }

            template <typename U = R, std::enable_if_t<not RAH_NAMESPACE::random_access_range<U>>* = nullptr>
            default_sentinel end()
            {
                return default_sentinel{};
            }
        };

        template <typename R>
        auto slice(R&& range, intptr_t begin_idx, intptr_t end_idx)
        {
            auto ref = RAH_NAMESPACE::views::all(std::forward<R>(range));
            return slice_view<decltype(ref)>{std::move(ref), begin_idx, end_idx};
        }

        inline auto slice(intptr_t begin, intptr_t end)
        {
            return make_pipeable(
                [=](auto&& range)
                { return slice(RAH_STD::forward<decltype(range)>(range), begin, end); });
        }

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

        public:
            static_assert(
                std::is_same_v<range_reference_t<R1>, range_reference_t<R2>>,
                "R1 and R2 doesn't have the same reference type");
            using reference = range_reference_t<R1>;

            struct iterator
                : iterator_facade<iterator, default_sentinel, reference, RAH_STD::forward_iterator_tag>
            {
                r1_iterator iter1_;
                r1_sentinel sent1_;
                r2_iterator iter2_;
                r2_sentinel sent2_;
                size_t range_index_;

                iterator(r1_iterator iter1, r1_sentinel sent1, r2_iterator iter2, r2_sentinel sent2)
                    : iter1_(iter1)
                    , sent1_(sent1)
                    , iter2_(iter2)
                    , sent2_(sent2)
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
                friend bool operator==(iterator const& it, default_sentinel const&)
                {
                    if (it.range_index_ == 0)
                        return it.iter1_ == it.sent1_;
                    else
                        return it.iter2_ == it.sent2_;
                }
                friend bool operator==(default_sentinel const&, iterator const& it)
                {
                    if (it.range_index_ == 0)
                        return it.iter1_ == it.sent1_;
                    else
                        return it.iter2_ == it.sent2_;
                }
            };

            concat_view(R1 base1, R2 base2)
                : base1_(base1)
                , base2_(base2)
            {
            }

            auto begin()
            {
                return iterator(
                    RAH_NAMESPACE::begin(base1_),
                    RAH_NAMESPACE::end(base1_),
                    RAH_NAMESPACE::begin(base2_),
                    RAH_NAMESPACE::end(base2_));
            }

            default_sentinel end()
            {
                return {};
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
            auto r1 = views::all(range1);
            auto r2 = views::all(range2);
            return concat_view<decltype(r1), decltype(r2)>(std::move(r1), std::move(r2));
        }

        /// @see rah::views::concat(R1&& range1, R2&& range2)
        template <typename R1, typename R2, typename... Ranges>
        auto concat(R1&& range1, R2&& range2, Ranges&&... ranges)
        {
            return concat(
                concat(RAH_STD::forward<R1>(range1), RAH_STD::forward<R2>(range2)), ranges...);
        }

    } // namespace views

} // namespace RAH_NAMESPACE
