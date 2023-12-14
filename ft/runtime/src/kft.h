//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//


#ifndef KFT_H
#   define KFT_H

#   include <stdlib.h>
#   include <stdint.h>

#   ifdef __cplusplus
    extern "C" {
#   endif

    extern int32_t __ft_vote(void *, int32_t);
    extern int32_t __ft_voter(void *, int32_t);
    extern int32_t __ft_votel(void *, int32_t);
    extern int32_t __ft_votenow(void *, int32_t);
    extern int32_t __ft_atomic_vote(void *, int32_t);
    extern int32_t __ft_atomic_voter(void *, int32_t);
    extern int32_t __ft_atomic_votel(void *, int32_t);
    extern int32_t __ft_auto_vote(void *, int32_t);
    extern int32_t __ft_auto_voter(void *, int32_t);
    extern int32_t __ft_auto_votel(void *, int32_t);
    extern int32_t __ft_auto_atomic_vote(void *, int32_t);
    extern int32_t __ft_auto_atomic_voter(void *, int32_t);
    extern int32_t __ft_auto_atomic_votel(void *, int32_t);

    extern int32_t __ft_vote_debug(void *, int32_t , void *,  int32_t);
    extern int32_t __ft_voter_debug(void *, int32_t , void *,  int32_t);
    extern int32_t __ft_votel_debug(void *, int32_t , void *,  int32_t);
    extern int32_t __ft_votenow_debug(void *, int32_t , void *,  int32_t);
    extern int32_t __ft_atomic_vote_debug(void *, int32_t , void *,  int32_t);
    extern int32_t __ft_atomic_voter_debug(void *, int32_t , void *,  int32_t);
    extern int32_t __ft_atomic_votel_debug(void *, int32_t , void *,  int32_t);
    extern int32_t __ft_auto_vote_debug(void *, int32_t , void *,  int32_t);
    extern int32_t __ft_auto_voter_debug(void *, int32_t , void *,  int32_t);
    extern int32_t __ft_auto_votel_debug(void *, int32_t , void *,  int32_t);
    extern int32_t __ft_auto_atomic_vote_debug(void *, int32_t , void *,  int32_t);
    extern int32_t __ft_auto_atomic_voter_debug(void *, int32_t , void *,  int32_t);
    extern int32_t __ft_auto_atomic_votel_debug(void *, int32_t , void *,  int32_t);
  
    extern int32_t __ft_dummy(int32_t, int32_t);

#   ifdef __cplusplus
    }
#   endif

#endif /* KFT_H */
