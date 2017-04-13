C                                                          Include PUVD
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-2000, 2002, 2007-2010, 2012, 2014-2015
C;  Associated Universities, Inc. Washington DC, USA.
C;
C;  This program is free software; you can redistribute it and/or
C;  modify it under the terms of the GNU General Public License as
C;  published by the Free Software Foundation; either version 2 of
C;  the License, or (at your option) any later version.
C;
C;  This program is distributed in the hope that it will be useful,
C;  but WITHOUT ANY WARRANTY; without even the implied warranty of
C;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C;  GNU General Public License for more details.
C;
C;  You should have received a copy of the GNU General Public
C;  License along with this program; if not, write to the Free
C;  Software Foundation, Inc., 675 Massachusetts Ave, Cambridge,
C;  MA 02139, USA.
C;
C;  Correspondence concerning AIPS should be addressed as follows:
C;         Internet email: aipsmail@nrao.edu.
C;         Postal address: AIPS Project Office
C;                         National Radio Astronomy Observatory
C;                         520 Edgemont Road
C;                         Charlottesville, VA 22903-2475 USA
C-----------------------------------------------------------------------
C                                       Parameters for uv data
      INTEGER   MAXANT,  MXBASE,  MAXIF, MAXFLD, MAXAFL, MAXCHA, MAXCIF,
     *   MAXSMO, MAXCID, MAXFQ
C                                       MAXANT = Max. no. antennas.
      PARAMETER (MAXANT=90)
C                                       MXBASE = max. no. baselines
      PARAMETER (MXBASE= ((MAXANT*(MAXANT+1))/2))
C                                       MAXIF=max. no. IFs.
      PARAMETER (MAXIF=90)
C                                       MAXFLD=max. no fields
      PARAMETER (MAXFLD=4096)
C                                       MAXAFL=max. no fields in adverbs
      PARAMETER (MAXAFL=64)
C                                       MAXCHA=max. no. freq. channels.
      PARAMETER (MAXCHA=132096)
C                                       MAXCIF = max. product of no.
C                                       channels times no. IFs times
C                                       no. polarizations
      PARAMETER (MAXCIF=132096)
C                                       MAXSMO = max freq smoothing
      PARAMETER (MAXSMO=256)
C                                       MAXCID = max correlation id.'s
      PARAMETER (MAXCID=64)
C                                       MAXCFQ = max FQ ID'S
      PARAMETER (MAXFQ=256)
C                                       Parameters for tables
      INTEGER MAXCLC, MAXSNC, MAXANC, MAXNXC, MAXSUC, MAXBPC, MAXBLC,
     *   MAXBSC, MAXFQC, MAXTYC, MAXCCC, MAXFGC
C                                       MAXCLC=max no. cols in CL table
      PARAMETER (MAXCLC=33)
C                                       MAXSNC=max no. cols in SN table
      PARAMETER (MAXSNC=26)
C                                       MAXANC=max no. cols in AN table
      PARAMETER (MAXANC=14)
C                                       MAXNXC=max no. cols in NX table
      PARAMETER (MAXNXC=7)
C                                       MAXSUC=max no. cols in SU table
      PARAMETER (MAXSUC=21)
C                                       MAXBPC=max no. cols in BP table
      PARAMETER (MAXBPC=16)
C                                       MAXBLC=max no. cols in BL table
      PARAMETER (MAXBLC=14)
C                                       MAXBFC=max no. cols in BS table
      PARAMETER (MAXBSC=21)
C                                       MAXFQC=max no. cols in FQ table
      PARAMETER (MAXFQC=6)
C                                       MAXTYC=max no. cols in TY table
      PARAMETER (MAXTYC=10)
C                                       MAXCCC=max no. cols in CC table
      PARAMETER (MAXCCC=7)
C                                       MAXFGC=max no. cols in FG table
      PARAMETER (MAXFGC=9)
C                                       former DSEL parameters:
      INTEGER   XCTBSZ, XBTBSZ, XPTBSZ, XSTBSZ, XBPRSZ, XCLRSZ
C                                       XCTBSZ=internal gain table size
      PARAMETER (XCTBSZ=25000)
C                                       XBTBSZ=baseline table size
C                                       90 IFs and 29 antennas ONLY
      PARAMETER (XBTBSZ=150000)
C                                       XPTBSZ=polar. corr. table size
C                                       Nbasl * 16 * Nif  continuum
C                                       Enough 32 IFs w 28 antennas
      PARAMETER (XPTBSZ=196608)
C                                       XSTBSZ=Source no. table size
      PARAMETER (XSTBSZ=10000)
C                                       XBPRSZ=BP record size
      PARAMETER (XBPRSZ=2*MAXCIF+10+4*MAXIF)
C                                       XCLRSZ=CL record size
      PARAMETER (XCLRSZ=40+15*MAXIF)
C                                                          End PUVD.
