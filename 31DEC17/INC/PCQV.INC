C                                       Include for correlator parameter
C                                       frequency (CQ) table.
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997, 2007
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
C                                       Requires PUVD.INC
      INTEGER MAXCQC, NKEYCQ, NKYCQ2, MXSPCQ, ICQREV
C                                       # cols. in CQ table
      PARAMETER (MAXCQC = 14)
C                                       # keywords in CQ table
      PARAMETER (NKEYCQ = 2)
C                                       2 x # keywords
      PARAMETER (NKYCQ2 = 2 * NKEYCQ)
C                                       Logical record length in
C                                       single precision words.
      PARAMETER (MXSPCQ = 2 + 15 * MAXIF)
C                                       CQ table revision no.
      PARAMETER (ICQREV = 1)
C                                       Parameters specifying the
C                                       position of variables within a
C                                       table row. Third character
C                                       gives var. type (D=double;
C                                       R=real; I=integer; H=hollerith)
      INTEGER CQIFQD, CQISUB, CQIFFT, CQINCH, CQISAV, CQDFRQ,
     *   CQDCBW, CQHTAP, CQIOVS, CQIZPD, CQIFLT, CQRTAV, CQIBIT,
     *   CQIOVL
C                                       CQIFQD = FQ. ID.
      PARAMETER (CQIFQD = 1)
C                                       CQISUB = Subarray
      PARAMETER (CQISUB = CQIFQD + 1)
C                                       CQIFFT = FFT size
      PARAMETER (CQIFFT = CQISUB + 1)
C                                       CQINCH = No. of channels
      PARAMETER (CQINCH = CQIFFT + 1)
C                                       CQISAV = Spectral avg. factor
      PARAMETER (CQISAV = CQINCH + 1)
C                                       CQDFRQ = Edge frequency
      PARAMETER (CQDFRQ = CQISAV + 1)
C                                       CQDCBW = Channel bandwidth
      PARAMETER (CQDCBW = CQDFRQ + 1)
C                                       CQHTAP = Weighting function
      PARAMETER (CQHTAP = CQDCBW + 1)
C                                       CQIOVS = Oversampling factor
      PARAMETER (CQIOVS = CQHTAP + 1)
C                                       CQIZPD = Zero padding
      PARAMETER (CQIZPD = CQIOVS + 1)
C                                       CQIFLT = Time filter
      PARAMETER (CQIFLT = CQIZPD + 1)
C                                       CQRTAV = Accumulation time
      PARAMETER (CQRTAV = CQIFLT + 1)
C                                       CQIBIT = Quantization
      PARAMETER (CQIBIT = CQRTAV + 1)
C                                       CQIOVL = FFT overlap
      PARAMETER (CQIOVL = CQIBIT + 1)

