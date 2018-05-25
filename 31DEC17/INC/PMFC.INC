C                                                          Include PMFC.
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1998, 2010
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
C                                       Parameters to define MF tables
      INTEGER   NUMCOL
      PARAMETER (NUMCOL=41)
C
      INTEGER   COLPLN, COLPEK, COLFLX, COLDLX, COLDLY, COLMJX, COLMNX,
     *   COLPAN, COLQFL, COLUFL, COLVFL, COEPEK, COEFLX, COEDLX, COEDLY,
     *   COEMJX, COEMNX, COEPAN, COEQFL, COEUFL, COEVFL, COLTYP
      INTEGER   COLD0J, COLD0N, COLD0P, COLDMJ, COLDMN, COLDMP, COLDPJ,
     *   COLDPN, COLDPP, CORRMS, CORPEK, CORFLX, COPCEX, COPCEY, COPMAJ,
     *   COPMIN, COPPAN, COBMFA, CODLFA
      PARAMETER (COLPLN = 1, COLPEK = 2, COLFLX = 3, COLDLX = 4,
     *   COLDLY = 5, COLMJX = 6, COLMNX = 7, COLPAN = 8,
     *   COLQFL = 9, COLUFL =10, COLVFL =11)
      PARAMETER (COEPEK =12, COEFLX =13, COEDLX =14, COEDLY =15,
     *   COEMJX =16, COEMNX =17, COEPAN =18, COEQFL =19,
     *   COEUFL =20, COEVFL =21, COLTYP =22)
      PARAMETER (COLD0J =23, COLD0N =24, COLD0P =25, COLDMJ =26,
     *   COLDMN =27, COLDMP =28, COLDPJ =29, COLDPN =30,
     *   COLDPP =31, CORRMS =32, CORPEK =33, CORFLX =34,
     *   COPCEX =35, COPCEY =36, COPMAJ =37, COPMIN =38,
     *   COPPAN =39, COBMFA =40, CODLFA =41)
C                                                          End PMFC.
