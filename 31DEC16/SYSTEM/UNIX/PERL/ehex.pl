# -*-Perl-*-
#-----------------------------------------------------------------------
#;  Copyright (C) 1995, 2001
#;  Associated Universities, Inc. Washington DC, USA.
#;
#;  This program is free software; you can redistribute it and/or
#;  modify it under the terms of the GNU General Public License as
#;  published by the Free Software Foundation; either version 2 of
#;  the License, or (at your option) any later version.
#;
#;  This program is distributed in the hope that it will be useful,
#;  but WITHOUT ANY WARRANTY; without even the implied warranty of
#;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#;  GNU General Public License for more details.
#;
#;  You should have received a copy of the GNU General Public
#;  License along with this program; if not, write to the Free
#;  Software Foundation, Inc., 675 Massachusetts Ave, Cambridge,
#;  MA 02139, USA.
#;
#;  Correspondence concerning AIPS should be addressed as follows:
#;         Internet email: aipsmail@nrao.edu.
#;         Postal address: AIPS Project Office
#;                         National Radio Astronomy Observatory
#;                         520 Edgemont Road
#;                         Charlottesville, VA 22903-2475 USA
#-----------------------------------------------------------------------

package Ehex;

$map = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";

# Call as: &dec_to_ehex (number, padding)
# where "padding" is the (optional) number of char's that the result
# should be padded to.
sub main'dec_to_ehex #'
{
    local ($outstr, $n, $r);
    ($n = shift) =~ /^\d*$/ || return -1;

    while ($n > 0) {
	$r = $n % 36;
	$n = int ($n / 36);
	substr ($outstr, 0, 0) = substr ($map, $r, 1);
    }

    $outstr || ($outstr = 0);
    substr ($outstr, 0, 0) = 0 x (shift (@_) - length $outstr);
    return $outstr;
}

# Call as: &ehex_to_dec (ehex_number)
sub main'ehex_to_dec #'
{
    local ($outval, $n, $i);
    ($n = reverse shift) =~ /^[A-Za-z0-9]*$/ || return -1;
    $n =~ tr/a-z/A-Z/;

    for ($i = 0; $i < length $n; $i++) {
	$outval += index ($map, substr ($n, $i, 1)) * 36 ** $i;
    }

    return $outval;
}

1;
