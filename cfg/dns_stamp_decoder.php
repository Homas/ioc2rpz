<?php
# to run
#grep "sdns://" public-resolvers.md | php dns_stamp_decoder.php
#https://dnscrypt.info/stamps/
#https://raw.githubusercontent.com/DNScrypt/dnscrypt-resolvers/master/v2/public-resolvers.md

	function base64url_decode($data, $strict = false){
			// Convert Base64URL to Base64 by replacing “-” with “+” and “_” with “/”
			$b64 = strtr($data, '-_', '+/');
			// Decode Base64 string and return the original data
			switch (strlen($b64)%4) {
				case 3: $f="=";
				case 2: $f="==";
				default: $f="";
			};
			return base64_decode($b64.$f, $strict);
	};

	while($f = chop(fgets(STDIN))){
		$sdns=explode("//",$f);
		$str=base64url_decode($sdns[1]);
		if (ord($str[0])==2){ //bin2hex($str[0])
			//  https://github.com/DNSCrypt/dnscrypt-proxy/wiki/stamps
			//  0 = type
			//  1-8 = params
			//  9 = len for IP
			//  10-len9 = IP
			if (ord($str[9])!=0) $IP_org=substr($str,10,ord($str[9])); else $IP_org="#no ip";
			$i=10+ord($str[9]);
			while (ord($str[$i])>127){
//				echo "$i ".ord($str[$i])." \n";
				$i=$i+ord($str[$i])-127;
				if ($i>strlen($str)) break;
			};
			if ($i>strlen($str)) break;
			$i=$i+ord($str[$i])+1;
			$domain_org=substr($str,$i+1,ord($str[$i]));
			preg_match('/^([a-zA-Z0-9\.\-]+)/',$domain_org,$m);
			$domain=$m[0];

			preg_match('/(^[0-9\.]+|^\[[0-9a-fA-F\:]+\]|#no ip)/',$IP_org,$m);
			$IP=trim($m[0],'[]');

			echo "#DoH $f\n";
			if ($domain_org != $domain) echo "#original domain: $domain_org\n";
			if ($IP_org != $IP) echo "#original IP: $IP_org\n";
			echo "$domain\n$IP\n\n";
		};
	}		
		
?>