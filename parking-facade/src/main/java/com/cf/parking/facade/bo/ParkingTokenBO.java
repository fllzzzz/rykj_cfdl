package com.cf.parking.facade.bo;

import lombok.Data;

@Data
public class ParkingTokenBO {

	private String access_token;
	
	private String token_type;
	
	private int expires_in;
}
