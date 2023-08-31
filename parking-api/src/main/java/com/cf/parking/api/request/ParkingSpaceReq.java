package com.cf.parking.api.request;

import com.cf.support.result.PageRequest;
import lombok.Data;

@Data
public class ParkingSpaceReq extends PageRequest {

    private String spaceNo;

    private String plateNo;

    private Integer state;
    private Integer parkFlag;

}