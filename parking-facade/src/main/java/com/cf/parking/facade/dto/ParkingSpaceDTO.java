package com.cf.parking.facade.dto;

import com.cf.support.result.PageRequest;
import lombok.Data;

@Data
public class ParkingSpaceDTO extends PageRequest {

    private String spaceNo;

    private String plateNo;

    private Integer state;
    private Integer parkFlag;


}