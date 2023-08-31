package com.cf.parking.facade.bo;

import lombok.Data;

import java.util.Date;

@Data
public class ParkingSpaceBO {

    private String spaceNo;

    private String plateNo;

    private Integer parkFlag;

    private String personName;

    private String personId;

    private Integer state;

    private String parkTime;

    private Date inTime;
}