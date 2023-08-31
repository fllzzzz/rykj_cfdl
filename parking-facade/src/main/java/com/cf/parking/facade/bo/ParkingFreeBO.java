package com.cf.parking.facade.bo;

import lombok.Data;

import java.util.Date;

@Data
public class ParkingFreeBO {
    private Date statDate;

    private Integer parkFlag;

    private Integer freeNum;

    public Date getStatDate() {
        return statDate;
    }

    public void setStatDate(Date statDate) {
        this.statDate = statDate;
    }

    public Integer getParkFlag() {
        return parkFlag;
    }

    public void setParkFlag(Integer parkFlag) {
        this.parkFlag = parkFlag;
    }

    public Integer getFreeNum() {
        return freeNum;
    }

    public void setFreeNum(Integer freeNum) {
        this.freeNum = freeNum;
    }
}
