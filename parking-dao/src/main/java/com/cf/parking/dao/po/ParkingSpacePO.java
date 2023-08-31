package com.cf.parking.dao.po;

import java.util.Date;

public class ParkingSpacePO {
    private Integer id;

    private String spaceNo;

    private String plateNo;

    private Integer parkFlag;

    private String personName;

    private String personId;

    private Integer state;

    private String parkTime;

    private Date inTime;

    private Date createTm;

    private Date updateTm;

    public Integer getId() {
        return id;
    }

    public void setId(Integer id) {
        this.id = id;
    }

    public String getSpaceNo() {
        return spaceNo;
    }

    public void setSpaceNo(String spaceNo) {
        this.spaceNo = spaceNo;
    }

    public String getPlateNo() {
        return plateNo;
    }

    public void setPlateNo(String plateNo) {
        this.plateNo = plateNo;
    }

    public Integer getParkFlag() {
        return parkFlag;
    }

    public void setParkFlag(Integer parkFlag) {
        this.parkFlag = parkFlag;
    }

    public String getPersonName() {
        return personName;
    }

    public void setPersonName(String personName) {
        this.personName = personName;
    }

    public String getPersonId() {
        return personId;
    }

    public void setPersonId(String personId) {
        this.personId = personId;
    }

    public Integer getState() {
        return state;
    }

    public void setState(Integer state) {
        this.state = state;
    }

    public String getParkTime() {
        return parkTime;
    }

    public void setParkTime(String parkTime) {
        this.parkTime = parkTime;
    }

    public Date getInTime() {
        return inTime;
    }

    public void setInTime(Date inTime) {
        this.inTime = inTime;
    }

    public Date getCreateTm() {
        return createTm;
    }

    public void setCreateTm(Date createTm) {
        this.createTm = createTm;
    }

    public Date getUpdateTm() {
        return updateTm;
    }

    public void setUpdateTm(Date updateTm) {
        this.updateTm = updateTm;
    }
}