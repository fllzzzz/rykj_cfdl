package com.cf.parking.dao.po;

import java.util.Date;

public class AdminOptLog {
    private Long adminOptLogId;

    private Long adminUserId;

    private String adminName;

    private String optTitle;

    private String params;

    private String response;

    private String ip;

    private Integer executeTime;

    private Date createAt;

    private Date updateAt;

    public Long getAdminOptLogId() {
        return adminOptLogId;
    }

    public void setAdminOptLogId(Long adminOptLogId) {
        this.adminOptLogId = adminOptLogId;
    }

    public Long getAdminUserId() {
        return adminUserId;
    }

    public void setAdminUserId(Long adminUserId) {
        this.adminUserId = adminUserId;
    }

    public String getAdminName() {
        return adminName;
    }

    public void setAdminName(String adminName) {
        this.adminName = adminName;
    }

    public String getOptTitle() {
        return optTitle;
    }

    public void setOptTitle(String optTitle) {
        this.optTitle = optTitle;
    }

    public String getParams() {
        return params;
    }

    public void setParams(String params) {
        this.params = params;
    }

    public String getResponse() {
        return response;
    }

    public void setResponse(String response) {
        this.response = response;
    }

    public String getIp() {
        return ip;
    }

    public void setIp(String ip) {
        this.ip = ip;
    }

    public Integer getExecuteTime() {
        return executeTime;
    }

    public void setExecuteTime(Integer executeTime) {
        this.executeTime = executeTime;
    }

    public Date getCreateAt() {
        return createAt;
    }

    public void setCreateAt(Date createAt) {
        this.createAt = createAt;
    }

    public Date getUpdateAt() {
        return updateAt;
    }

    public void setUpdateAt(Date updateAt) {
        this.updateAt = updateAt;
    }
}