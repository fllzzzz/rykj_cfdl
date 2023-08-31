package com.cf.parking.api.request;

import com.cf.support.result.PageRequest;

public class HikvisionReq extends PageRequest {
    private String apiPath;


    public String getApiPath() {
        return apiPath;
    }

    public void setApiPath(String apiPath) {
        this.apiPath = apiPath;
    }

}
