package com.cf.parking.facade.dto;

import lombok.Data;

import java.io.Serializable;

@Data
public final class HikvisionResult<T> implements Serializable {
    private static final long serialVersionUID = 8168900316765357276L;
    private String code;
    private String msg;
    private T data;
}
