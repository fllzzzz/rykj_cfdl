package com.cf.parking.api.request;

import com.cf.support.result.PageRequest;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import javax.validation.constraints.Pattern;

/**
 * @author lpy
 * @date 2023-03-27 09:43:43
 * @description 黑名单
 */
@Data
@Accessors(chain = true)
public class BlackListPageReq extends PageRequest {


    @ApiModelProperty(value = "工号")
    @Pattern(regexp = "^.{0,20}$", message = "工号长度不可超过20")
    private String jobNumber;

    @ApiModelProperty(value = "车牌号")
    @Pattern(regexp = "^.{0,20}$", message = "车牌号长度不可超过20")
    private String plateNo;

}
